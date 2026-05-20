# West Virginia Project
# Shiny App
# Tiffany Wu
# 4/17/26

# setwd("//soe-shared.m.storage.umich.edu/soe-shared/Boston PK3/step3/Tiffany/West Virginia Project")

# Read in libraries
library(shiny)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(DT)
library(sf)
library(readr)
library(readxl)
library(scales)
library(htmltools)

select <- dplyr::select

options(shiny.launch.browser = TRUE)

# =========================
# 0. STATIC ASSETS
# =========================
# Make sure Shiny can serve files under www/ no matter how the app is launched
# (runApp(), Run App button, sourced, working dir set elsewhere, etc.).
# We resolve the absolute path of the www/ folder and register two prefixes:
#   /static/    -> www/                    (e.g. /static/wv_flag.jpg)
#   /map_files/ -> www/map_files/          (preserves the iframe's relative
#                                           links into enrollment_change_map_files/)
local({
  # Try to find the directory this script lives in.
  app_dir <- tryCatch({
    f <- sys.frame(1)$ofile
    if (is.null(f)) stop("not sourced")
    dirname(normalizePath(f, mustWork = FALSE))
  }, error = function(e) {
    # Fallback to the current working directory.
    getwd()
  })

  www_dir <- file.path(app_dir, "www")
  if (!dir.exists(www_dir)) www_dir <- normalizePath("www", mustWork = FALSE)

  if (dir.exists(www_dir)) {
    shiny::addResourcePath("static", www_dir)
    map_dir <- file.path(www_dir, "map_files")
    if (dir.exists(map_dir)) {
      shiny::addResourcePath("map_files", map_dir)
    }
    message("Registered Shiny resource paths from: ", www_dir)
  } else {
    warning("www/ folder not found near app.R — images and the map iframe may 404.")
  }
})

# =========================
# 1. READ RAW DATA
# =========================
school_existence          <- read_excel("school_existence.xlsx")
school_composition        <- read_excel("school_composition.xlsx")
district_enrollment_panel <- read.csv("district_enrollment_panel.csv")
census_raw                <- read_csv("wv_2011_2024.csv")

# Preserve the original Type from the .xlsx files BEFORE the pipeline rewrites
# them, so we can compare three sources side-by-side later:
#   Type_raw      = original Type column from school_existence / school_composition
#   Type          = result of the existing pipeline (school_year_enrollment_types.csv
#                   + standardize_type + elem_middle_schools override)
#   Type_inferred = final_type from inferred_school_types_final.csv
school_existence$Type_raw   <- school_existence$Type
school_composition$Type_raw <- school_composition$Type

# =========================
# 1.0 FILTER TO THE 55 WV COUNTY DISTRICTS
# =========================
# Only keep rows whose district matches one of these 55 counties (case-insensitive,
# after squishing whitespace). Each raw input source has its own native
# district column — we filter using whatever that column is called:
#   school_existence          -> "District"
#   school_composition        -> "District"
#   district_enrollment_panel -> "District"
#   wv_2011_2024.csv (census) -> "county"   (no "District" column in this file)
# Anything outside this list (state-wide programs, virtual academies, blank rows,
# typos) is dropped here so it never enters the rest of the pipeline.
ALLOWED_DISTRICTS <- toupper(c(
  "Barbour", "Berkeley", "Boone", "Braxton", "Brooke", "Cabell", "Calhoun", "Clay",
  "Doddridge", "Fayette", "Gilmer", "Grant", "Greenbrier", "Hampshire", "Hancock",
  "Hardy", "Harrison", "Jackson", "Jefferson", "Kanawha", "Lewis", "Lincoln", "Logan",
  "Marion", "Marshall", "Mason", "McDowell", "Mercer", "Mineral", "Mingo", "Monongalia",
  "Monroe", "Morgan", "Nicholas", "Ohio", "Pendleton", "Pleasants", "Pocahontas",
  "Preston", "Putnam", "Raleigh", "Randolph", "Ritchie", "Roane", "Summers", "Taylor",
  "Tucker", "Tyler", "Upshur", "Wayne", "Webster", "Wetzel", "Wirt", "Wood", "Wyoming"
))

keep_district <- function(df, col) {
  key    <- toupper(stringr::str_squish(as.character(df[[col]])))
  before <- nrow(df)
  out    <- df[key %in% ALLOWED_DISTRICTS, , drop = FALSE]
  message(sprintf("District filter on %s$%s: kept %d of %d rows",
                  deparse(substitute(df)), col, nrow(out), before))
  out
}

school_existence          <- keep_district(school_existence,          "District")
school_composition        <- keep_district(school_composition,        "District")
district_enrollment_panel <- keep_district(district_enrollment_panel, "District")
census_raw                <- keep_district(census_raw,                "county")

# Build the census frame the rest of the app expects (renames county -> District
# for downstream joins, and year -> Year).
census <- census_raw %>%
  mutate(District = county,
         Year     = year)

# Replace Type in school_composition with version without NAs
school_types <- read.csv("school_year_enrollment_types.csv")

school_composition <- school_composition %>%
  mutate(Schl = as.character(Schl)) %>%
  select(-Type) %>%
  left_join(
    school_types %>%
      mutate(school_code = as.character(school_code)) %>%
      distinct(school_code, .keep_all = TRUE) %>%
      select(school_code, school_type) ,
    by = c("Schl" = "school_code")
  ) %>%
  rename(Type = school_type)  %>%
  mutate(
    Type = case_when(
      is.na(Type) & str_detect(str_to_lower(School), "elementary") ~ "Elementary",
      is.na(Type) & str_detect(str_to_lower(School), "middle") ~ "Middle",
      is.na(Type) & str_detect(str_to_lower(School), "primary") ~ "Primary",
      is.na(Type) & str_detect(str_to_lower(School), "high school") ~ "High School",
      TRUE ~ Type
    )
  )

school_types_clean <- school_types %>%
  mutate(
    local_school_id = as.character(local_school_id),
    local_school_id = stringr::str_remove(local_school_id, "^0+")
  ) %>%
  select(local_school_id, school_type) %>%
  distinct(local_school_id, .keep_all = TRUE)

school_existence <- school_existence %>%
  mutate(
    Schl_ID = as.character(Schl_ID),
    Schl_ID_join = stringr::str_remove(Schl_ID, "^0+")
  ) %>%
  select(-Type) %>%
  left_join(
    school_types_clean,
    by = c("Schl_ID_join" = "local_school_id")
  ) %>%
  rename(Type = school_type) %>%
  select(-Schl_ID_join)

# Change type in school_status
# Replace Type in school_status
school_existence <- school_existence %>%
  mutate(
    Schl_ID = as.character(Schl_ID),
    Schl_ID_join = stringr::str_remove(Schl_ID, "^0+")
  ) %>%
  select(-Type) %>%
  left_join(
    school_types_clean,
    by = c("Schl_ID_join" = "local_school_id")
  ) %>%
  rename(Type = school_type) %>%
  select(-Schl_ID_join)  %>%
  mutate(
    Type = case_when(
      is.na(Type) & str_detect(str_to_lower(School), "elementary") ~ "Elementary",
      is.na(Type) & str_detect(str_to_lower(School), "middle") ~ "Middle",
      is.na(Type) & str_detect(str_to_lower(School), "primary") ~ "Primary",
      is.na(Type) & str_detect(str_to_lower(School), "high school") ~ "High School",
      TRUE ~ Type
    )
  )

# =========================
# 1a. LOAD INFERRED SCHOOL TYPES (parallel column for comparison)
# =========================
# This loads final_type from inferred_school_types_final.csv and attaches it
# as Type_inferred alongside the existing Type column, so the two assignments
# can be compared (see type_comparison below). The existing Type pipeline
# (school_types_clean + standardize_type + elem_middle_schools) is unchanged.

# Normalize a school name so it can be matched across data sources:
#   uppercase, drop punctuation, collapse whitespace, drop trailing " SCHOOL".
normalize_school_name <- function(x) {
  x <- stringr::str_to_upper(x)
  x <- stringr::str_replace_all(x, "[^A-Z0-9 ]", " ")
  x <- stringr::str_squish(x)
  x <- stringr::str_remove(x, "\\s+SCHOOL$")
  x
}

# Map final_type (lowercase tokens in the CSV) to the human-readable labels
# the rest of the app uses for Type.
final_type_to_label <- function(x) {
  dplyr::case_when(
    x == "elementary"        ~ "Elementary",
    x == "middle"            ~ "Middle",
    x == "high"              ~ "High School",
    x == "elementary_middle" ~ "Elementary + Middle",
    x == "alternative"       ~ "Alternative",
    TRUE                     ~ NA_character_
  )
}

inferred_school_types <- read_csv("inferred_school_types_final.csv",
                                  show_col_types = FALSE) %>%
  mutate(
    school_key = normalize_school_name(School),
    Type_inferred = final_type_to_label(final_type)
  ) %>%
  filter(!is.na(Type_inferred)) %>%
  distinct(school_key, .keep_all = TRUE) %>%
  select(school_key, Type_inferred)

# Attach Type_inferred to school_composition (parallel to Type)
school_composition <- school_composition %>%
  mutate(school_key = normalize_school_name(School)) %>%
  left_join(inferred_school_types, by = "school_key") %>%
  select(-school_key)

# Attach Type_inferred to school_existence (parallel to Type)
school_existence <- school_existence %>%
  mutate(school_key = normalize_school_name(School)) %>%
  left_join(inferred_school_types, by = "school_key") %>%
  select(-school_key)
# =========================
# 2. HELPER FUNCTIONS
# =========================
clean_school_status <- function(df) {
  if (!"Type_inferred" %in% names(df)) df$Type_inferred <- NA_character_
  if (!"Type_raw"      %in% names(df)) df$Type_raw      <- NA_character_

  df %>%
    mutate(across(where(is.character), stringr::str_squish)) %>%
    mutate(
      Schl_ID = as.character(Schl_ID),
      District = str_remove(District, " County Schools$| Schools$| School District$"),
      District = str_squish(District),
      School = str_squish(School),
      Type = str_squish(Type),
      Type_raw = str_squish(Type_raw),
      Type_inferred = str_squish(Type_inferred),
      start_year = as.integer(start_year),
      end_year = as.integer(end_year),
      flipped = as.logical(flipped),
      closed = as.logical(closed)
    )
}

clean_district_panel <- function(df) {
  df %>%
    mutate(across(where(is.character), stringr::str_squish)) %>%
    rename(enrollment = Enrollment) %>%
    mutate(
      Year = as.integer(Year),
      District = str_remove(District, " County Schools$| Schools$| School District$"),
      District = str_squish(District)
    )
}

build_district_panel <- function(district_enr, school_status) {
  district_cons_cross <- school_status %>%
    filter(closed, !is.na(end_year)) %>%
    group_by(District) %>%
    summarise(
      first_cons_year = min(end_year, na.rm = TRUE),
      last_cons_year = max(end_year, na.rm = TRUE),
      n_closed = n(),
      .groups = "drop"
    )
  
  district_enr %>%
    left_join(district_cons_cross, by = "District") %>%
    mutate(
      event_time_cons_year = Year - first_cons_year,
      event_time_pre_year = Year - (first_cons_year - 1)
    )
}

build_map_data <- function(district_panel, district_shapes) {
  district_summary <- district_panel %>%
    group_by(District) %>%
    arrange(Year, .by_group = TRUE) %>%
    summarise(
      enrollment_first = enrollment[Year == min(Year, na.rm = TRUE)][1],
      enrollment_last = enrollment[Year == max(Year, na.rm = TRUE)][1],
      first_cons_year = first(first_cons_year),
      last_cons_year = first(last_cons_year),
      n_closed = first(n_closed),
      .groups = "drop"
    ) %>%
    mutate(
      total_change = enrollment_last - enrollment_first,
      pct_change = 100 * total_change / enrollment_first,
      n_closed = tidyr::replace_na(n_closed, 0)
    )
  
  district_shapes %>%
    mutate(
      District = str_remove(NAME, " County School District$| County Schools$| Schools$| School District$"),
      District = str_squish(District)
    ) %>%
    left_join(district_summary, by = "District")
}

clean_school_composition <- function(df) {
  names(df) <- names(df) %>%
    stringr::str_replace_all("\\r\\n", " ") %>%
    stringr::str_squish()

  if (!"Type_inferred" %in% names(df)) df$Type_inferred <- NA_character_
  if (!"Type_raw"      %in% names(df)) df$Type_raw      <- NA_character_

  df %>%
    mutate(across(where(is.character), stringr::str_squish)) %>%
    mutate(
      Year = as.integer(Year),
      Schl = as.character(Schl),
      District = str_remove(District, " County Schools$| Schools$| School District$"),
      District = str_squish(District),
      School = str_squish(School),
      Type = str_squish(Type),
      Type_raw = str_squish(Type_raw),
      Type_inferred = str_squish(Type_inferred),
      headcount = readr::parse_number(`Total Headcount`)
    ) %>%
    select(Year, District, Schl, School, Type_raw, Type, Type_inferred, headcount)
}

build_pre_post_plot_data <- function(district_panel_df, offset = 0) {
  district_panel_df %>%
    filter(!is.na(first_cons_year)) %>%
    mutate(
      threshold = first_cons_year + offset,
      period = ifelse(Year <= threshold, "pre", "post"),
      facet_label = paste0(District, " (c=", first_cons_year, ")")
    )
}

build_closure_year_map_data <- function(school_status, district_shapes, year_min = NULL, year_max = NULL) {
  
  shape_df <- district_shapes %>%
    mutate(
      District = stringr::str_remove(NAME, " County School District$| County Schools$| Schools$| School District$"),
      District = stringr::str_squish(District)
    )
  
  closure_counts <- school_status %>%
    filter(closed, !is.na(end_year)) %>%
    group_by(District, Year = end_year) %>%
    summarise(
      n_closed_that_year = n(),
      .groups = "drop"
    )
  
  years_all <- sort(unique(school_status$end_year[!is.na(school_status$end_year)]))
  
  if (!is.null(year_min)) years_all <- years_all[years_all >= year_min]
  if (!is.null(year_max)) years_all <- years_all[years_all <= year_max]
  
  district_year_grid <- tidyr::expand_grid(
    District = unique(shape_df$District),
    Year = years_all
  )
  
  closure_counts_full <- district_year_grid %>%
    left_join(closure_counts, by = c("District", "Year")) %>%
    mutate(
      n_closed_that_year = tidyr::replace_na(n_closed_that_year, 0)
    )
  
  shape_df %>%
    left_join(closure_counts_full, by = "District")
}

build_cumulative_closure_map_data <- function(school_status, district_shapes, year_min = NULL, year_max = NULL) {
  
  shape_df <- district_shapes %>%
    mutate(
      District = stringr::str_remove(NAME, " County School District$| County Schools$| Schools$| School District$"),
      District = stringr::str_squish(District)
    )
  
  years_all <- sort(unique(school_status$end_year[!is.na(school_status$end_year)]))
  
  if (!is.null(year_min)) years_all <- years_all[years_all >= year_min]
  if (!is.null(year_max)) years_all <- years_all[years_all <= year_max]
  
  district_year_grid <- tidyr::expand_grid(
    District = unique(shape_df$District),
    Year = years_all
  )
  
  cumulative_counts <- district_year_grid %>%
    rowwise() %>%
    mutate(
      cumulative_closed = sum(
        school_status$District == District &
          school_status$closed == TRUE &
          !is.na(school_status$end_year) &
          school_status$end_year <= Year,
        na.rm = TRUE
      )
    ) %>%
    ungroup()
  
  shape_df %>%
    left_join(cumulative_counts, by = "District")
}

build_type_specific_panel <- function(school_comp_clean, school_status, selected_types) {
  
  if (is.null(selected_types) || "All" %in% selected_types) {
    
    type_enr <- school_comp_clean %>%
      group_by(Year, District) %>%
      summarise(
        enrollment = sum(headcount, na.rm = TRUE),
        .groups = "drop"
      )
    
    type_status <- school_status
    
  } else {
    
    type_enr <- school_comp_clean %>%
      filter(Type %in% selected_types) %>%
      group_by(Year, District) %>%
      summarise(
        enrollment = sum(headcount, na.rm = TRUE),
        .groups = "drop"
      )
    
    type_status <- school_status %>%
      filter(Type %in% selected_types)
  }
  
  build_district_panel(type_enr, type_status)
}

build_selected_type_panel <- function(school_comp_clean, school_status, selected_types) {
  
  if (is.null(selected_types) || "All" %in% selected_types) {
    comp_df <- school_comp_clean
    status_df <- school_status
  } else {
    comp_df <- school_comp_clean %>%
      filter(Type %in% selected_types)
    
    status_df <- school_status %>%
      filter(Type %in% selected_types)
  }
  
  type_enr <- comp_df %>%
    group_by(Year, District) %>%
    summarise(
      enrollment = sum(headcount, na.rm = TRUE),
      .groups = "drop"
    )
  
  build_district_panel(type_enr, status_df)
}
# =========================
# 3. BUILD CLEAN OBJECTS
# =========================


# K-8s?
# Identify schools serving both elementary and middle grades
elem_middle_schools <- school_composition %>%
  mutate(
    grade3 = suppressWarnings(readr::parse_number(`Grade 3`)),
    grade8 = suppressWarnings(readr::parse_number(`Grade 8`)),
    Schl = as.character(Schl),
    District = stringr::str_squish(District)
  ) %>%
  filter(
    !is.na(grade3), grade3 > 0,
    !is.na(grade8), grade8 > 0
  ) %>%
  distinct(Schl, District)


school_status <- clean_school_status(school_existence)
district_enr <- clean_district_panel(district_enrollment_panel)
school_comp_clean <- clean_school_composition(school_composition)
#district_panel <- build_district_panel(district_enr, school_status)


standardize_type <- function(type, school_name) {
  type_lower <- str_to_lower(type)
  name_lower <- str_to_lower(school_name)
  
  case_when(
    str_detect(type_lower, "alternative") ~ "Alternative",
    str_detect(type_lower, "elementary|primary") ~ "Elementary",
    str_detect(type_lower, "middle") ~ "Middle",
    str_detect(type_lower, "high|secondary") ~ "High School",
    
    str_detect(name_lower, "alternative") ~ "Alternative",
    str_detect(name_lower, "elementary|primary") ~ "Elementary",
    str_detect(name_lower, "middle") ~ "Middle",
    str_detect(name_lower, "high") ~ "High School",
    
    TRUE ~ type
  )
}

school_comp_clean <- school_comp_clean %>%
  mutate(Type = standardize_type(Type, School))


# Recode school_comp_clean
school_comp_clean <- school_comp_clean %>%
  mutate(
    Schl = as.character(Schl),
    District = stringr::str_squish(District)
  ) %>%
  left_join(
    elem_middle_schools %>%
      mutate(elem_middle = TRUE),
    by = c("Schl", "District")
  ) %>%
  mutate(
    Type = case_when(
      elem_middle == TRUE ~ "Elementary + Middle",
      TRUE ~ Type
    )
  ) %>%
  select(-elem_middle)

school_status <- school_status %>%
  mutate(Type = standardize_type(Type, School))

# Recode school_status
school_status <- school_status %>%
  mutate(
    Schl_ID = as.character(Schl_ID),
    District = stringr::str_squish(District),
    
    # keep only last 3 characters
    school_match_id = stringr::str_sub(Schl_ID, -3, -1)
  ) %>%
  left_join(
    elem_middle_schools %>%
      mutate(
        Schl = as.character(Schl),
        District = stringr::str_squish(District),
        
        # keep only last 3 characters
        school_match_id = stringr::str_sub(Schl, -3, -1),
        
        elem_middle = TRUE
      ) %>%
      select(District, school_match_id, elem_middle),
    
    by = c("District", "school_match_id")
  ) %>%
  mutate(
    Type = case_when(
      elem_middle == TRUE ~ "Elementary + Middle",
      TRUE ~ Type
    )
  ) %>%
  select(-elem_middle, -school_match_id)

# =========================
# 3a. COMPARE THREE TYPE SOURCES SIDE-BY-SIDE
# =========================
# Type_raw      = original Type from school_existence.xlsx / school_composition.xlsx
# Type          = result of the existing pipeline
#                 (school_year_enrollment_types.csv + standardize_type + elem_middle override)
# Type_inferred = final_type from inferred_school_types_final.csv

classify_match <- function(a, b, c) {
  # all-three agreement, any pairwise agreement, or all three different
  has_a <- !is.na(a); has_b <- !is.na(b); has_c <- !is.na(c)
  dplyr::case_when(
    has_a & has_b & has_c & a == b & b == c ~ "all_three_agree",
    has_a & has_b & has_c & a != b & a != c & b != c ~ "all_three_differ",
    !has_c                              ~ "no_inferred_match",
    !has_b                              ~ "no_pipeline_type",
    !has_a                              ~ "no_raw_type",
    a == b & b != c                     ~ "raw=pipeline; inferred differs",
    a == c & b != c                     ~ "raw=inferred; pipeline differs",
    b == c & a != b                     ~ "pipeline=inferred; raw differs",
    TRUE                                ~ "disagree"
  )
}

# school_status: one row per school in the existence/status file
type_comparison_status <- school_status %>%
  mutate(
    Type_raw      = str_squish(Type_raw),
    Type          = str_squish(Type),
    Type_inferred = str_squish(Type_inferred),
    type_match    = classify_match(Type_raw, Type, Type_inferred)
  ) %>%
  select(District, Schl_ID, School,
         Type_raw, Type_pipeline = Type, Type_inferred,
         type_match)

# school_comp_clean: collapse the year panel to one row per (Schl, School)
type_comparison_comp <- school_comp_clean %>%
  mutate(
    Type_raw      = str_squish(Type_raw),
    Type          = str_squish(Type),
    Type_inferred = str_squish(Type_inferred)
  ) %>%
  distinct(District, Schl, School, Type_raw, Type, Type_inferred) %>%
  mutate(
    type_match = classify_match(Type_raw, Type, Type_inferred)
  ) %>%
  rename(Type_pipeline = Type)

# Quick console summary
message("Three-source type match — school_status:")
print(table(type_comparison_status$type_match, useNA = "ifany"))
message("Three-source type match — school_comp_clean:")
print(table(type_comparison_comp$type_match, useNA = "ifany"))

# -------------------------------------------------------------
# Write the comparison to an Excel workbook for manual review.
# Sheets:
#   school_status_all      — all schools in school_status with the 3 type cols
#   school_comp_all        — distinct (Schl, School) rows from school_comp_clean
#   school_status_disagree — only rows where the 3 sources don't unanimously agree
#   school_comp_disagree   — same, for school_comp_clean
#   summary_status / summary_comp — counts by type_match
#
# NOTE: written with explicit dplyr:: calls (no %>%) so this block still runs
# even if magrittr/dplyr aren't attached in the current session.
# -------------------------------------------------------------
if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx", repos = "https://cloud.r-project.org")
}
# Defensive: make sure the pipe is available anywhere it's still used.
suppressMessages(suppressWarnings(library(magrittr)))

type_check_path <- "type_source_comparison.xlsx"

school_status_all      <- dplyr::arrange(type_comparison_status, District, School)
school_comp_all        <- dplyr::arrange(type_comparison_comp,   District, School)

school_status_disagree <- dplyr::arrange(
  dplyr::filter(type_comparison_status, type_match != "all_three_agree"),
  type_match, District, School
)
school_comp_disagree   <- dplyr::arrange(
  dplyr::filter(type_comparison_comp,   type_match != "all_three_agree"),
  type_match, District, School
)

summary_status <- dplyr::arrange(
  dplyr::count(type_comparison_status, type_match, name = "n"),
  dplyr::desc(n)
)
summary_comp   <- dplyr::arrange(
  dplyr::count(type_comparison_comp,   type_match, name = "n"),
  dplyr::desc(n)
)

openxlsx::write.xlsx(
  list(
    school_status_all      = school_status_all,
    school_comp_all        = school_comp_all,
    school_status_disagree = school_status_disagree,
    school_comp_disagree   = school_comp_disagree,
    summary_status         = summary_status,
    summary_comp           = summary_comp
  ),
  file = type_check_path,
  overwrite = TRUE
)

message("Wrote three-source type comparison to: ", normalizePath(type_check_path))

district_panel <- build_district_panel(district_enr, school_status) %>%
  left_join(census, by = c("District", "Year"))

district_shapes <- sf::st_read("district_shapes/tl_2021_54_unsd.shp", quiet = TRUE)
district_shapes <- sf::st_transform(district_shapes, 4326)

school_type_choices <- sort(unique(c(
  as.character(school_status$Type),
  as.character(school_comp_clean$Type)
)))

school_type_choices <- school_type_choices[!is.na(school_type_choices)]
school_type_choices <- sort(unique(c(
  as.character(school_status$Type),
  as.character(school_comp_clean$Type)
)))


#district_map_sf <- build_map_data(district_panel, district_shapes) %>%
#  mutate(
#    label_html = sprintf(
#      "<strong>%s</strong><br>Percent change: %.1f%%<br>Change in # Students: %s<br>Closed schools: %s",
#      District,
#      pct_change,
#      scales::comma(total_change),
#      n_closed
#    )
#  )



closure_map_df <- build_closure_year_map_data(
  school_status = school_status,
  district_shapes = district_shapes,
  year_min = min(district_panel$Year, na.rm = TRUE),
  year_max = max(district_panel$Year, na.rm = TRUE)
)



#closure_map_df$Year <- factor(closure_map_df$Year)

#ggplot(closure_map_df) +
#  geom_sf(aes(fill = n_closed_that_year), color = "gray50", linewidth = 0.15) +
#  facet_wrap(~Year, ncol = 5) +
#  scale_fill_gradient(
#    low = "#f7fbff",
#    high = "red3",
#    na.value = "#d9d9d9"
#  ) +
#  labs(
#    title = "Number of Schools Closed Within Each District by Year",
#    fill = "# Closed"
#  ) +
#  theme_void() +
#  theme(
###    strip.text = element_text(size = 9),
##    plot.title = element_text(size = 12, hjust = 0.5),
#    legend.position = "bottom"
#  )
# =========================
# 4. UI
# =========================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
    
    /* Inactive tabs */
    .nav-tabs > li > a {
      background-color: #eaf6ff;
      color: #003366;
      border: 1px solid #b3e0ff;
    }

    /* Active tab */
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #87ceeb;
      color: #003366;
      border: 1px solid #87ceeb;
      font-weight: 600;
    }

    /* Hover */
    .nav-tabs > li > a:hover {
      background-color: #cceeff;
    }

  "))
  ),
  
 # titlePanel("West Virginia School Closures & Enrollment Dashboard"),
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; align-items: center; gap: 15px;",
        
        img(
          src = "static/wv_flag.jpg",
          height = "50px",
          alt = "West Virginia state flag"
        ),
        
        h2("West Virginia School Consolidations",
           style = "margin: 0; font-weight: 600;")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,

      # DESCRIPTION
      div(
        style = "background-color: #f7fbff; padding: 10px; border-radius: 6px; margin-bottom: 15px;",
        
        h4("About this Dashboard", style = "margin-top: 0;"),
        
        p(
          "This dashboard explores school closures and enrollment trends across West Virginia districts from 2011 to 2026. 
      It allows users to examine enrollment changes before and after closures, identify closure patterns, 
      and visualize both annual and cumulative closures across districts.",
          style = "font-size: 13px; margin-bottom: 5px;"
        ),
        
        p(
          "Use the filters below to explore specific districts, school grade levels, and time periods.",
          style = "font-size: 12px; color: #555; margin-top: 10px;"
        )
      ),
      
      hr(),
      # Filters
      selectInput(
        "district_filter",
        "District",
        choices = c("All", sort(unique(school_status$District))),
        selected = "All",
        multiple = TRUE
      ),
      
      selectInput(
        "school_type_filter",
        "School Grade Levels",
        choices = c("All", school_type_choices),
        selected = "All",
        multiple = TRUE
      ),
      
      sliderInput(
        "year_range",
        "Year range",
        min = min(district_panel$Year, na.rm = TRUE),
        max = max(district_panel$Year, na.rm = TRUE),
        value = c(
          min(district_panel$Year, na.rm = TRUE),
          max(district_panel$Year, na.rm = TRUE)
        ),
        step = 1,
        sep = ""
    ),
    
    hr(),
    
    # Footer / credits
    div(
      style = "font-size: 11px; color: #666; margin-top: 15px; line-height: 1.5;",
      
      HTML(
        'This dashboard is updated and maintained by 
      Jonas Xie, Tiffany Wu, and Christina Weiland at the 
      <a href="https://edpolicy.umich.edu/" target="_blank">
      University of Michigan&apos;s Education Policy Initiative
      </a>.
      <br><br>
      Please contact 
      <a href="mailto:weilandc@umich.edu">weilandc@umich.edu</a> 
      with any questions.'
      )
    )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          DTOutput("desc_table")
        ),
        tabPanel(
          "Total Enrollment Change Map",
          tags$iframe(
            src = "map_files/enrollment_change_map.html",
            style = "width: 100%; height: 700px; border: none;",
            seamless = NA
          )
        ),
        tabPanel(
          "Schools Closed by Year Maps (Cumulative)",
          plotOutput("cumulative_closure_maps")
        ),
        
        tabPanel(
          "Schools Closed by Year Maps",
          plotOutput("year_maps_grid")
        ),
        tabPanel(
          "Closure Timing Patterns",
          plotOutput("closure_histogram", height = "500px")
        ),
        tabPanel(
          "Enrollment Trends: Consolidation Year",
          div(
            style = "background-color: #f7fbff; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
            
            h4("What this plot shows", style = "margin-top: 0;"),
            
            p(
              "These plots show how enrollment changes over time within each district, centered around the timing of school consolidation. 
      Each panel represents a district, and the dashed vertical line marks the first year a school closure occurs.",
              style = "font-size: 13px;"
            ),
            
            p(
              "The goal is to compare enrollment trends before (in blue) and after (in red) consolidation to understand how student populations change around these events.",
              style = "font-size: 13px;"
            ),
            
            hr(),
            
            h4("Using the School Grade Levels gilter", style = "margin-top: 10px;"),
            
            tags$ul(
              style = "font-size: 13px;",
              
              tags$li(
                strong("Enrollment: "),
                "Filtering by School Grade Level will only show enrollment for the selected school grades(s) only (e.g., elementary enrollment if 'Elementary' is selected)."
              ),
              
              tags$li(
                strong("Consolidation timing: "),
                "The dashed line marks the first closure of the selected school grades level(s) in that district."
              ),
              
              tags$li(
                strong("Districts shown: "),
                "Only districts with at least one closure of the selected school grade level(s) are included."
              )
            )
              ),
          plotOutput("did_plot_1")
        ),
        
        tabPanel(
          "Enrollment Trends: Year Before",
          div(
            style = "background-color: #f7fbff; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
            
            h4("What this plot shows", style = "margin-top: 0;"),
            
            p(
              "These plots show enrollment trends leading up to school consolidation. 
      The dashed vertical line is shifted one year earlier than the first closure, allowing you to examine changes just before consolidation occurs.",
              style = "font-size: 13px;"
            ),
            
            p(
              "This helps highlight whether enrollment was already declining prior to the first school closure.",
              style = "font-size: 13px;"
            )
            ),
            
          plotOutput("did_plot_2")
        ),
        
        tabPanel(
          "Enrollment Trends: Compared with Population",
          div(
            style = "background-color: #f7fbff; padding: 12px; border-radius: 6px; margin-bottom: 15px;",
            
            h4("How to read the plot below", style = "margin-top: 0;"),
            
            #  Intro (before example)
            p(
              "The plot below shows district enrollment alongside county population over time. 
    Because these measures are on very different scales, both are indexed to 100 in the first year shown for each district. 
    This allows them to be displayed on the same graph and compared more easily.",
              style = "font-size: 13px;"
            ),
            
            p(
              "After indexing, values represent percent change relative to the starting year. 
    For instance, a value of 90 indicates a 10% decline from the baseline of 100. The table below shows an example  to further explain this.",
              style = "font-size: 13px; color: #555;"
            ),
            
            #  Example table
            HTML("
  <table style='border-collapse: collapse; width: 100%; font-size: 13px; margin-top: 10px;'>
    <tr>
      <th style='border-bottom: 1px solid #ccc; text-align: left;'>Year</th>
      <th style='border-bottom: 1px solid #ccc; text-align: left;'>School Enrollment</th>
      <th style='border-bottom: 1px solid #ccc; text-align: left;'>Total Population</th>
      <th style='border-bottom: 1px solid #ccc; text-align: left;'>Enrollment Index</th>
      <th style='border-bottom: 1px solid #ccc; text-align: left;'>Population Index</th>
    </tr>
    <tr>
      <td>2015</td>
      <td>2,000</td>
      <td>60,000</td>
      <td>100</td>
      <td>100</td>
    </tr>
    <tr>
      <td>2016</td>
      <td>1,800</td>
      <td>59,000</td>
      <td>90</td>
      <td>98.3</td>
    </tr>
  </table>
  "),
            
            # Explanation of example
            p(
              "In this example, from the year 2015 to 2016, school enrollment declines by 10% (from 2,000 students to 1,800 students, or an enrollment index from 100 to 90), while 
              population only declines by about 1.7% (from 60,000 people to 59,000 people, or an enrollment index from 100 to 98.3). 
    This shows that enrollment is falling faster than the underlying population.",
              style = "font-size: 13px; color: #555; margin-top: 10px;"
            )
          ),
          plotOutput("did_plot_pop")
        )
      )
    )
  )
)

# =========================
# 5. SERVER
# =========================
server <- function(input, output, session) {
  
  facet_plot_height <- function(df, ncol = 3,
                                row_height = 200,
                                min_height = 450) {
    
    n_panels <- dplyr::n_distinct(df$facet_label)
    
    n_rows <- ceiling(n_panels / ncol)
    
    max(min_height, n_rows * row_height)
  }
  
  map_plot_height <- function(df, panels_per_row = 3,
                              row_height = 300,
                              min_height = 450) {
    n_panels <- dplyr::n_distinct(df$Year)
    n_rows <- ceiling(n_panels / panels_per_row)
    max(min_height, n_rows * row_height)
  }
  
  observeEvent(input$district_filter, {
    selected <- input$district_filter
    
    if ("All" %in% selected && length(selected) > 1) {
      updateSelectInput(
        session,
        "district_filter",
        selected = setdiff(selected, "All")
      )
    }
    
    if (length(selected) == 0) {
      updateSelectInput(
        session,
        "district_filter",
        selected = "All"
      )
    }
  })
  
  observeEvent(input$school_type_filter, {
    selected <- input$school_type_filter
    
    if ("All" %in% selected && length(selected) > 1) {
      updateSelectInput(
        session,
        "school_type_filter",
        selected = setdiff(selected, "All")
      )
    }
    
    if (length(selected) == 0) {
      updateSelectInput(
        session,
        "school_type_filter",
        selected = "All"
      )
    }
  })
  
  
  filtered_school_status <- reactive({
    df <- school_status
    
    if (!is.null(input$district_filter) && !("All" %in% input$district_filter)) {
      df <- df %>% filter(District %in% input$district_filter)
    }
    
    if (!is.null(input$school_type_filter) && !("All" %in% input$school_type_filter)) {
      df <- df %>% filter(Type %in% input$school_type_filter)
    }
    
    df
  })
  
#  filtered_district_panel <- reactive({
#    keep_districts <- filtered_school_status() %>%
#      distinct(District) %>%
#      pull(District)
  
  filtered_district_panel <- reactive({
    
    selected_types <- input$school_type_filter
    
    if (!is.null(selected_types) &&
        length(selected_types) > 1 &&
        "All" %in% selected_types) {
      selected_types <- setdiff(selected_types, "All")
    }
    
    # Enrollment data: all students in selected school type(s)
    comp_df <- school_comp_clean
    
    # Closure timing data: closures in selected school type(s)
    status_df <- school_status %>%
      filter(closed == TRUE, !is.na(end_year))
    
    # Apply school type filter
    if (!is.null(selected_types) && !("All" %in% selected_types)) {
      comp_df <- comp_df %>%
        filter(Type %in% selected_types)
      
      status_df <- status_df %>%
        filter(Type %in% selected_types)
    }
    
    # Apply district filter
    if (!is.null(input$district_filter) && !("All" %in% input$district_filter)) {
      comp_df <- comp_df %>%
        filter(District %in% input$district_filter)
      
      status_df <- status_df %>%
        filter(District %in% input$district_filter)
    }
    
    # Apply year filter to enrollment only
    comp_df <- comp_df %>%
      filter(
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
    
    # District-level enrollment for selected type(s)
    enr_df <- comp_df %>%
      group_by(District, Year) %>%
      summarise(
        enrollment = sum(headcount, na.rm = TRUE),
        .groups = "drop"
      )
    
    # District-level first closure year for selected type(s)
    closure_df <- status_df %>%
      group_by(District) %>%
      summarise(
        first_cons_year = min(end_year, na.rm = TRUE),
        last_cons_year = max(end_year, na.rm = TRUE),
        n_closed = n(),
        .groups = "drop"
      )
    
    validate(
      need(nrow(closure_df) > 0,
           "No districts have closures for the selected filters.")
    )
    
    out <- enr_df %>%
      inner_join(closure_df, by = "District") %>%
      left_join(census, by = c("District", "Year"))
    
    validate(
      need(nrow(out) > 0,
           "No enrollment data match the selected district, school grade level, and year range.")
    )
    
    out
  })
  
  
  filtered_map_sf <- reactive({
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    df <- district_map_sf %>%
      filter(District %in% keep_districts)
    
    validate(need(nrow(df) > 0, "No districts match the current filters."))
    df
  })
  
  filtered_school_comp <- reactive({
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    school_comp_clean %>%
      filter(
        District %in% keep_districts,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      )
  })
  
  output$desc_table <- renderDT({
    desc_df <- filtered_school_comp() %>%
      group_by(Year, District) %>%
      summarise(
        `Total # Students` = sum(headcount, na.rm = TRUE),
        `Total # Schools` = n_distinct(School),
        .groups = "drop"
      ) %>%
      left_join(
        filtered_school_status() %>%
          filter(closed, !is.na(end_year)) %>%
          group_by(District, Year = end_year) %>%
          summarise(
            `# Schools Closed` = n(),
            `Schools Closed (Names)` = paste(sort(unique(School)), collapse = "; "),
            .groups = "drop"
          ),
        by = c("District", "Year")
      ) %>%
      mutate(
        `# Schools Closed` = tidyr::replace_na(`# Schools Closed`, 0),
        `Schools Closed (Names)` = ifelse(
          is.na(`Schools Closed (Names)`),
          "None",
          `Schools Closed (Names)`
        )
      ) %>%
      arrange(District, Year)
    
    
    DT::datatable(
      desc_df,
      rownames = FALSE,
      options = list(
        pageLength = 16,
        scrollX = TRUE
      )
    )
  })
  
  plot_data_cons <- reactive({
    build_pre_post_plot_data(filtered_district_panel(), offset = 0)
  })
  
  plot_data_pre <- reactive({
    build_pre_post_plot_data(filtered_district_panel(), offset = -1)
  })
  
  output$did_plot_1 <- renderPlot({
    
    df <- plot_data_cons()
    req(nrow(df) > 0)
    
    vlines <- df %>%
      distinct(facet_label, threshold)
    
    ggplot(df, aes(x = Year, y = enrollment, color = period, group = 1)) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.8) +
      geom_vline(
        data = vlines,
        aes(xintercept = threshold + 0.5),
        inherit.aes = FALSE,
        linetype = "dashed",
        color = "gray50",
        linewidth = 0.4
      ) +
      facet_wrap(~facet_label, scales = "free_y", ncol = 3) +
      scale_color_manual(values = c("pre" = "#2b6cb0", "post" = "#c53030")) +
      theme_minimal(base_size = 10) +
      theme(aspect.ratio = 0.7)
    
  }, height = function() {
    facet_plot_height(plot_data_cons())
  })
  

  output$did_plot_2 <- renderPlot({
    df <- plot_data_pre()
    req(nrow(df) > 0)
    
    vlines <- df %>%
      distinct(facet_label, threshold)
    
    ggplot(df, aes(x = Year, y = enrollment, color = period, group = 1)) +
      geom_line(linewidth = 0.7) +
      geom_point(size = 1.8) +
      geom_vline(
        data = vlines,
        aes(xintercept = threshold + 0.5),
        inherit.aes = FALSE,
        linetype = "dashed",
        color = "gray50",
        linewidth = 0.4
      ) +
      facet_wrap(~facet_label, scales = "free_y", ncol = 3) +
      scale_color_manual(values = c("pre" = "#2b6cb0", "post" = "#c53030")) +
      labs(
        title = "Pre/post enrollment (threshold = one year before first consolidation)",
        x = NULL,
        y = "Enrollment",
        color = NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "top",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        axis.title.y = element_text(size = 10),
        aspect.ratio = 0.7
      )
  }, height = function() {
    facet_plot_height(plot_data_pre())
  })
  
  
  year_maps_sf <- reactive({
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    shape_df <- district_shapes %>%
      mutate(
        District = stringr::str_remove(NAME, " County School District$| County Schools$| Schools$| School District$"),
        District = stringr::str_squish(District)
      )
    
    panel_df <- district_panel %>%
      filter(
        District %in% keep_districts,
        Year >= input$year_range[1],
        Year <= input$year_range[2]
      ) %>%
      select(District, Year, enrollment)
    
    out <- shape_df %>%
      left_join(panel_df, by = "District") %>%
      filter(!is.na(Year))
    
    validate(need(nrow(out) > 0, "No districts/years match the current filters."))
    out
  })
  
  output$year_maps_grid <- renderPlot({
    
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    closure_map_df <- build_closure_year_map_data(
      school_status = filtered_school_status(),
      district_shapes = district_shapes,
      year_min = input$year_range[1],
      year_max = input$year_range[2]
    ) %>%
      filter(District %in% keep_districts)
    
    req(nrow(closure_map_df) > 0)
    
    closure_map_df$Year <- factor(closure_map_df$Year)
    
    ggplot(closure_map_df) +
      geom_sf(aes(fill = n_closed_that_year), color = "gray50", linewidth = 0.15) +
      geom_sf_text(
        data = closure_map_df %>% filter(n_closed_that_year > 0),
        aes(label = District),
        size = 3
      ) +
      facet_wrap(~Year, ncol = 3) +
      scale_fill_gradient(
        low = "#f7fbff",
        high = "red3",
        limits = c(0, 10),
        breaks = seq(0, 10, by = 2),
        na.value = "#d9d9d9",
        oob = scales::squish
      ) +
      labs(
        title = "Number of Schools Closed Within Each District by Year",
        fill = "# Closed"
      ) +
      theme_void() +
      theme(
        strip.text = element_text(size = 9),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom",
        aspect.ratio = 0.7
      )
  }, height = function() {
    
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    closure_map_df <- build_closure_year_map_data(
      school_status = filtered_school_status(),
      district_shapes = district_shapes,
      year_min = input$year_range[1],
      year_max = input$year_range[2]
    ) %>%
      filter(District %in% keep_districts)
    
    map_plot_height(closure_map_df, panels_per_row = 3)
  })
  
# test pop
  # Build the same data used in the app
#  df <- build_pre_post_plot_data(district_panel, offset = 0)
  
  # Optional: filter to a few districts so it's not overwhelming
#  df <- df %>%
#    filter(District %in% unique(District)[1:6])
#  df <- df %>%
#    group_by(facet_label) %>%
#    mutate(
#      enrollment_index = 100 * enrollment / first(enrollment),
#      population_index = 100 * population / first(population)
#    ) %>%
#    ungroup()
  
  
  output$did_plot_pop <- renderPlot({
    
    df <- plot_data_cons()
    req(nrow(df) > 0)
    
    df <- df %>%
      arrange(facet_label, Year) %>%
      group_by(facet_label) %>%
      mutate(
        enrollment_base = first(enrollment[!is.na(enrollment) & enrollment > 0]),
        population_base = first(population[!is.na(population) & population > 0]),
        enrollment_index = 100 * enrollment / enrollment_base,
        population_index = 100 * population / population_base
      ) %>%
      ungroup() %>%
      filter(
        !is.na(enrollment_index),
        !is.na(population_index)
      )
    
    req(nrow(df) > 0)
    
    vlines <- df %>%
      distinct(facet_label, threshold)
    
    school_type_label <- ifelse(
      is.null(input$school_type_filter) || "All" %in% input$school_type_filter,
      "all school types",
      paste(input$school_type_filter, collapse = " + ")
    )
    
    ggplot(df, aes(x = Year)) +
      geom_line(
        aes(y = enrollment_index, color = period, group = 1),
        linewidth = 0.7
      ) +
      geom_point(
        aes(y = enrollment_index, color = period),
        size = 1.8
      ) +
      geom_line(
        aes(y = population_index, group = 1),
        color = "darkgray",
        linewidth = 0.9
      ) +
      geom_vline(
        data = vlines,
        aes(xintercept = threshold + 0.5),
        inherit.aes = FALSE,
        linetype = "dashed",
        color = "gray50",
        linewidth = 0.4
      ) +
      facet_wrap(~facet_label, ncol = 3, scales = "free_y") +
      scale_color_manual(values = c("pre" = "#2b6cb0", "post" = "#c53030")) +
      labs(
        title = "Enrollment and Population Trends Around First Closure",
        subtitle = paste0(
          "Enrollment reflects ", school_type_label,
          "; population shown in gray. Both are indexed to 100."
        ),
        x = NULL,
        y = "Index (Base Year = 100)",
        color = NULL
      ) +
      theme_minimal(base_size = 10) +
      theme(
        legend.position = "top",
        strip.text = element_text(size = 8),
        axis.text = element_text(size = 7),
        aspect.ratio = 0.7
      )
  }, height = function() {
    facet_plot_height(plot_data_cons())
  })
  
  output$closure_histogram <- renderPlot({
    
    closure_year_df <- filtered_school_status() %>%
      filter(
        closed,
        !is.na(end_year),
        end_year >= input$year_range[1],
        end_year <= input$year_range[2]
      ) %>%
      count(end_year, name = "n_closed")
    
    req(nrow(closure_year_df) > 0)
    
    ggplot(closure_year_df, aes(x = end_year, y = n_closed)) +
      geom_col(fill = "skyblue3", width = 0.8) +
      geom_text(
        aes(label = n_closed),
        vjust = -0.3,
        size = 3
      ) +
      scale_x_continuous(
        breaks = seq(input$year_range[1], input$year_range[2], by = 1)
      ) +
      coord_cartesian(
        xlim = c(input$year_range[1] - 0.5, input$year_range[2] + 0.5)
      ) +
      labs(
        title = "Number of School Closures by Year",
        x = "Year",
        y = "# Schools Closed"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$cumulative_closure_maps <- renderPlot({
    
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    cumulative_map_df <- build_cumulative_closure_map_data(
      school_status = filtered_school_status(),
      district_shapes = district_shapes,
      year_min = input$year_range[1],
      year_max = input$year_range[2]
    ) %>%
      filter(District %in% keep_districts)
    
    req(nrow(cumulative_map_df) > 0)
    
    cumulative_map_df$Year <- factor(cumulative_map_df$Year)
    
    ggplot(cumulative_map_df) +
      geom_sf(aes(fill = cumulative_closed), color = "gray50", linewidth = 0.15) +
      geom_sf_text(
        data = cumulative_map_df %>% filter(cumulative_closed > 0),
        aes(label = District),
        size = 2
      ) +
      facet_wrap(~Year, ncol = 3) +
      scale_fill_gradient(
        low = "#f7fbff",
        high = "red3",
        limits = c(0, 10),
        oob = scales::squish,
        breaks = seq(0, 10, by = 2),
        labels = c("0", "2", "4", "6", "8", "10+"),
        na.value = "#d9d9d9"
      ) +
      labs(
        title = "Cumulative Number of Schools Closed Within Each District by Year",
        fill = "# Closed"
      ) +
      theme_void() +
      theme(
        strip.text = element_text(size = 9),
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = "bottom"
      )
  }, height = function() {
    
    keep_districts <- filtered_school_status() %>%
      distinct(District) %>%
      pull(District)
    
    closure_map_df <- build_closure_year_map_data(
      school_status = filtered_school_status(),
      district_shapes = district_shapes,
      year_min = input$year_range[1],
      year_max = input$year_range[2]
    ) %>%
      filter(District %in% keep_districts)
    
    map_plot_height(closure_map_df, panels_per_row = 3)
  })
}
  
# =========================
# 6. RUN APP
# =========================
shinyApp(ui = ui, server = server)
