# West Virginia Project
# Data preparation script
# Run this script after changing raw inputs; app.R loads processed_data/app_data.rds.

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
# district column â€?we filter using whatever that column is called:
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
# 1a. LOAD INFERRED SCHOOL TYPES (joined on SchlID, not school name)
# =========================
# Loads final_type from inferred_school_types_final.csv and attaches it as
# Type_inferred alongside the existing Type column. The merge key is SchlID
# (6-digit district+local school code), which is more stable than the school
# name. The inferred file's own School column is also carried through as
# School_inferred so we can verify in section 3a whether the SchlID match
# actually points to the same school (school codes are occasionally reused
# after closures/consolidations).

# Zero-pad a school identifier to a 6-character string for matching.
pad_schlid <- function(x) {
  x <- stringr::str_squish(as.character(x))
  x <- stringr::str_pad(x, width = 6, side = "left", pad = "0")
  x
}

# Helper to normalize names for similarity scoring (used only in verification,
# not in the join itself).
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
    SchlID_key    = pad_schlid(SchlID),
    School_inferred = stringr::str_squish(School),
    Type_inferred = final_type_to_label(final_type)
  ) %>%
  filter(!is.na(Type_inferred), !is.na(SchlID_key), SchlID_key != "000000") %>%
  distinct(SchlID_key, .keep_all = TRUE) %>%
  select(SchlID_key, School_inferred, Type_inferred)

# Attach Type_inferred + School_inferred to school_composition.
# school_composition's SchlID = Dist (3 digits) concatenated with Schl (3 digits).
school_composition <- school_composition %>%
  mutate(
    SchlID_key = paste0(
      stringr::str_pad(stringr::str_squish(as.character(Dist)), 3, "left", "0"),
      stringr::str_pad(stringr::str_squish(as.character(Schl)), 3, "left", "0")
    )
  ) %>%
  left_join(inferred_school_types, by = "SchlID_key") %>%
  select(-SchlID_key)

# Attach Type_inferred + School_inferred to school_existence.
# school_existence's SchlID = Schl_ID (already 6 digits, but pad defensively).
school_existence <- school_existence %>%
  mutate(SchlID_key = pad_schlid(Schl_ID)) %>%
  left_join(inferred_school_types, by = "SchlID_key") %>%
  select(-SchlID_key)

# =========================
# 1b. LOAD CURATED FINAL TYPES (source of truth for visualization)
# =========================
# school_existence_with_cleaned_types.xlsx is a CURATED file the user manually
# edited: it carries Type_raw / Type_pipeline / Type_inferred plus a hand-
# verified `type_final` column. We treat this file as read-only input and use
# `type_final` as the canonical school Type everywhere downstream
# (visualizations, filters, panels). The earlier Type / Type_inferred /
# Type_raw columns are kept for transparency and for the comparison sheets.
curated_path <- "school_existence_with_cleaned_types.xlsx"
if (!file.exists(curated_path)) {
  warning(curated_path, " not found â€?Type_final will be NA and Type will fall ",
          "back to the pipeline value.")
  curated_final_types <- tibble::tibble(
    SchlID_key = character(0),
    Type_final = character(0)
  )
} else {
  curated_final_types <- read_excel(curated_path) %>%
    mutate(
      SchlID_key = pad_schlid(Schl_ID),
      Type_final = stringr::str_squish(as.character(type_final))
    ) %>%
    filter(!is.na(SchlID_key), nzchar(SchlID_key)) %>%
    distinct(SchlID_key, .keep_all = TRUE) %>%
    select(SchlID_key, Type_final)
  message("Loaded curated type_final for ", nrow(curated_final_types), " schools.")
}

# Attach Type_final to school_existence (by Schl_ID).
school_existence <- school_existence %>%
  mutate(SchlID_key = pad_schlid(Schl_ID)) %>%
  left_join(curated_final_types, by = "SchlID_key") %>%
  select(-SchlID_key)

# Attach Type_final to school_composition (by Dist + Schl -> 6-char SchlID).
school_composition <- school_composition %>%
  mutate(
    SchlID_key = paste0(
      stringr::str_pad(stringr::str_squish(as.character(Dist)), 3, "left", "0"),
      stringr::str_pad(stringr::str_squish(as.character(Schl)), 3, "left", "0")
    )
  ) %>%
  left_join(curated_final_types, by = "SchlID_key") %>%
  select(-SchlID_key)
# =========================
# 2. HELPER FUNCTIONS
# =========================
clean_school_status <- function(df) {
  if (!"Type_inferred"   %in% names(df)) df$Type_inferred   <- NA_character_
  if (!"Type_raw"        %in% names(df)) df$Type_raw        <- NA_character_
  if (!"Type_final"      %in% names(df)) df$Type_final      <- NA_character_
  if (!"School_inferred" %in% names(df)) df$School_inferred <- NA_character_

  df %>%
    mutate(across(where(is.character), stringr::str_squish)) %>%
    mutate(
      Schl_ID = as.character(Schl_ID),
      District = str_remove(District, " County Schools$| Schools$| School District$"),
      District = str_squish(District),
      School = str_squish(School),
      School_inferred = str_squish(School_inferred),
      Type = str_squish(Type),
      Type_raw = str_squish(Type_raw),
      Type_inferred = str_squish(Type_inferred),
      Type_final = str_squish(Type_final),
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

  if (!"Type_inferred"   %in% names(df)) df$Type_inferred   <- NA_character_
  if (!"Type_raw"        %in% names(df)) df$Type_raw        <- NA_character_
  if (!"Type_final"      %in% names(df)) df$Type_final      <- NA_character_
  if (!"School_inferred" %in% names(df)) df$School_inferred <- NA_character_

  df %>%
    mutate(across(where(is.character), stringr::str_squish)) %>%
    mutate(
      Year = as.integer(Year),
      Schl = as.character(Schl),
      District = str_remove(District, " County Schools$| Schools$| School District$"),
      District = str_squish(District),
      School = str_squish(School),
      School_inferred = str_squish(School_inferred),
      Type = str_squish(Type),
      Type_raw = str_squish(Type_raw),
      Type_inferred = str_squish(Type_inferred),
      Type_final = str_squish(Type_final),
      headcount = readr::parse_number(`Total Headcount`)
    ) %>%
    select(Year, District, Schl, School, School_inferred,
           Type_raw, Type, Type_inferred, Type_final, headcount)
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

# --- Name similarity helper for verifying the SchlID merge ---
# Token-Jaccard similarity on normalized names: shared tokens / total unique tokens.
# 1.00 = identical after normalization, 0.00 = no tokens shared, NA = either name missing.
name_similarity <- function(a, b) {
  out <- rep(NA_real_, length(a))
  for (i in seq_along(a)) {
    if (is.na(a[i]) || is.na(b[i])) next
    ta <- strsplit(normalize_school_name(a[i]), " ", fixed = TRUE)[[1]]
    tb <- strsplit(normalize_school_name(b[i]), " ", fixed = TRUE)[[1]]
    ta <- ta[nchar(ta) > 0]; tb <- tb[nchar(tb) > 0]
    if (!length(ta) && !length(tb)) { out[i] <- 1; next }
    if (!length(ta) ||  !length(tb)) { out[i] <- 0; next }
    inter <- length(intersect(ta, tb))
    uni   <- length(union(ta, tb))
    out[i] <- if (uni == 0) NA_real_ else inter / uni
  }
  out
}

merge_check <- function(sim) {
  dplyr::case_when(
    is.na(sim)  ~ "no_inferred_match",
    sim >= 0.80 ~ "ok",
    sim >= 0.40 ~ "minor_diff",        # name formatting / suffix differences
    TRUE        ~ "likely_wrong_match" # names look like different schools
  )
}

# school_status: one row per school in the existence/status file
type_comparison_status <- school_status %>%
  mutate(
    Type_raw         = str_squish(Type_raw),
    Type             = str_squish(Type),
    Type_inferred    = str_squish(Type_inferred),
    Type_final       = str_squish(Type_final),
    School_inferred  = str_squish(School_inferred),
    name_similarity  = name_similarity(School, School_inferred),
    merge_check      = merge_check(name_similarity),
    type_match       = classify_match(Type_raw, Type, Type_inferred)
  ) %>%
  select(District, Schl_ID, School, School_inferred,
         name_similarity, merge_check,
         Type_raw, Type_pipeline = Type, Type_inferred, Type_final,
         type_match)

# school_comp_clean: collapse the year panel to one row per (Schl, School)
type_comparison_comp <- school_comp_clean %>%
  mutate(
    Type_raw         = str_squish(Type_raw),
    Type             = str_squish(Type),
    Type_inferred    = str_squish(Type_inferred),
    Type_final       = str_squish(Type_final),
    School_inferred  = str_squish(School_inferred)
  ) %>%
  distinct(District, Schl, School, School_inferred,
           Type_raw, Type, Type_inferred, Type_final) %>%
  mutate(
    name_similarity = name_similarity(School, School_inferred),
    merge_check     = merge_check(name_similarity),
    type_match      = classify_match(Type_raw, Type, Type_inferred)
  ) %>%
  rename(Type_pipeline = Type)

# Quick console summary
message("Three-source type match â€?school_status:")
print(table(type_comparison_status$type_match, useNA = "ifany"))
message("Three-source type match â€?school_comp_clean:")
print(table(type_comparison_comp$type_match, useNA = "ifany"))

message("SchlID merge check (name similarity) â€?school_status:")
print(table(type_comparison_status$merge_check, useNA = "ifany"))
message("SchlID merge check (name similarity) â€?school_comp_clean:")
print(table(type_comparison_comp$merge_check, useNA = "ifany"))

# -------------------------------------------------------------
# Write the comparison to an Excel workbook for manual review.
# Sheets:
#   school_status_all      â€?all schools in school_status with the 3 type cols
#   school_comp_all        â€?distinct (Schl, School) rows from school_comp_clean
#   school_status_disagree â€?only rows where the 3 sources don't unanimously agree
#   school_comp_disagree   â€?same, for school_comp_clean
#   summary_status / summary_comp â€?counts by type_match
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

# Merge-quality sheets: SchlID matched but school names look different.
# Sorted worst-first so the suspect rows (likely school-code reuse) are on top.
merge_check_status <- dplyr::arrange(
  dplyr::filter(type_comparison_status, merge_check %in% c("minor_diff", "likely_wrong_match")),
  name_similarity, District, School
)
merge_check_comp <- dplyr::arrange(
  dplyr::filter(type_comparison_comp,   merge_check %in% c("minor_diff", "likely_wrong_match")),
  name_similarity, District, School
)

merge_check_summary <- dplyr::bind_rows(
  dplyr::mutate(dplyr::count(type_comparison_status, merge_check, name = "n"),
                source = "school_status"),
  dplyr::mutate(dplyr::count(type_comparison_comp,   merge_check, name = "n"),
                source = "school_comp_clean")
)
merge_check_summary <- merge_check_summary[, c("source", "merge_check", "n")]

openxlsx::write.xlsx(
  list(
    school_status_all      = school_status_all,
    school_comp_all        = school_comp_all,
    school_status_disagree = school_status_disagree,
    school_comp_disagree   = school_comp_disagree,
    summary_status         = summary_status,
    summary_comp           = summary_comp,
    merge_check_status     = merge_check_status,
    merge_check_comp       = merge_check_comp,
    merge_check_summary    = merge_check_summary
  ),
  file = type_check_path,
  overwrite = TRUE
)

message("Wrote three-source type comparison to: ", normalizePath(type_check_path))

# =========================
# 3b. PER-SCHOOL CSV: types + status (from school_comp_clean joined to school_status)
# =========================
# One row per school in the enrollment file. Carries all three Type columns and
# the status fields (start/end year, closed, flipped, news URLs) from school_status.
# Status is joined on District + the last 3 chars of the school code, which is
# the same matching rule already used elsewhere in this script (Schl in
# school_composition is the 3-digit local code; Schl_ID in school_status is
# the 6-digit district+local code).

# Distinct school-level rows from school_comp_clean
schools_from_comp <- dplyr::group_by(school_comp_clean, District, Schl)
schools_from_comp <- dplyr::summarise(
  schools_from_comp,
  School                = dplyr::first(School),
  Type_raw              = dplyr::first(Type_raw),
  Type_pipeline         = dplyr::first(Type),
  Type_inferred         = dplyr::first(Type_inferred),
  n_years_with_data     = dplyr::n_distinct(Year),
  first_year_with_data  = suppressWarnings(min(Year, na.rm = TRUE)),
  last_year_with_data   = suppressWarnings(max(Year, na.rm = TRUE)),
  total_headcount_last  = sum(headcount[Year == suppressWarnings(max(Year, na.rm = TRUE))],
                              na.rm = TRUE),
  .groups = "drop"
)

# Status side (the columns we want to merge in)
status_cols <- intersect(
  c("Schl_ID", "start_year", "end_year", "flipped", "closed",
    "Consolidation_Summary", "Closure_News_URL", "Closure_News_URL2"),
  names(school_status)
)
status_for_join <- dplyr::mutate(
  school_status,
  school_match_id = stringr::str_sub(as.character(Schl_ID), -3, -1)
)
status_for_join <- dplyr::select(status_for_join,
                                 dplyr::all_of(c("District", "school_match_id", status_cols)))

schools_with_types_and_status <- dplyr::mutate(
  schools_from_comp,
  school_match_id = stringr::str_sub(as.character(Schl), -3, -1)
)
schools_with_types_and_status <- dplyr::left_join(
  schools_with_types_and_status,
  status_for_join,
  by = c("District", "school_match_id")
)
schools_with_types_and_status <- dplyr::select(schools_with_types_and_status,
                                               -school_match_id)
schools_with_types_and_status <- dplyr::arrange(schools_with_types_and_status,
                                                District, School)

schools_csv_path <- "schools_with_types_and_status.csv"
write.csv(schools_with_types_and_status, schools_csv_path, row.names = FALSE, na = "")
message("Wrote per-school types + status CSV to: ", normalizePath(schools_csv_path))

# =========================
# 3c. MERGED school_existence.xlsx with cleaned types
# =========================
# Take the cleaned school_status (which already carries Type_raw / Type / Type_inferred
# along with every original existence column) and write it back to an .xlsx with
# the three Type columns up front. Saved to a NEW filename so the original
# school_existence.xlsx is not overwritten â€?rename it if you want to swap it in.
school_existence_merged <- dplyr::rename(school_status, Type_pipeline = Type)

# Put the three type columns right after School for easy scanning
front_cols <- intersect(
  c("Schl_ID", "District", "School", "Type_raw", "Type_pipeline", "Type_inferred"),
  names(school_existence_merged)
)
other_cols <- setdiff(names(school_existence_merged), front_cols)
school_existence_merged <- school_existence_merged[, c(front_cols, other_cols)]

# Auto-write disabled: school_existence_with_cleaned_types.xlsx is now the
# CURATED INPUT (it carries the user's hand-verified `type_final` column) and
# must not be overwritten by the script. If you want an updated snapshot of
# the pipeline's view, write to a different filename, e.g.:
# openxlsx::write.xlsx(school_existence_merged,
#                      "school_existence_pipeline_types.xlsx",
#                      overwrite = TRUE)


# =========================
# 3d. OVERRIDE Type WITH CURATED type_final
# =========================
# This is the canonical Type column from this point on. Every downstream
# visualization, filter, panel, and map reads `Type`, so by replacing it here
# (after the comparison sheets and CSV are written) we ensure the dashboard
# shows the user's curated types â€?falling back to the pipeline value only if
# Type_final is missing for a school.
apply_type_final <- function(df) {
  if (!"Type_final" %in% names(df)) return(df)
  has_final     <- !is.na(df$Type_final) & nzchar(df$Type_final)
  df$Type_final_source <- ifelse(has_final, "curated", "pipeline_fallback")
  df$Type <- ifelse(has_final, df$Type_final, df$Type)
  df
}

school_status    <- apply_type_final(school_status)
school_comp_clean <- apply_type_final(school_comp_clean)

message(sprintf(
  "Type_final applied â€?school_status: %d curated, %d pipeline fallback",
  sum(school_status$Type_final_source == "curated",            na.rm = TRUE),
  sum(school_status$Type_final_source == "pipeline_fallback",  na.rm = TRUE)
))
message(sprintf(
  "Type_final applied â€?school_comp_clean: %d curated, %d pipeline fallback",
  sum(school_comp_clean$Type_final_source == "curated",           na.rm = TRUE),
  sum(school_comp_clean$Type_final_source == "pipeline_fallback", na.rm = TRUE)
))


district_panel <- build_district_panel(district_enr, school_status) %>%
  left_join(census, by = c("District", "Year"))

district_shapes <- sf::st_read("district_shapes/tl_2021_54_unsd.shp", quiet = TRUE)
district_shapes <- sf::st_transform(district_shapes, 4326)

school_type_order <- c(
  "Elementary",
  "Elementary + Middle",
  "Middle",
  "High School",
  "Alternative"
)

school_type_choices <- unique(c(
  as.character(school_status$Type),
  as.character(school_comp_clean$Type)
))
school_type_choices <- school_type_choices[!is.na(school_type_choices)]
school_type_choices <- c(
  school_type_order[school_type_order %in% school_type_choices],
  sort(setdiff(school_type_choices, school_type_order))
)


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

# =========================
# 4. SAVE PROCESSED APP DATA
# =========================
processed_dir <- "processed_data"
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

app_data <- list(
  school_status = school_status,
  school_comp_clean = school_comp_clean,
  district_panel = district_panel,
  district_shapes = district_shapes,
  census = census,
  school_type_choices = school_type_choices,
  closure_map_df = closure_map_df
)

processed_path <- file.path(processed_dir, "app_data.rds")
saveRDS(app_data, processed_path)
message("Wrote processed app data to: ", normalizePath(processed_path))
