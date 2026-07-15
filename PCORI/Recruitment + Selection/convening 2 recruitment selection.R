# ==============================================================================
# PCORI PROFESSIONAL-ONLY RECRUITMENT - CLEAN DYNAMIC WEIGHTED RANDOM SCRIPT
# Target: 50 CONFIRMED professional participants
#
# Design:
# - Proportional weighted random sampling
# - Occupation is the strongest dynamic priority
# - Men receive a dynamic boost until cumulative confirmed/completed reaches 30%
# - No hard gender cap or deterministic gender quota
# - Redundant gender logic removed
# - age is included in master output
# ==============================================================================

library(tidyverse)
library(readxl)

# ==============================================================================
# SECTION 1: CONFIGURATION
# ==============================================================================

SOURCE_FILE <- "data/Clinicians + Researchers.csv"

OUT_DIR <- "output"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

MASTER_FILE <- file.path(OUT_DIR, "professional_only_master.csv")

TARGET_TOTAL_N <- 50

OCCUPATION_TARGETS <- c(
  SLP                     = 12,
  OT_PT                   = 8,
  Researcher_Faculty      = 8,
  Physician_APP           = 6,
  Nurse                   = 6,
  Behavioral_Social_Rehab = 6,
  Administrator           = 2,
  Other_Professional      = 2
)
stopifnot(sum(OCCUPATION_TARGETS) == TARGET_TOTAL_N)

OVERALL_TARGETS <- list(
  min_minority_pct    = 0.40,
  male_floor          = 0.30,
  min_rural_pct       = 0.20,
  min_age_50_plus_pct = 0.50
)

# ------------------------------------------------------------------------------
# Weight settings
# ------------------------------------------------------------------------------

# Occupation remains the strongest driver
OCC_BOOST_WEIGHTS <- list(
  very_far_behind = 3.00,   # 6+ still needed
  far_behind      = 2.40,   # 4-5 still needed
  behind          = 1.80,   # 2-3 still needed
  slightly_behind = 1.35,   # 1 still needed
  met_or_over     = 0.40    # target met/exceeded
)

# Men are boosted only when cumulative confirmed/completed is below 30%
MALE_FLOOR_WEIGHTS <- list(
  large_shortfall  = 1.35,
  shortfall        = 1.22,
  slight_shortfall = 1.10,
  on_target        = 1.00,
  female_default   = 1.00,
  other            = 1.00,
  unknown          = 1.00
)

RACE_WEIGHTS <- list(
  black              = 2.40,
  asian              = 2.00,
  hispanic           = 2.30,
  native_american    = 2.40,
  pacific_islander   = 2.40,
  multiracial        = 1.90,
  white_non_hispanic = 1.00,
  other              = 1.25
)

URBAN_WEIGHTS <- list(
  rural   = 1.35,
  urban   = 1.00,
  unknown = 1.00
)

AGE_WEIGHTS <- list(
  age_65_plus  = 1.15,
  age_50_64    = 1.08,
  age_under_50 = 1.00
)

REGION_WEIGHTS <- list(
  southeast = 1.10,
  northeast = 1.00,
  midwest   = 1.00,
  west      = 1.00,
  other     = 1.00
)

# Occupation diversity caps only
MAX_SHARE_PER_OCCUPATION_IN_WAVE  <- 0.35
MAX_SHARE_SLP_IN_WAVE             <- 0.30
MAX_SHARE_PER_OCCUPATION_IN_GROUP <- 0.40
MAX_SHARE_SLP_IN_GROUP            <- 0.33

DECLINE_BUFFER_RATE <- 0.25
WAVE_SIZE_CAP       <- 14
MIN_WAVE_SIZE       <- 4

# ==============================================================================
# SECTION 2: HELPERS
# ==============================================================================

resolve_field_name <- function(data, preferred, candidates = character(0)) {
  possible <- unique(c(preferred, candidates))
  raw_names <- names(data)
  
  exact_match <- possible[possible %in% raw_names]
  if (length(exact_match) > 0) return(exact_match[1])
  
  clean <- function(x) tolower(gsub("[^a-z0-9]", "", x))
  raw_clean <- clean(raw_names)
  possible_clean <- clean(possible)
  
  for (cand in possible_clean) {
    idx <- which(raw_clean == cand)
    if (length(idx) > 0) return(raw_names[idx[1]])
  }
  
  for (cand in possible_clean[nchar(possible_clean) >= 3]) {
    idx <- which(grepl(cand, raw_clean, fixed = TRUE) |
                   grepl(raw_clean, cand, fixed = TRUE))
    if (length(idx) > 0) return(raw_names[idx[1]])
  }
  
  stop(sprintf("Could not find field. Tried: %s", paste(possible, collapse = ", ")))
}

resolve_optional_field_name <- function(data, preferred, candidates = character(0)) {
  tryCatch(resolve_field_name(data, preferred, candidates), error = function(e) NA_character_)
}

safe_prob <- function(x) {
  x <- ifelse(is.na(x) | !is.finite(x) | x <= 0, 0.001, x)
  x / sum(x)
}

assign_region_from_state <- function(state_value) {
  southeast <- c("FL", "GA", "NC", "SC", "VA", "AL", "MS", "TN", "AR", "LA", "KY", "WV")
  northeast <- c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA", "DE", "MD", "DC")
  midwest   <- c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD")
  west      <- c("AZ", "CO", "ID", "MT", "NV", "NM", "UT", "WY", "AK", "CA", "HI", "OR", "WA", "TX", "OK")
  
  state_value <- toupper(trimws(coalesce(as.character(state_value), "")))
  
  case_when(
    state_value %in% southeast ~ "Southeast",
    state_value %in% northeast ~ "Northeast",
    state_value %in% midwest   ~ "Midwest",
    state_value %in% west      ~ "West",
    TRUE                       ~ "Other"
  )
}

assign_occupation_group <- function(x) {
  x <- str_to_lower(coalesce(as.character(x), ""))
  
  case_when(
    str_detect(x, "speech|speech-language|slp") ~ "SLP",
    str_detect(x, "occupational therapist|occupational therapy assistant|\\bota\\b|physical therapist|physical therapy assistant|\\bpta\\b|\\bot\\b|\\bpt\\b|physiotherapist") ~ "OT_PT",
    str_detect(x, "physician assistant|\\bpa\\b|nurse practitioner|\\bnp\\b|aprn|physician|doctor|\\bmd\\b|\\bdo\\b|neurologist|physiatrist") ~ "Physician_APP",
    str_detect(x, "nurse|\\brn\\b|registered nurse|\\blvn\\b") ~ "Nurse",
    str_detect(x, "social work|social worker|case manager|counsel|psycholog|neuropsych|audiolog|rehab counselor|vocational|care coordinator") ~ "Behavioral_Social_Rehab",
    str_detect(x, "research|scientist|faculty|professor|investigator|postdoc|academic") ~ "Researcher_Faculty",
    str_detect(x, "director|manager|administrator|admin|lead|leadership|coordinator|chief") ~ "Administrator",
    TRUE ~ "Other_Professional"
  )
}

determine_wave_spec <- function(n_confirmed, wave_num, target_total = TARGET_TOTAL_N) {
  remaining <- target_total - n_confirmed
  if (remaining <= 0) return(NULL)
  
  needed_with_buffer <- ceiling(remaining * (1 + DECLINE_BUFFER_RATE))
  wave_total <- min(needed_with_buffer, WAVE_SIZE_CAP)
  wave_total <- max(wave_total, min(remaining, MIN_WAVE_SIZE))
  
  if (wave_total >= 12) {
    group_sizes <- c(ceiling(wave_total / 2), floor(wave_total / 2))
  } else {
    group_sizes <- wave_total
  }
  
  start_group <- ((wave_num - 1) * 2) + 1
  groups <- start_group:(start_group + length(group_sizes) - 1)
  
  list(
    wave_num            = wave_num,
    groups              = groups,
    group_sizes         = group_sizes,
    total               = sum(group_sizes),
    remaining_to_target = remaining,
    min_eligible_needed = sum(group_sizes) + 4
  )
}

# ------------------------------------------------------------------------------
# Dynamic progress helpers
# ------------------------------------------------------------------------------

get_occ_status <- function(confirmed_data, targets) {
  base <- tibble(
    occupation_group = names(targets),
    target_n         = as.numeric(targets)
  )
  
  if (!("occupation_group" %in% names(confirmed_data)) || nrow(confirmed_data) == 0) {
    counts <- base %>% mutate(n_confirmed = 0L)
  } else {
    counts <- confirmed_data %>%
      count(occupation_group, name = "n_confirmed") %>%
      right_join(base, by = "occupation_group") %>%
      mutate(n_confirmed = replace_na(n_confirmed, 0L))
  }
  
  counts %>%
    mutate(
      remaining_needed = pmax(target_n - n_confirmed, 0),
      occ_boost = case_when(
        remaining_needed >= 6 ~ OCC_BOOST_WEIGHTS$very_far_behind,
        remaining_needed >= 4 ~ OCC_BOOST_WEIGHTS$far_behind,
        remaining_needed >= 2 ~ OCC_BOOST_WEIGHTS$behind,
        remaining_needed >= 1 ~ OCC_BOOST_WEIGHTS$slightly_behind,
        TRUE                  ~ OCC_BOOST_WEIGHTS$met_or_over
      )
    )
}

get_gender_status <- function(confirmed_data,
                              target_total = TARGET_TOTAL_N,
                              male_floor = OVERALL_TARGETS$male_floor) {
  male_target_n <- ceiling(target_total * male_floor)
  
  male_confirmed <- 0L
  female_confirmed <- 0L
  
  if ("gender" %in% names(confirmed_data) && nrow(confirmed_data) > 0) {
    male_confirmed <- sum(confirmed_data$gender == "Male", na.rm = TRUE)
    female_confirmed <- sum(confirmed_data$gender == "Female", na.rm = TRUE)
  }
  
  male_gap_n <- male_target_n - male_confirmed
  
  male_boost <- case_when(
    male_gap_n >= 6 ~ MALE_FLOOR_WEIGHTS$large_shortfall,
    male_gap_n >= 3 ~ MALE_FLOOR_WEIGHTS$shortfall,
    male_gap_n >= 1 ~ MALE_FLOOR_WEIGHTS$slight_shortfall,
    TRUE            ~ MALE_FLOOR_WEIGHTS$on_target
  )
  
  tibble(
    gender = c("Male", "Female"),
    target_n = c(male_target_n, NA_real_),
    n_confirmed = c(male_confirmed, female_confirmed),
    gap_n = c(male_gap_n, NA_real_),
    gender_boost = c(male_boost, MALE_FLOOR_WEIGHTS$female_default)
  )
}

# ------------------------------------------------------------------------------
# Occupation diversity caps
# ------------------------------------------------------------------------------

apply_wave_caps <- function(selection_pool, current_wave_selected, wave_total) {
  if (nrow(current_wave_selected) == 0) return(selection_pool)
  
  wave_occ_counts <- current_wave_selected %>% count(occupation_group, name = "wave_n")
  
  selection_pool %>%
    left_join(wave_occ_counts, by = "occupation_group") %>%
    mutate(
      wave_n       = replace_na(wave_n, 0L),
      wave_cap     = pmax(1, floor(wave_total * MAX_SHARE_PER_OCCUPATION_IN_WAVE)),
      slp_wave_cap = pmax(1, floor(wave_total * MAX_SHARE_SLP_IN_WAVE)),
      weight = case_when(
        occupation_group == "SLP" & wave_n >= slp_wave_cap ~ weight * 0.05,
        wave_n >= wave_cap                                 ~ weight * 0.10,
        TRUE                                               ~ weight
      )
    ) %>%
    select(-wave_n, -wave_cap, -slp_wave_cap)
}

apply_group_caps <- function(selection_pool, current_group, group_size) {
  if (nrow(current_group) == 0) return(selection_pool)
  
  group_occ_counts <- current_group %>% count(occupation_group, name = "group_n")
  
  selection_pool %>%
    left_join(group_occ_counts, by = "occupation_group") %>%
    mutate(
      group_n       = replace_na(group_n, 0L),
      group_cap     = pmax(1, floor(group_size * MAX_SHARE_PER_OCCUPATION_IN_GROUP)),
      slp_group_cap = pmax(1, floor(group_size * MAX_SHARE_SLP_IN_GROUP)),
      weight = case_when(
        occupation_group == "SLP" & group_n >= slp_group_cap ~ weight * 0.05,
        group_n >= group_cap                                 ~ weight * 0.10,
        TRUE                                                 ~ weight
      )
    ) %>%
    select(-group_n, -group_cap, -slp_group_cap)
}

# ------------------------------------------------------------------------------
# Group scoring
# Occupation strongest, light preference for male presence
# ------------------------------------------------------------------------------

score_group <- function(df, group_size) {
  if (nrow(df) == 0) return(-Inf)
  
  occ_diversity <- n_distinct(df$occupation_group)
  minority_n    <- sum(df$is_non_white, na.rm = TRUE)
  rural_n       <- sum(df$urban_rural == "rural", na.rm = TRUE)
  male_n        <- sum(df$gender == "Male", na.rm = TRUE)
  regions_n     <- n_distinct(df$region)
  
  score <- 0
  score <- score + occ_diversity * 1.80
  if (minority_n >= 2) score <- score + 1.50
  if (minority_n >= 3) score <- score + 0.75
  if (male_n >= 1)     score <- score + 0.60
  if (male_n >= 2)     score <- score + 0.20
  if (rural_n >= 1)    score <- score + 0.75
  if (regions_n >= 2)  score <- score + 0.50
  
  score
}

# ==============================================================================
# SECTION 3: LOAD MASTER / DETERMINE CURRENT WAVE
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("   PROFESSIONAL-ONLY RECRUITMENT\n")
cat(strrep("=", 80), "\n\n")

if (file.exists(MASTER_FILE)) {
  master_prior <- as_tibble(read.csv(MASTER_FILE, stringsAsFactors = FALSE))
  
  if ("participation_status" %in% names(master_prior)) {
    master_prior$participation_status <- tolower(trimws(as.character(master_prior$participation_status)))
    master_prior$participation_status[is.na(master_prior$participation_status) |
                                        master_prior$participation_status == ""] <- "selected"
  } else {
    master_prior$participation_status <- "selected"
  }
  
  if (!("wave_selected" %in% names(master_prior))) {
    master_prior$wave_selected <- 1L
  }
} else {
  master_prior <- tibble()
}

if (nrow(master_prior) > 0) {
  CURRENT_WAVE   <- max(master_prior$wave_selected, na.rm = TRUE) + 1L
  confirmed_data <- master_prior %>%
    filter(participation_status %in% c("completed", "confirmed"))
  frozen_ids     <- master_prior %>%
    filter(!(participation_status %in% c("released"))) %>%
    pull(record_id)
} else {
  CURRENT_WAVE   <- 1L
  confirmed_data <- tibble()
  frozen_ids     <- character(0)
}

if (nrow(confirmed_data) > 0) {
  if (!("occupation_group" %in% names(confirmed_data)) && "occupation" %in% names(confirmed_data)) {
    confirmed_data <- confirmed_data %>%
      mutate(occupation_group = assign_occupation_group(occupation))
  }
  
  if (!("is_non_white" %in% names(confirmed_data))) {
    confirmed_data <- confirmed_data %>%
      mutate(is_non_white = (race != "White") | (ethnicity == "Hispanic"))
  }
  
  if (!("is_age_50_plus" %in% names(confirmed_data)) && "age" %in% names(confirmed_data)) {
    confirmed_data <- confirmed_data %>%
      mutate(is_age_50_plus = suppressWarnings(as.numeric(age)) >= 50)
  }
}

n_confirmed <- nrow(confirmed_data)

cat(sprintf("Starting WAVE %d\n", CURRENT_WAVE))
cat(sprintf("Currently CONFIRMED: %d of %d\n", n_confirmed, TARGET_TOTAL_N))
cat(sprintf("Currently FROZEN (in pipeline): %d\n\n", length(frozen_ids)))

if (n_confirmed >= TARGET_TOTAL_N) {
  cat(strrep("=", 80), "\n")
  cat(sprintf("TARGET REACHED: %d confirmed participants. Nothing to do.\n", n_confirmed))
  cat(strrep("=", 80), "\n")
  stop("Target reached.", call. = FALSE)
}

CURRENT_WAVE_SPEC <- determine_wave_spec(n_confirmed, CURRENT_WAVE, TARGET_TOTAL_N)

cat("THIS WAVE\n")
cat(strrep("-", 40), "\n")
cat(sprintf("  Remaining to target:  %d\n", CURRENT_WAVE_SPEC$remaining_to_target))
cat(sprintf("  Wave total:           %d\n", CURRENT_WAVE_SPEC$total))
cat(sprintf("  Group sizes:          %s\n", paste(CURRENT_WAVE_SPEC$group_sizes, collapse = ", ")))
cat(sprintf("  Focus groups:         %s\n", paste(CURRENT_WAVE_SPEC$groups, collapse = ", ")))
cat(sprintf("  Min eligible needed:  %d\n\n", CURRENT_WAVE_SPEC$min_eligible_needed))

set.seed(12121 + CURRENT_WAVE * 100)

# ==============================================================================
# SECTION 4: LOAD SOURCE DATA AND MAP FIELDS
# ==============================================================================

cat("LOADING DATA\n")
cat(strrep("-", 40), "\n")

raw_data <- read.csv(SOURCE_FILE, stringsAsFactors = FALSE)
cat(sprintf("Total records in database: %d\n", nrow(raw_data)))

FIELD_NAMES <- list()
FIELD_NAMES$id                <- resolve_field_name(raw_data, "record_id",         c("Record ID", "recordid", "id", "participant_id"))
FIELD_NAMES$first_name        <- resolve_field_name(raw_data, "first_name",        c("firstname", "first", "First Name"))
FIELD_NAMES$last_name         <- resolve_field_name(raw_data, "last_name",         c("lastname", "last", "Last Name"))
FIELD_NAMES$email             <- resolve_optional_field_name(raw_data, "email",    c("email_address", "Email"))
FIELD_NAMES$phone             <- resolve_optional_field_name(raw_data, "phone",    c("phone_number", "telephone", "Phone Number"))
FIELD_NAMES$age               <- resolve_field_name(raw_data, "age",               c("age_years", "Age"))
FIELD_NAMES$sex               <- resolve_field_name(raw_data, "gender",            c("sex", "gender_identity", "Gender"))
FIELD_NAMES$race              <- resolve_field_name(raw_data, "race",              c("race_ethnicity_race", "racial_identity", "Race"))
FIELD_NAMES$ethnicity         <- resolve_field_name(raw_data, "ethnicity",         c("hispanic", "ethnic_group", "Ethnicity"))
FIELD_NAMES$state             <- resolve_field_name(raw_data, "state",             c("state_code", "us_state", "State"))
FIELD_NAMES$city              <- resolve_optional_field_name(raw_data, "city",     c("town", "City"))
FIELD_NAMES$education         <- resolve_optional_field_name(raw_data, "education_years", c("education", "years_education"))
FIELD_NAMES$occupation        <- resolve_field_name(raw_data, "occupation",        c("job_title", "job", "profession", "Occupation"))
FIELD_NAMES$stakeholder_group <- resolve_field_name(raw_data, "stakeholder_group", c("stakeholder", "Stakeholder Group"))
FIELD_NAMES$ruca              <- resolve_optional_field_name(raw_data, "ruca",     c("RUCA", "ruca_code", "Urban"))

optional_fields <- c("email", "phone", "city", "education", "ruca")
for (nm in optional_fields) {
  col_name <- FIELD_NAMES[[nm]]
  if (is.na(col_name) || !(col_name %in% names(raw_data))) {
    placeholder <- paste0("__missing_", nm)
    raw_data[[placeholder]] <- NA
    FIELD_NAMES[[nm]] <- placeholder
  }
}

available_data <- raw_data %>%
  filter(!(.data[[FIELD_NAMES$id]] %in% frozen_ids))

cat(sprintf("Available for selection (after frozen removed): %d\n\n", nrow(available_data)))

# ==============================================================================
# SECTION 5: APPLY ELIGIBILITY CRITERIA
# ==============================================================================

cat("APPLYING ELIGIBILITY CRITERIA\n")
cat(strrep("-", 40), "\n")

MED_HX_PREFIX  <- "med_hx___"
MED_EXCLUSIONS <- c("alzheimers", "ppa", "tbi", "parkinsons")

education_values <- suppressWarnings(as.numeric(available_data[[FIELD_NAMES$education]]))
age_values       <- suppressWarnings(as.numeric(available_data[[FIELD_NAMES$age]]))

invalid_education <- !is.na(education_values) & (
  education_values > 60 | education_values < 0 | education_values > 1900 |
    (education_values >= 100 & education_values <= 200)
)

invalid_age <- !is.na(age_values) & (
  age_values < 18 | age_values > 120 | age_values > 1900
)

exclusion_columns       <- paste0(MED_HX_PREFIX, MED_EXCLUSIONS)
existing_exclusion_cols <- exclusion_columns[exclusion_columns %in% names(available_data)]

has_medical_exclusion <- if (length(existing_exclusion_cols) > 0) {
  rowSums(available_data[existing_exclusion_cols], na.rm = TRUE) > 0
} else {
  rep(FALSE, nrow(available_data))
}

is_professional <- available_data[[FIELD_NAMES$stakeholder_group]] %in% c(3, 4)

eligible_data <- available_data %>%
  filter(
    !invalid_education,
    !invalid_age,
    !has_medical_exclusion,
    !is.na(.data[[FIELD_NAMES$age]]),
    is_professional
  )

cat(sprintf("  Invalid education:  %d\n", sum(invalid_education)))
cat(sprintf("  Invalid age:        %d\n", sum(invalid_age)))
cat(sprintf("  Medical exclusions: %d\n", sum(has_medical_exclusion)))
cat(sprintf("  Not professional:   %d\n", sum(!is_professional)))
cat(sprintf("  ELIGIBLE:           %d\n\n", nrow(eligible_data)))

if (nrow(eligible_data) < CURRENT_WAVE_SPEC$min_eligible_needed) {
  stop(sprintf(
    "Only %d eligible professionals available; need at least %d for wave %d.",
    nrow(eligible_data), CURRENT_WAVE_SPEC$min_eligible_needed, CURRENT_WAVE
  ))
}

# ==============================================================================
# SECTION 6: DERIVE DEMOGRAPHICS
# ==============================================================================

cat("CALCULATING DEMOGRAPHICS\n")
cat(strrep("-", 40), "\n")

STATE_CODES <- c(
  "1" = "AL", "2" = "AK", "3" = "AZ", "4" = "AR", "5" = "CA", "6" = "CO", "7" = "CT",
  "8" = "DC", "9" = "DE", "10" = "FL", "11" = "GA", "12" = "HI", "13" = "ID", "14" = "IL",
  "15" = "IN", "16" = "IA", "17" = "KS", "18" = "KY", "19" = "LA", "20" = "ME",
  "21" = "MD", "22" = "MA", "23" = "MI", "24" = "MN", "25" = "MS", "26" = "MO",
  "27" = "MT", "28" = "NE", "29" = "NV", "30" = "NH", "31" = "NJ", "32" = "NM",
  "33" = "NY", "34" = "NC", "35" = "ND", "36" = "OH", "37" = "OK", "38" = "OR",
  "39" = "PA", "40" = "RI", "41" = "SC", "42" = "SD", "43" = "TN", "44" = "TX",
  "45" = "UT", "46" = "VT", "47" = "VA", "48" = "WA", "49" = "WV", "50" = "WI", "51" = "WY"
)

demo_data <- eligible_data %>%
  mutate(
    record_id         = .data[[FIELD_NAMES$id]],
    first_name        = .data[[FIELD_NAMES$first_name]],
    last_name         = .data[[FIELD_NAMES$last_name]],
    email             = .data[[FIELD_NAMES$email]],
    phone             = .data[[FIELD_NAMES$phone]],
    city              = .data[[FIELD_NAMES$city]],
    age               = suppressWarnings(as.numeric(.data[[FIELD_NAMES$age]])),
    is_age_50_plus    = age >= 50,
    
    occupation        = .data[[FIELD_NAMES$occupation]],
    occupation_group  = assign_occupation_group(occupation),
    
    stakeholder_group = .data[[FIELD_NAMES$stakeholder_group]],
    
    state_raw = as.character(.data[[FIELD_NAMES$state]]),
    state     = STATE_CODES[state_raw],
    state     = ifelse(is.na(state), toupper(state_raw), state),
    region    = assign_region_from_state(state),
    
    race_raw = .data[[FIELD_NAMES$race]],
    race = case_when(
      race_raw %in% c("1", 1) ~ "White",
      race_raw %in% c("2", 2) ~ "Black",
      race_raw %in% c("3", 3) ~ "Asian",
      race_raw %in% c("4", 4) ~ "Native American",
      race_raw %in% c("5", 5) ~ "Pacific Islander",
      race_raw %in% c("6", 6) ~ "Multiracial",
      TRUE                    ~ "Other"
    ),
    
    ethnicity_raw = .data[[FIELD_NAMES$ethnicity]],
    ethnicity = case_when(
      ethnicity_raw %in% c(1, "1") ~ "Hispanic",
      ethnicity_raw %in% c(0, "0") ~ "Non-Hispanic",
      TRUE                         ~ "Unknown"
    ),
    is_hispanic  = ethnicity == "Hispanic",
    is_non_white = (race != "White") | is_hispanic,
    
    sex_raw = .data[[FIELD_NAMES$sex]],
    gender = case_when(
      sex_raw %in% c("1", 1) ~ "Male",
      sex_raw %in% c("2", 2) ~ "Female",
      sex_raw %in% c("3", 3) ~ "Other",
      TRUE                   ~ "Unknown"
    ),
    
    ruca_code   = suppressWarnings(as.numeric(.data[[FIELD_NAMES$ruca]])),
    urban_rural = case_when(
      !is.na(ruca_code) & ruca_code <= 3 ~ "urban",
      !is.na(ruca_code) & ruca_code > 3  ~ "rural",
      TRUE                               ~ "unknown"
    )
  )

cat("Occupation groups in eligible pool:\n")
occ_pool <- demo_data %>% count(occupation_group, sort = TRUE, name = "n")
for (i in seq_len(nrow(occ_pool))) {
  cat(sprintf("  %-28s %d\n", occ_pool$occupation_group[i], occ_pool$n[i]))
}
cat("\n")

cat("Eligible pool by occupation x gender:\n")
demo_data %>%
  count(occupation_group, gender, name = "n") %>%
  arrange(occupation_group, desc(n)) %>%
  print(na.print = "NA")
cat("\n")

# ==============================================================================
# SECTION 7: BUILD DYNAMIC WEIGHTS
# ==============================================================================

cat("CALCULATING WEIGHTS\n")
cat(strrep("-", 40), "\n")

occ_status <- get_occ_status(confirmed_data, OCCUPATION_TARGETS)
gender_status <- get_gender_status(confirmed_data, TARGET_TOTAL_N, OVERALL_TARGETS$male_floor)

cat("Occupation progress (confirmed / target):\n")
for (i in seq_len(nrow(occ_status))) {
  cat(sprintf("  %-28s %2d / %2d  (remaining %2d, boost %.2f)\n",
              occ_status$occupation_group[i],
              occ_status$n_confirmed[i],
              occ_status$target_n[i],
              occ_status$remaining_needed[i],
              occ_status$occ_boost[i]))
}
cat("\n")

cat("Gender progress:\n")
cat(sprintf("  Male confirmed:   %d / %d target floor (gap %+d, boost %.2f)\n",
            gender_status$n_confirmed[gender_status$gender == "Male"],
            gender_status$target_n[gender_status$gender == "Male"],
            gender_status$gap_n[gender_status$gender == "Male"],
            gender_status$gender_boost[gender_status$gender == "Male"]))
cat(sprintf("  Female confirmed: %d\n",
            gender_status$n_confirmed[gender_status$gender == "Female"]))
cat("\n")

demo_data <- demo_data %>%
  left_join(
    occ_status %>%
      select(occupation_group, target_n, n_confirmed, remaining_needed, occ_boost),
    by = "occupation_group"
  ) %>%
  left_join(
    gender_status %>%
      select(gender,
             gender_target_n = target_n,
             gender_confirmed_n = n_confirmed,
             gender_gap_n = gap_n,
             gender_boost),
    by = "gender"
  ) %>%
  mutate(
    race_weight = case_when(
      race == "Black"                ~ RACE_WEIGHTS$black,
      race == "Asian"                ~ RACE_WEIGHTS$asian,
      is_hispanic                    ~ RACE_WEIGHTS$hispanic,
      race == "Native American"      ~ RACE_WEIGHTS$native_american,
      race == "Pacific Islander"     ~ RACE_WEIGHTS$pacific_islander,
      race == "Multiracial"          ~ RACE_WEIGHTS$multiracial,
      race == "White" & !is_hispanic ~ RACE_WEIGHTS$white_non_hispanic,
      TRUE                           ~ RACE_WEIGHTS$other
    ),
    gender_weight = case_when(
      gender == "Male"   ~ coalesce(gender_boost, 1.00),
      gender == "Female" ~ MALE_FLOOR_WEIGHTS$female_default,
      gender == "Other"  ~ MALE_FLOOR_WEIGHTS$other,
      TRUE               ~ MALE_FLOOR_WEIGHTS$unknown
    ),
    urban_weight = case_when(
      urban_rural == "rural" ~ URBAN_WEIGHTS$rural,
      urban_rural == "urban" ~ URBAN_WEIGHTS$urban,
      TRUE                   ~ URBAN_WEIGHTS$unknown
    ),
    age_weight = case_when(
      age >= 65 ~ AGE_WEIGHTS$age_65_plus,
      age >= 50 ~ AGE_WEIGHTS$age_50_64,
      TRUE      ~ AGE_WEIGHTS$age_under_50
    ),
    region_weight = case_when(
      region == "Southeast" ~ REGION_WEIGHTS$southeast,
      region == "Northeast" ~ REGION_WEIGHTS$northeast,
      region == "Midwest"   ~ REGION_WEIGHTS$midwest,
      region == "West"      ~ REGION_WEIGHTS$west,
      TRUE                  ~ REGION_WEIGHTS$other
    ),
    weight = occ_boost * gender_weight * race_weight * urban_weight * age_weight * region_weight
  )

# Only keep one secondary adjustment layer: minority / rural / age
if (nrow(confirmed_data) > 0 &&
    all(c("urban_rural", "gender") %in% names(confirmed_data))) {
  
  prev_stats <- confirmed_data %>%
    summarise(
      pct_rural       = mean(urban_rural == "rural", na.rm = TRUE),
      pct_minority    = mean(is_non_white, na.rm = TRUE),
      pct_age_50_plus = mean(is_age_50_plus, na.rm = TRUE),
      pct_male        = mean(gender == "Male", na.rm = TRUE)
    )
  
  cat("Adjusting weights based on confirmed-pool shortfalls:\n")
  did_any <- FALSE
  
  if (!is.na(prev_stats$pct_minority) && prev_stats$pct_minority < OVERALL_TARGETS$min_minority_pct) {
    demo_data <- demo_data %>%
      mutate(weight = ifelse(is_non_white, weight * 1.50, weight))
    cat(sprintf("  -> Boosting minorities (currently %.1f%%)\n", prev_stats$pct_minority * 100))
    did_any <- TRUE
  }
  
  if (!is.na(prev_stats$pct_rural) && prev_stats$pct_rural < OVERALL_TARGETS$min_rural_pct) {
    demo_data <- demo_data %>%
      mutate(weight = ifelse(urban_rural == "rural", weight * 1.25, weight))
    cat(sprintf("  -> Boosting rural (currently %.1f%%)\n", prev_stats$pct_rural * 100))
    did_any <- TRUE
  }
  
  if (!is.na(prev_stats$pct_age_50_plus) && prev_stats$pct_age_50_plus < OVERALL_TARGETS$min_age_50_plus_pct) {
    demo_data <- demo_data %>%
      mutate(weight = ifelse(is_age_50_plus, weight * 1.15, weight))
    cat(sprintf("  -> Boosting age 50+ (currently %.1f%%)\n", prev_stats$pct_age_50_plus * 100))
    did_any <- TRUE
  }
  
  if (!did_any) cat("  (all confirmed-pool shortfalls within tolerance)\n")
  cat("\n")
}

cat(sprintf("Ready to select from %d eligible participants.\n\n", nrow(demo_data)))

cat("Top weighted candidates:\n")
demo_data %>%
  select(record_id, age, occupation_group, gender, occ_boost, gender_weight, weight) %>%
  arrange(desc(weight)) %>%
  head(25) %>%
  print(n = 25, na.print = "NA")
cat("\n")

# ==============================================================================
# SECTION 8: SELECT PARTICIPANTS
# Weighted random sampling + best-of-many candidate groups
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("   SELECTING PARTICIPANTS\n")
cat(strrep("=", 80), "\n\n")

wave_selected  <- tibble()
pool_remaining <- demo_data

for (i in seq_along(CURRENT_WAVE_SPEC$groups)) {
  group_num  <- CURRENT_WAVE_SPEC$groups[i]
  group_size <- CURRENT_WAVE_SPEC$group_sizes[i]
  
  cat(sprintf("Selecting Group %d (target n=%d)...\n", group_num, group_size))
  
  best_group <- NULL
  best_score <- -Inf
  
  for (attempt in 1:300) {
    if (nrow(pool_remaining) < group_size) {
      cat("  Not enough participants remaining!\n")
      break
    }
    
    candidate_group <- tibble()
    temp_pool <- pool_remaining
    
    while (nrow(candidate_group) < group_size && nrow(temp_pool) > 0) {
      selection_pool <- temp_pool %>%
        apply_wave_caps(wave_selected, CURRENT_WAVE_SPEC$total) %>%
        apply_group_caps(candidate_group, group_size)
      
      idx <- sample(
        seq_len(nrow(selection_pool)),
        size = 1,
        prob = safe_prob(selection_pool$weight)
      )
      
      selected_id <- selection_pool$record_id[idx]
      selected_person <- selection_pool %>% filter(record_id == selected_id)
      
      candidate_group <- bind_rows(candidate_group, selected_person)
      temp_pool <- temp_pool %>% filter(record_id != selected_id)
    }
    
    if (nrow(candidate_group) < group_size) next
    
    s <- score_group(candidate_group, group_size)
    if (s > best_score) {
      best_group <- candidate_group
      best_score <- s
      if (best_score >= 6) break
    }
  }
  
  if (!is.null(best_group) && nrow(best_group) > 0) {
    best_group <- best_group %>%
      mutate(
        focus_group   = group_num,
        wave_selected = CURRENT_WAVE
      )
    
    cat(sprintf(
      "  -> %d selected | minorities %d | rural %d | men %d | occ diversity %d\n",
      nrow(best_group),
      sum(best_group$is_non_white, na.rm = TRUE),
      sum(best_group$urban_rural == "rural", na.rm = TRUE),
      sum(best_group$gender == "Male", na.rm = TRUE),
      n_distinct(best_group$occupation_group)
    ))
    cat(sprintf("  Diversity score: %.2f\n", best_score))
    
    wave_selected <- bind_rows(wave_selected, best_group)
    pool_remaining <- pool_remaining %>% filter(!(record_id %in% best_group$record_id))
  }
}
cat("\n")

# ==============================================================================
# SECTION 9: WAVE SUMMARY
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("   WAVE SUMMARY\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("Participants selected this wave: %d\n", nrow(wave_selected)))
cat(sprintf("  Minorities:     %d (%.1f%%)\n",
            sum(wave_selected$is_non_white, na.rm = TRUE),
            mean(wave_selected$is_non_white, na.rm = TRUE) * 100))
cat(sprintf("  Men/Women:      %d/%d\n",
            sum(wave_selected$gender == "Male", na.rm = TRUE),
            sum(wave_selected$gender == "Female", na.rm = TRUE)))
cat(sprintf("  Rural:          %d (%.1f%%)\n",
            sum(wave_selected$urban_rural == "rural", na.rm = TRUE),
            mean(wave_selected$urban_rural == "rural", na.rm = TRUE) * 100))
cat(sprintf("  Age 50+:        %d (%.1f%%)\n",
            sum(wave_selected$is_age_50_plus, na.rm = TRUE),
            mean(wave_selected$is_age_50_plus, na.rm = TRUE) * 100))

cat("\nOccupation breakdown this wave:\n")
occ_wave <- wave_selected %>% count(occupation_group, sort = TRUE, name = "n")
for (i in seq_len(nrow(occ_wave))) {
  cat(sprintf("  %-28s %d\n", occ_wave$occupation_group[i], occ_wave$n[i]))
}
cat("\n")

# ==============================================================================
# SECTION 10: UPDATE MASTER FILE
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("   UPDATING MASTER FILE\n")
cat(strrep("=", 80), "\n\n")

MASTER_COLS <- c(
  "wave_selected", "record_id",
  "first_name", "last_name",
  "email", "phone", "date_of_birth",
  "age",
  "race", "ethnicity", "gender", "address", "state", "zip_code", "urban_rural", "region", 
  "occupation", "occupation_group",
  "participation_status", "notes"
)

new_wave_rows <- wave_selected %>%
  mutate(
    participation_status = "selected",
    notes                = ""
  ) %>%
  transmute(
    wave_selected, focus_group, record_id,
    first_name, last_name,
    email, phone, date_of_birth,
    age,
    race, ethnicity, gender,
    address, state, zip_code, region, urban_rural,
    occupation, occupation_group,
    participation_status,
    notes
  )

if (nrow(master_prior) > 0) {
  for (col in MASTER_COLS) {
    if (!(col %in% names(master_prior))) master_prior[[col]] <- NA
  }
  
  master_prior_clean <- master_prior %>% select(all_of(MASTER_COLS))
  
  master_out <- bind_rows(master_prior_clean, new_wave_rows) %>%
    distinct(record_id, .keep_all = TRUE) %>%
    arrange(wave_selected, occupation_group, last_name, first_name)
} else {
  master_out <- new_wave_rows %>%
    arrange(wave_selected, occupation_group, last_name, first_name)
}

write.csv(master_out, MASTER_FILE, row.names = FALSE)
cat(sprintf("Updated: %s (%d total rows)\n\n", MASTER_FILE, nrow(master_out)))

# ==============================================================================
# SECTION 11: CUMULATIVE PROGRESS
# ==============================================================================

cat(strrep("=", 80), "\n")
cat("   CUMULATIVE PROGRESS TOWARD TARGET\n")
cat(strrep("=", 80), "\n\n")

cat(sprintf("Confirmed participants:  %d of %d (%.1f%%)\n",
            nrow(confirmed_data), TARGET_TOTAL_N,
            nrow(confirmed_data) / TARGET_TOTAL_N * 100))
cat(sprintf("Total ever selected:     %d\n", nrow(master_out)))
cat(sprintf("In pipeline (not yet confirmed): %d\n",
            nrow(master_out) - nrow(confirmed_data)))

if (nrow(confirmed_data) > 0) {
  cat("\nConfirmed occupation progress:\n")
  for (i in seq_len(nrow(occ_status))) {
    cat(sprintf("  %-28s %2d / %2d\n",
                occ_status$occupation_group[i],
                occ_status$n_confirmed[i],
                occ_status$target_n[i]))
  }
  
  cat("\nConfirmed gender progress:\n")
  cat(sprintf("  Men:   %d / %d target floor\n",
              gender_status$n_confirmed[gender_status$gender == "Male"],
              gender_status$target_n[gender_status$gender == "Male"]))
  cat(sprintf("  Women: %d\n",
              gender_status$n_confirmed[gender_status$gender == "Female"]))
}

# ==============================================================================
# SECTION 12: FINAL MESSAGE
# ==============================================================================

cat("\n")
cat(strrep("=", 80), "\n")
cat(sprintf("   WAVE %d COMPLETE\n", CURRENT_WAVE))
cat(strrep("=", 80), "\n\n")

remaining_after_this_wave <- TARGET_TOTAL_N - nrow(confirmed_data)

if (remaining_after_this_wave <= 0) {
  cat("ALL 50 CONFIRMED - no further waves needed.\n")
} else {
  cat(sprintf("Still needed (confirmations): up to %d more.\n", remaining_after_this_wave))
  cat("\nNEXT STEPS:\n")
  cat(sprintf("  1. Open %s\n", MASTER_FILE))
  cat("  2. For people you've contacted, set participation_status to one of:\n")
  cat("       completed, confirmed, declined, no_show, released\n")
  cat("     Add anything useful in the notes column.\n")
  cat("  3. Save the file and re-run this script to launch the next wave.\n")
}

cat(strrep("=", 80), "\n")

