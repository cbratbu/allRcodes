#' ============================================================
#' Configuration: libraries, item map, group + domain metadata
#' ============================================================
#'
#' Three-group Core Impact Set pipeline
#'   Groups : People with Aphasia (PWA), Family/Caregivers (FMC),
#'            Clinicians/Researchers (CR)
#'   Items  : A curated 40-item subset of the full instrument (see
#'            ITEM_MAP_40 below). Every item maps 1:1 onto a label that
#'            already existed in the original 9-domain instrument, so
#'            domain structure, palette, and column-prefix conventions
#'            are reused unchanged from the original pipeline.
#'
#' ── IMPORTANT: column names ────────────────────────────────
#' The `item` column in ITEM_MAP_40 below is confirmed against the actual
#' REDCap export (PCORIDatabase-Convening3_DATA_*.csv): 40 item columns
#' named with domain prefixes + "_v2" suffix, e.g. "s_reading_v2",
#' "fe_confidence_v2", "costs_job_v2". If a future export drops or
#' changes the "_v2" suffix, update ITEM_MAP_40 accordingly — everything
#' downstream keys off this map, so nothing else needs to change.

# ── Libraries ──────────────────────────────────────────────
library(tidyverse)
library(janitor)
library(ggplot2)
library(viridis)
library(Hmisc)
library(dunn.test)

# ── Color palette (Okabe-Ito, colorblind-safe) — domains ───
OKABE_ITO <- c(
  symptom       = "#000000",
  costs         = "#E69F00",
  emotions      = "#56B4E9",
  social        = "#009E73",
  cognition     = "#F0E442",
  treatment     = "#0072B2",
  qol           = "#D55E00",
  resources     = "#CC79A7",
  impact_others = "#999999"
)

# ── Stakeholder groups ──────────────────────────────────────
GROUP_LEVELS <- c("PWA", "FMC", "CR")

GROUP_LABELS <- c(
  PWA = "People with Aphasia",
  FMC = "Family/Caregivers",
  CR  = "Clinicians/Researchers"
)

# Distinct from the domain palette (used on different plot types)
GROUP_PALETTE <- c(
  PWA = "#D55E00",
  FMC = "#0072B2",
  CR  = "#009E73"
)

#' Normalize free-text / variably-coded group values onto GROUP_LEVELS.
#'
#' Matches the REDCap "stakeholder_group" field (instrument.csv, "history"
#' form): 1 = Person with aphasia, 2 = Family member/caregiver,
#' 3 = Clinician, 4 = Researcher. Clinicians (3) and researchers (4) are
#' merged into the single CR group used throughout this pipeline. Also
#' accepts common text spellings as a fallback.
#'
#' @param x Character or factor vector of raw group values.
#' @return Factor with levels GROUP_LEVELS (NA for unrecognized values).
normalize_group <- function(x) {
  x_chr <- toupper(trimws(as.character(x)))
  
  out <- case_when(
    x_chr %in% c("1", "PWA", "PERSON WITH APHASIA", "PEOPLE WITH APHASIA") ~ "PWA",
    x_chr %in% c("2", "FMC", "FAMILY", "FRIEND", "CAREGIVER", "CARE PARTNER",
                 "FAMILY MEMBER / CAREGIVER OF PERSON WITH APHASIA",
                 "FAMILY/CAREGIVER", "FAMILY MEMBER/CARE PARTNER") ~ "FMC",
    x_chr %in% c("3", "4", "CR", "CLINICIAN", "RESEARCHER",
                 "CLINICIAN/RESEARCHER", "CLINICIAN RESEARCHER") ~ "CR",
    TRUE ~ NA_character_
  )
  
  factor(out, levels = GROUP_LEVELS)
}

# ── State code lookup (REDCap "state" field is numeric 1-51, not an
#    abbreviation) — from instrument.csv's data dictionary choices ────
STATE_CODE_LOOKUP <- c(
  `1` = "AL", `2` = "AK", `3` = "AZ", `4` = "AR", `5` = "CA", `6` = "CO",
  `7` = "CT", `8` = "DC", `9` = "DE", `10` = "FL", `11` = "GA", `12` = "HI",
  `13` = "ID", `14` = "IL", `15` = "IN", `16` = "IA", `17` = "KS", `18` = "KY",
  `19` = "LA", `20` = "ME", `21` = "MD", `22` = "MA", `23` = "MI", `24` = "MN",
  `25` = "MS", `26` = "MO", `27` = "MT", `28` = "NE", `29` = "NV", `30` = "NH",
  `31` = "NJ", `32` = "NM", `33` = "NY", `34` = "NC", `35` = "ND", `36` = "OH",
  `37` = "OK", `38` = "OR", `39` = "PA", `40` = "RI", `41` = "SC", `42` = "SD",
  `43` = "TN", `44` = "TX", `45` = "UT", `46` = "VT", `47` = "VA", `48` = "WA",
  `49` = "WV", `50` = "WI", `51` = "WY"
)

# ── The 40-item map (item = confirmed CSV column name) ─────
# Confirmed against PCORIDatabase-Convening3_DATA_2026-08-12_1451.csv and
# its _LABELS export (record-level field labels). Column names use the
# "_v2" suffix from that instrument version.
ITEM_MAP_40 <- tribble(
  ~item,                          ~item_label,                              ~domain,
  # -- symptom (5) --
  "s_reading_v2",                 "Reading",                                "symptom",
  "s_writing_v2",                 "Writing & spelling",                     "symptom",
  "s_speed_v2",                   "Speed of processing",                    "symptom",
  "s_numbers_v2",                 "Numbers & math",                         "symptom",
  "s_expressive_language_v2",     "Expressive language",                    "symptom",
  # -- costs (3) --
  "costs_stability_v2",           "Financial stability",                    "costs",
  "costs_therapy_v2",             "Healthcare & therapy costs",             "costs",
  "costs_job_v2",                 "Financial impact of job difficulties",   "costs",
  # -- emotions (8) --
  "fe_confidence_v2",             "Confidence",                             "emotions",
  "fe_frustration_v2",            "Frustration",                           "emotions",
  "fe_sadness_v2",                "Sadness",                                "emotions",
  "fe_stress_v2",                 "Stress",                                 "emotions",
  "fe_value_v2",                  "Importance / valued",                    "emotions",
  "fe_misunderstood_v2",          "Not feeling understood by others",       "emotions",
  "fe_excluded_v2",               "Excluded",                               "emotions",
  "fe_interruptions_v2",          "Interruptions",                          "emotions",
  # -- cognition (3) --
  "fc_loud_group_v2",             "Groups/loud environments",               "cognition",
  "fc_comm_fatigue_v2",           "Communication fatigue",                  "cognition",
  "fc_decision_advocate_v2",      "Decision making",                        "cognition",
  # -- social (7) --
  "fs_travel_v2",                 "Traveling",                              "social",
  "fs_participation_v2",          "Social participation",                   "social",
  "fs_isolation_v2",              "Isolation",                              "social",
  "fs_role_changes_v2",           "Role changes",                           "social",
  "fs_employment_v2",             "Employment",                             "social",
  "fs_complex_v2",                "Complex discussions",                    "social",
  "fs_maintain_friendships_v2",   "Maintaining friendships",                "social",
  # -- qol (5) --
  "qol_dreams_v2",                "Dreams for future",                      "qol",
  "qol_mindset_v2",               "Mindset",                                "qol",
  "qol_personal_growth_v2",       "Increased confidence / personal growth", "qol",
  "qol_future_v2",                "Worry about the future",                 "qol",
  "qol_identity_v2",              "Identity",                               "qol",
  # -- treatment (2) --
  "te_comm_healthcare_v2",        "Communication with healthcare providers","treatment",
  "te_medical_navigation_v2",     "Medical navigation",                     "treatment",
  # -- resources (2) --
  "r_strategies_v2",              "Communication strategies",               "resources",
  "r_insurance_v2",               "Insurance & disability",                 "resources",
  # -- impact_others (5) --
  "io_emotion_v2",                "Emotional impact on care partners",      "impact_others",
  "io_financial_v2",              "Financial impact on carepartners",       "impact_others",
  "io_strained_v2",               "Strained relationships",                 "impact_others",
  "io_social_v2",                 "Social impact on carepartners",          "impact_others",
  "io_exhuastion_v2",             "Exhaustion",                             "impact_others"
)

stopifnot(nrow(ITEM_MAP_40) == 40)

ITEM_LABELS_40 <- setNames(ITEM_MAP_40$item_label, ITEM_MAP_40$item)
ALL_ITEMS_40   <- ITEM_MAP_40$item

# Named list of column vectors per domain (for domain-mean calculations)
DOMAINS_40 <- split(ITEM_MAP_40$item, ITEM_MAP_40$domain)

format_domain_label <- function(x) {
  x %>%
    as.character() %>%
    str_remove("_mean$") %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    str_replace("^Qol$", "QoL") %>%
    str_replace("^Impact Others$", "Impact on Others")
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x
