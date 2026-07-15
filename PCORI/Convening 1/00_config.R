#' Configuration: libraries, domain definitions, item labels, metadata
#'
#' Source this file first — everything else depends on it.
#'
#' Handles two CSV naming conventions:
#'   Abbreviated: s_expressive_language, fe_frustration, io_emotion, …
#'   Numbered:    symptom1, emotions6, impact_others7, …

# ── Libraries ──────────────────────────────────────────────
library(tidyverse)
library(janitor)
library(ggplot2)
library(viridis)
library(VennDiagram)
library(Hmisc)
library(dunn.test)

# ── Color palette (Okabe-Ito, colorblind-safe) ────────────
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

# ── Ordered display labels per domain ──────────────────────
# Position within each vector must match the column order in the CSV.
DOMAIN_LABELS <- list(
  symptom = c(
    "Expressive language",
    "Reading",
    "Comprehension",
    "Writing & spelling",
    "Memory",
    "Numbers & math"
  ),
  costs = c(
    "Healthcare & therapy costs",
    "Financial impact of job difficulties",
    "Financial stability"
  ),
  cognition = c(
    "Groups/loud environments",
    "Technology",
    "Phone use",
    "Decision making",
    "Communication fatigue",
    "Driving",
    "Humor",
    "Media",
    "Focus"
  ),
  emotions = c(
    "Importance / valued",
    "Frustration",
    "Sadness",
    "Increased emotionality / sensitivity",
    "Not feeling understood by others",
    "Fear",
    "Boredom",
    "Interruptions",
    "Excluded",
    "Stress",
    "Confidence",
    "Grief"
  ),
  social = c(
    "Aphasia support groups / communities",
    "Employment",
    "Social participation",
    "Volunteering / advocacy / new purpose",
    "Maintaining friendships",
    "Role changes",
    "Isolation",
    "Public transit",
    "Traveling",
    "Complex discussions",
    "Making friends",
    "Dating",
    "Voting",
    "Public places"
  ),
  qol = c(
    "Deal with change",
    "Mindset",
    "Identity",
    "Dreams for future",
    "Faith & spirituality",
    "Worry about the future",
    "Increased confidence / personal growth",
    "Negative QoL",
    "Positive QoL"
  ),
  treatment = c(
    "Lack of awareness",
    "Communication with healthcare providers",
    "Involved in decisions",
    "Medical navigation"
  ),
  resources = c(
    "Communication strategies",
    "Home modification",
    "Independence",
    "Insurance & disability",
    "Moving",
    "Mental health services",
    "Additional therapy and hospitalization",
    "Residential facility"
  ),
  impact_others = c(
    "Emotional impact on carepartners",
    "Social impact on carepartners",
    "Strained relationships",
    "Parenting",
    "Financial impact on carepartners",
    "Intimacy / sex",
    "Exhaustion"
  )
)

# ── Domain detection (works with both naming conventions) ──
#' @param col_names Character vector of column names (post clean_names).
#' @return Named list of character vectors (column names per domain).
define_domains <- function(col_names) {
  patterns <- list(
    symptom       = "^(s_|symptom)",
    costs         = "^cost",
    emotions      = "^(fe_|emotion)",
    social        = "^(fs_|social)",
    cognition     = "^(fc_|cognition)",
    treatment     = "^(te_|treatment)",
    qol           = "^qol",
    resources     = "^(r_|resource)",
    impact_others = "^(io_|impact_other)"
  )

  domains <- lapply(patterns, function(pat) {
    cols <- grep(pat, col_names, value = TRUE)

    if (length(cols) == 0) {
      return(character(0))
    }

    # Preserve original CSV order for abbreviated names.
    # Only reorder when every matched column is clearly numbered.
    nums <- suppressWarnings(as.numeric(gsub("\\D+", "", cols)))
    if (all(!is.na(nums))) {
      cols <- cols[order(nums)]
    }

    cols
  })

  n_found <- sum(lengths(domains))
  if (n_found == 0) {
    warning(
      "define_domains: no survey columns matched any pattern. ",
      "Check that column names start with expected prefixes."
    )
  } else {
    message(
      "define_domains: found ", n_found, " survey columns across ",
      sum(lengths(domains) > 0), " domains."
    )
  }

  domains
}

# ── Build ITEM_LABELS from detected columns ────────────────
#' Maps actual column names → display labels using positional matching
#' within each domain.
#'
#' @param domains Output of define_domains().
#' @return Named character vector: names = column names, values = labels.
build_item_labels <- function(domains) {
  labels <- character(0)

  for (dom in names(domains)) {
    cols    <- domains[[dom]]
    display <- DOMAIN_LABELS[[dom]]

    if (length(cols) == 0) next

    if (length(cols) != length(display)) {
      warning(
        "Domain '", dom, "': found ", length(cols),
        " columns but expected ", length(display),
        ". Labels may be misaligned."
      )
    }

    n          <- min(length(cols), length(display))
    new_labels <- setNames(display[seq_len(n)], cols[seq_len(n)])

    # If more columns than labels, use column name as fallback
    if (length(cols) > length(display)) {
      extra      <- cols[(length(display) + 1):length(cols)]
      new_labels <- c(new_labels, setNames(extra, extra))
    }

    labels <- c(labels, new_labels)
  }

  labels
}

# ── Known vs New (environmental scan vs patient-generated) ─
#
# known_new      : "Known" = from environmental scan; "New" = novel item
# source_type    : For Known items — who documented it in the literature:
#                    "PR"      = peer-reviewed sources only
#                    "PG"      = patient-generated sources only
#                    "PR; PG"  = both
#                  For New items — all tagged "New – PG": novel impacts
#                  surfaced by focus groups (patient-generated source).
KNOWN_NEW_DF <- tribble(
  ~item_label,                                    ~known_new,  ~source_type,
  # ── Symptom ────────────────────────────────────────────────
  "Expressive language",                          "Known",     "PR; PG",
  "Reading",                                      "New",       "New – PG",
  "Comprehension",                                "Known",     "PR; PG",
  "Writing & spelling",                           "New",       "New – PG",
  "Memory",                                       "New",       "New – PG",
  "Numbers & math",                               "New",       "New – PG",
  # ── Cognition ──────────────────────────────────────────────
  "Groups/loud environments",                     "New",       "New – PG",
  "Technology",                                   "New",       "New – PG",
  "Phone use",                                    "New",       "New – PG",
  "Communication fatigue",                        "Known",     "PG",
  "Decision making",                              "New",       "New – PG",
  "Driving",                                      "Known",     "PR",
  "Humor",                                        "New",       "New – PG",
  "Media",                                        "Known",     "PR; PG",
  "Focus",                                        "New",       "New – PG",
  # ── Costs ──────────────────────────────────────────────────
  "Healthcare & therapy costs",                   "Known",     "PG",
  "Financial impact of job difficulties",         "Known",     "PG",
  "Financial stability",                          "New",       "New – PG",
  # ── Emotions ───────────────────────────────────────────────
  "Importance / valued",                          "New",       "New – PG",
  "Frustration",                                  "Known",     "PG",
  "Sadness",                                      "Known",     "PR",
  "Fear",                                         "Known",     "PR",
  "Increased emotionality / sensitivity",         "New",       "New – PG",
  "Not feeling understood by others",             "New",       "New – PG",
  "Boredom",                                      "New",       "New – PG",
  "Interruptions",                                "New",       "New – PG",
  "Excluded",                                     "New",       "New – PG",
  "Stress",                                       "Known",     "PR",
  "Confidence",                                   "New",       "New – PG",
  "Grief",                                        "Known",     "PG",
  # ── QoL ────────────────────────────────────────────────────
  "Deal with change",                             "New",       "New – PG",
  "Mindset",                                      "New",       "New – PG",
  "Identity",                                     "Known",     "PR; PG",
  "Dreams for future",                            "New",       "New – PG",
  "Faith & spirituality",                         "New",       "New – PG",
  "Worry about the future",                       "New",       "New – PG",
  "Increased confidence / personal growth",       "New",       "New – PG",
  "Negative QoL",                                 "Known",     "PG",
  "Positive QoL",                                 "New",       "New – PG",
  # ── Impact on others ───────────────────────────────────────
  "Emotional impact on carepartners",             "Known",     "PR",
  "Social impact on carepartners",                "Known",     "PR",
  "Strained relationships",                       "Known",     "PR",
  "Parenting",                                    "New",       "New – PG",
  "Exhaustion",                                   "New",       "New – PG",
  "Financial impact on carepartners",             "Known",     "PR",
  "Intimacy / sex",                               "New",       "New – PG",
  # ── Resources ──────────────────────────────────────────────
  "Communication strategies",                     "New",       "New – PG",
  "Home modification",                            "New",       "New – PG",
  "Independence",                                 "New",       "New – PG",
  "Insurance & disability",                       "New",       "New – PG",
  "Moving",                                       "New",       "New – PG",
  "Mental health services",                       "New",       "New – PG",
  "Additional therapy and hospitalization",       "Known",     "PR",
  "Residential facility",                         "Known",     "PG",
  # ── Social / role ──────────────────────────────────────────
  "Employment",                                   "Known",     "PG",
  "Aphasia support groups / communities",         "New",       "New – PG",
  "Volunteering / advocacy / new purpose",        "New",       "New – PG",
  "Social participation",                         "Known",     "PG",
  "Maintaining friendships",                      "Known",     "PR",
  "Role changes",                                 "Known",     "PR",
  "Isolation",                                    "Known",     "PG",
  "Public transit",                               "Known",     "PG",
  "Traveling",                                    "Known",     "PG",
  "Making friends",                               "Known",     "PG",
  "Public places",                                "New",       "New – PG",
  "Complex discussions",                          "New",       "New – PG",
  "Dating",                                       "New",       "New – PG",
  "Voting",                                       "Known",     "PG",
  # ── Treatment experiences ───────────────────────────────────
  "Lack of awareness",                            "New",       "New – PG",
  "Communication with healthcare providers",      "Known",     "PR",
  "Involved in decisions",                        "Known",     "PR; PG",
  "Medical navigation",                           "New",       "New – PG"
)

# Convenience: three-level source category used in 09_novel_vs_known.R
# "Known – PR", "Known – PG", "Known – PR; PG", "New – PG", "New – PG"
KNOWN_NEW_DF <- KNOWN_NEW_DF %>%
  mutate(
    source_category = case_when(
      known_new == "Known" ~ paste0("Known – ", source_type),
      TRUE                 ~ source_type          # already "New – PG" / "New – PG"
    )
  )
