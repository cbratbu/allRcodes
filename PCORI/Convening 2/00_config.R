#' Configuration: libraries, domain definitions, item labels, metadata
#'
#' Source this file first — everything else depends on it.
#'
#' ── Stakeholder group ──────────────────────────────────────
#' This pipeline analyses a SINGLE stakeholder group: Clinicians / Researchers.
#' Unlike the original PWA-vs-FMC pipeline there is no group split and no
#' between-group comparison. All scoring, ranking, and association analyses
#' run on the full respondent sample.
#'
#' Handles two CSV naming conventions:
#'   Abbreviated: s_expressive_language, fe_frustration, io_emotion, …
#'   Numbered:    symptom1, emotions6, impact_others7, …

# ── Libraries ──────────────────────────────────────────────
library(tidyverse)
library(janitor)
library(ggplot2)
library(viridis)
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
# Position within each vector must match the column order in the CSV
# (Clinicians___Researchers template). Updated item set: several new items
# added across every domain relative to the original PWA/FMC instrument.
DOMAIN_LABELS <- list(
  symptom = c(
    "Expressive language",
    "Reading",
    "Comprehension",
    "Writing & spelling",
    "Memory",
    "Numbers & math",
    "Speed of processing"
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
    "Focus",
    "Thought organization",
    "Inner speech",
    "Self-awareness",
    "Safety",
    "Managing money"
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
    "Grief",
    "Feeling less intelligent",
    "Guilt",
    "Feeling like a burden",
    "Helplessness",
    "Anger"
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
    "Public places",
    "Emergency situations",
    "Small talk",
    "Religious participation",
    "Interactions with law enforcement",
    "Legal matters",
    "Caregiving responsibilities",
    "Education / school",
    "Overcompensation by others",
    "Educating others about aphasia",
    "Leisure activities"
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
    "Positive QoL",
    "Denial",
    "Hope"
  ),
  treatment = c(
    "Lack of awareness",
    "Communication with healthcare providers",
    "Involved in decisions",
    "Medical navigation",
    "Medication & treatment",
    "Patient portal use",
    "Access to research / clinical trials"
  ),
  resources = c(
    "Communication strategies",
    "Home modification",
    "Independence",
    "Insurance & disability",
    "Moving",
    "Mental health services",
    "Additional therapy and hospitalization",
    "Residential facility",
    "Access to resources / information"
  ),
  impact_others = c(
    "Emotional impact on carepartners",
    "Social impact on carepartners",
    "Strained relationships",
    "Parenting",
    "Financial impact on carepartners",
    "Intimacy / sex",
    "Exhaustion",
    "Perceived intelligence of carepartners"
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
# For the Clinicians/Researchers instrument every item is treated as a
# NEW item surfaced via patient-generated focus-group work:
#   known_new   = "New"
#   source_type = "New – PG"
#
# The Known-vs-New comparison machinery is retained for structural
# compatibility with the scoring/visualization modules, but because all
# items share a single classification the Known group is empty. Downstream
# code that compares Known vs New (09_novel_vs_known.R) detects this and
# skips the between-source tests gracefully.
#
# Build KNOWN_NEW_DF programmatically from DOMAIN_LABELS so it always stays
# in sync with the item set above.
KNOWN_NEW_DF <- tibble(
  item_label  = unlist(DOMAIN_LABELS, use.names = FALSE),
  known_new   = "New",
  source_type = "New – PG"
) %>%
  mutate(
    source_category = source_type   # already "New – PG"
  )
