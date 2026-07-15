#' Single-group summaries (Clinicians / Researchers)
#'
#' The original pipeline compared PWA vs Family/Caregiver here with
#' Mann-Whitney U tests. With a single stakeholder group there is no
#' between-group contrast, so this module instead produces descriptive
#' domain- and item-level summaries for the one group.

#' Domain-level descriptive summary.
#'
#' @param domain_means  Output of compute_domain_means().
#' @return A tibble with mean / sd / median per domain.
domain_summary_table <- function(domain_means) {
  domain_cols <- c(
    "symptom_mean", "costs_mean", "emotions_mean", "social_mean",
    "cognition_mean", "treatment_mean", "qol_mean",
    "resources_mean", "impact_others_mean"
  )

  domain_means %>%
    select(all_of(domain_cols)) %>%
    pivot_longer(everything(), names_to = "domain", values_to = "score") %>%
    drop_na(score) %>%
    group_by(domain) %>%
    summarise(
      n_respondents = n(),
      mean_score    = mean(score),
      sd_score      = sd(score),
      median_score  = median(score),
      .groups       = "drop"
    ) %>%
    arrange(desc(mean_score))
}

#' Item-level descriptive summary.
#'
#' @param results  Item-level results tibble.
#' @return A tibble of per-item metrics sorted by priority score.
item_summary_table <- function(results) {
  results %>%
    select(item, item_label, domain, mean_score, prevalence,
           high_priority, priority_score) %>%
    arrange(desc(priority_score))
}

#' High-priority percentage table (top items by % rating 4-5).
#'
#' @param results  Item-level results tibble.
#' @return A tibble sorted by descending high_priority.
hp_summary <- function(results) {
  results %>%
    select(item, item_label, domain, high_priority, priority_score) %>%
    arrange(desc(high_priority))
}
