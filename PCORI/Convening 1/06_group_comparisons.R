#' Group comparisons: PWA vs Family/Caregiver
#'
#' Domain-level and item-level Mann-Whitney U tests.
#'
#' ── Interpretability note: impact_others domain ───────────────────────────
#' Any PWA vs FMC group comparison that includes the `impact_others` domain
#' should be interpreted with caution. FMC respondents ARE the carepartners
#' described by these items and rate from direct lived experience; PWA
#' respondents rate perceived impact on others. The referent of the construct
#' differs between groups, so a significant group difference in this domain
#' may reflect that mismatch rather than (or in addition to) genuine
#' disagreement about priority. This note is printed at runtime and flagged
#' in exported CSVs via an `impact_others_caution` column.
.IMPACT_OTHERS_NOTE_GRP <- paste0(
  "INTERPRETABILITY CAUTION — impact_others domain: ",
  "FMC respondents are the carepartners these items describe (direct experience); ",
  "PWA respondents rate perceived impact on others (observer perspective). ",
  "A significant PWA vs FMC difference here may reflect referent mismatch, ",
  "not only disagreement about priority."
)

#' Domain-level Mann-Whitney U tests (PWA vs Family).
#'
#' @param domain_means  Output of compute_domain_means().
#' @return A tibble with test results per domain.
domain_group_tests <- function(domain_means) {
  domain_means %>%
    select(
      stakeholder_group,
      symptom_mean, costs_mean, emotions_mean, social_mean,
      cognition_mean, treatment_mean, qol_mean,
      resources_mean, impact_others_mean
    ) %>%
    pivot_longer(-stakeholder_group, names_to = "domain", values_to = "score") %>%
    group_by(domain) %>%
    group_modify(~ wilcox_rank_biserial(.x$score, .x$stakeholder_group)) %>%
    ungroup() %>%
    mutate(
      p_adj_fdr = p.adjust(p_value, method = "fdr"),
      impact_others_caution = domain == "impact_others_mean",
      caution_note = if_else(
        impact_others_caution,
        .IMPACT_OTHERS_NOTE_GRP,
        NA_character_
      )
    )
}

#' Item-level Mann-Whitney U tests (PWA vs Family).
#'
#' @param dat  Output of load_and_clean().
#' @return A tibble with test results per item.
item_group_tests <- function(dat) {
  domain_map <- item_domain_lookup(dat$domains)

  dat$df_clean %>%
    select(stakeholder_group, all_of(unlist(dat$domains, use.names = FALSE))) %>%
    pivot_longer(-stakeholder_group, names_to = "item", values_to = "value") %>%
    group_by(item) %>%
    group_modify(~ wilcox_rank_biserial(.x$value, .x$stakeholder_group)) %>%
    ungroup() %>%
    mutate(p_adj_fdr = p.adjust(p_value, method = "fdr")) %>%
    left_join(domain_map, by = "item") %>%
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    mutate(
      item_label = coalesce(item_label, item),
      impact_others_caution = domain == "impact_others",
      caution_note = if_else(
        impact_others_caution,
        .IMPACT_OTHERS_NOTE_GRP,
        NA_character_
      )
    )
}

#' High-priority percentage comparison between groups.
#'
#' @param results_compare  Full scored results (both groups).
#' @return A tibble with hp_{group1}, hp_{group2}, hp_diff.
hp_comparison <- function(results_compare) {
  results_compare %>%
    select(item, item_label, group, high_priority) %>%
    pivot_wider(names_from = group, values_from = high_priority,
                names_prefix = "hp_") %>%
    mutate(hp_diff = hp_PWA - hp_FMC) %>%
    arrange(desc(abs(hp_diff)))
}
