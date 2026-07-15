#' Sensitivity analysis: compare ranking methods
#'
#' Three methods: composite priority score, mean impact, consensus (% 4-5).

#' Build a combined ranking table for one group.
#'
#' @param results  Item-level results for a single group.
#' @return A tibble with rank_composite, rank_mean, rank_consensus.
sensitivity_rankings <- function(results) {
  results %>%
    mutate(
      item_label     = coalesce(ITEM_LABELS[item], item),
      rank_composite = min_rank(desc(priority_score)),
      rank_mean      = min_rank(desc(mean_score)),
      rank_consensus = min_rank(desc(high_priority))
    ) %>%
    select(
      item, item_label, domain, priority_score,
      rank_composite, rank_mean, rank_consensus
    )
}

#' Spearman correlations between the three ranking methods.
#'
#' @param rankings  Output of sensitivity_rankings().
#' @param group     Group label string.
#' @return A 3-row tibble of pairwise rho values.
sensitivity_correlations <- function(rankings, group) {
  tibble(
    comparison = c(
      "Composite vs Mean",
      "Composite vs Consensus",
      "Mean vs Consensus"
    ),
    rho = c(
      cor(rankings$rank_composite, rankings$rank_mean,
          method = "spearman", use = "complete.obs"),
      cor(rankings$rank_composite, rankings$rank_consensus,
          method = "spearman", use = "complete.obs"),
      cor(rankings$rank_mean, rankings$rank_consensus,
          method = "spearman", use = "complete.obs")
    ),
    group = group
  )
}
