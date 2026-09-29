#' Between-group statistical comparisons (PWA vs FMC vs CR)
#'
#' For every item and every domain: Kruskal-Wallis test across the three
#' groups, with Dunn's post-hoc (Bonferroni-corrected) pairwise comparisons
#' (PWA-vs-FMC, PWA-vs-CR, FMC-vs-CR) wherever the omnibus test is
#' significant.
#'
#' Depends on: 01_statistics_helpers.R, 03_scoring.R

#' Run the full 3-group comparison (item-level and domain-level).
#'
#' @param dat           Output of load_and_clean().
#' @param domain_means  Output of compute_domain_means().
#' @return A named list: item = list(kw, dunn), domain = list(kw, dunn)
run_group_comparisons <- function(dat, domain_means) {
  item_out   <- item_kw_association(dat$df_clean, "stakeholder_group",
                                    "Stakeholder group", dat$items)
  domain_out <- domain_kw_association(domain_means, "stakeholder_group",
                                      "Stakeholder group")

  list(item = item_out, domain = domain_out)
}

#' Pretty-print the 3-group comparison results.
print_group_comparisons <- function(gc) {
  cat("\n=== STAKEHOLDER GROUP (PWA vs FMC vs CR): Domain-Level KW ===\n")
  if (nrow(gc$domain$kw) > 0) {
    print(gc$domain$kw %>% arrange(p_value), n = Inf)
  } else {
    cat("(no domain-level results)\n")
  }

  if (!is.null(gc$domain$dunn)) {
    cat("\n--- Dunn's Post-Hoc (Group x Domains) ---\n")
    print(gc$domain$dunn, n = 30)
  }

  cat("\n=== STAKEHOLDER GROUP (PWA vs FMC vs CR): Item-Level KW ===\n")
  if (nrow(gc$item$kw) > 0) {
    print(
      gc$item$kw %>%
        arrange(p_value) %>%
        select(item_label, domain, statistic, eps_sq, p_value, p_adj_bonf),
      n = 40
    )
  } else {
    cat("(no item-level results)\n")
  }

  if (!is.null(gc$item$dunn)) {
    cat("\n--- Dunn's Post-Hoc (Group x Items, significant items only) ---\n")
    print(gc$item$dunn, n = 60)
  }
}
