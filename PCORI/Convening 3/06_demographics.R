#' Demographic associations: region, socioeconomic status, race/ethnicity, age
#'
#' Run at three levels of granularity:
#'   1. Pooled (all groups together) — does region/SES/etc. matter overall?
#'   2. Within each stakeholder group separately — does it matter for PWA
#'      specifically? For FMC? For CR?
#'   3. Group x demographic interaction — a simple, transparent way to see
#'      whether a demographic effect differs by group: combine group and
#'      the demographic factor into one joint factor and test it. (This is
#'      a screening-level interaction check; for a formal interaction test
#'      an ordinal mixed model with a group x demographic term would be the
#'      next step — see note at the bottom of this file.)
#'
#' Depends on: 01_statistics_helpers.R, 03_scoring.R

CATEGORICAL_DEMOGRAPHICS <- c(
  age_group                 = "Age group",
  region                    = "Region",
  race_ethnicity_collapsed  = "Race/ethnicity (collapsed)",
  gender_label               = "Gender",
  ses_category                = "Socioeconomic status",
  urban_category               = "Urbanicity (urban/rural)",
  occupation_group            = "Occupation group (clinicians/researchers)",
  severity                    = "Self-reported aphasia severity (PWA/FMC)"
)

# Continuous demographics tested with Spearman correlations (alongside Age)
CONTINUOUS_DEMOGRAPHICS <- c(
  age              = "Age",
  education_years  = "Years of education",
  mpo              = "Months post-onset (PWA/FMC)"
)

#' Run item- and domain-level KW for every available categorical demographic,
#' on a given data subset (pooled or one group).
.demographics_for_subset <- function(df_sub, domain_means_sub, items) {
  out <- list()
  for (var in names(CATEGORICAL_DEMOGRAPHICS)) {
    if (!var %in% names(df_sub)) next
    label <- CATEGORICAL_DEMOGRAPHICS[[var]]
    out[[var]] <- list(
      items   = item_kw_association(df_sub, var, label, items),
      domains = domain_kw_association(domain_means_sub, var, label)
    )
  }

  # Continuous predictors (Age, education, ADI)
  out$continuous <- list()
  for (var in names(CONTINUOUS_DEMOGRAPHICS)) {
    if (!var %in% names(df_sub)) next
    out$continuous[[var]] <- spearman_item_associations(
      df_sub, var, CONTINUOUS_DEMOGRAPHICS[[var]], items
    )
  }

  out
}

#' Master function: pooled + per-group demographic associations.
#'
#' @param dat           Output of load_and_clean().
#' @param domain_means  Output of compute_domain_means().
#' @return A named list: pooled = ..., by_group = list(PWA = ..., FMC = ..., CR = ...)
run_demographics <- function(dat, domain_means) {
  pooled <- .demographics_for_subset(dat$df_clean, domain_means, dat$items)

  by_group <- set_names(GROUP_LEVELS) %>%
    map(function(g) {
      df_sub <- dat$df_clean %>% filter(stakeholder_group == g)
      dm_sub <- domain_means %>% filter(stakeholder_group == g)
      .demographics_for_subset(df_sub, dm_sub, dat$items)
    })

  list(pooled = pooled, by_group = by_group)
}

#' Screening-level group x demographic interaction check.
#' Combines group and a demographic factor into one joint label and runs a
#' single KW test per item; a low p-value means ratings differ across the
#' *combination* of group and demographic level (consistent with, though not
#' proof of, an interaction — group and demographic main effects can also
#' produce this pattern).
#'
#' @param dat        Output of load_and_clean().
#' @param demo_var   Name of the demographic column (e.g. "region").
#' @param demo_label Human-readable label.
#' @return A tibble like kw_categorical_association()'s output.
group_demographic_interaction_screen <- function(dat, demo_var, demo_label) {
  if (!demo_var %in% names(dat$df_clean)) {
    message("  Variable '", demo_var, "' not found. Skipping.")
    return(tibble())
  }

  df <- dat$df_clean %>%
    mutate(group_x_demo = interaction(stakeholder_group, .data[[demo_var]], drop = TRUE))

  kw_categorical_association(
    df, "group_x_demo",
    paste0("Group x ", demo_label, " (joint factor)"),
    dat$items
  )
}

#' Pretty-print helper for one demographics bundle (pooled or one group).
print_demographics_bundle <- function(bundle, header = "") {
  if (nzchar(header)) cat("\n############# ", header, " #############\n", sep = "")

  for (var in names(CATEGORICAL_DEMOGRAPHICS)) {
    if (is.null(bundle[[var]])) next
    label <- CATEGORICAL_DEMOGRAPHICS[[var]]

    dkw <- bundle[[var]]$domains$kw
    if (!is.null(dkw) && nrow(dkw) > 0) {
      cat("\n=== ", label, ": Domain-Level KW ===\n", sep = "")
      print(dkw %>% arrange(p_value), n = Inf)
    }

    ikw <- bundle[[var]]$items$kw
    if (!is.null(ikw) && nrow(ikw) > 0) {
      cat("\n=== ", label, ": Item-Level KW (top 15 by p-value) ===\n", sep = "")
      print(
        ikw %>% arrange(p_value) %>%
          select(item_label, domain, statistic, eps_sq, p_value, p_adj_bonf),
        n = 15
      )
    }
  }

  for (var in names(CONTINUOUS_DEMOGRAPHICS)) {
    ct <- bundle$continuous[[var]]
    if (!is.null(ct) && nrow(ct) > 0) {
      cat("\n=== ", CONTINUOUS_DEMOGRAPHICS[[var]],
          " x Item Ratings (Spearman, top 15 by p-value) ===\n", sep = "")
      print(
        ct %>% arrange(p_value) %>%
          select(item_label, domain, rho, p_value, p_adj_fdr, n),
        n = 15
      )
    }
  }
}
