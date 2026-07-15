#' Demographic associations (single stakeholder group: Clinicians / Researchers)
#'
#' Categorical factors (domain-level KW + Dunn, item-level KW + Dunn):
#'   Age group, Region, Race/ethnicity (collapsed), Occupation group
#' Continuous predictor (domain-level + item-level Spearman):
#'   Age
#'
#' SES, urbanicity, severity, and months-post-onset analyses from the
#' original PWA/FMC pipeline are not included: SES was dropped per request,
#' and the others are aphasia-specific clinical variables not collected for
#' this stakeholder group.

# ── Internal: item-level KW + Dunn for a grouping variable ──────────────
.kw_with_posthoc <- function(dat, group_var, label) {
  if (!group_var %in% names(dat$df_clean)) {
    message("  Variable '", group_var, "' not found. Skipping.")
    return(list(kw = tibble(), dunn = NULL))
  }

  all_items  <- unlist(dat$domains, use.names = FALSE)
  domain_map <- item_domain_lookup(dat$domains)

  item_long <- dat$df_clean %>%
    select(all_of(c(group_var, all_items))) %>%
    pivot_longer(all_of(all_items), names_to = "item", values_to = "value") %>%
    rename(group_cat = all_of(group_var)) %>%
    drop_na(value, group_cat) %>%
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    mutate(item_label = coalesce(item_label, item))

  kw_results <- item_long %>%
    group_by(item, item_label) %>%
    group_modify(~ {
      if (n_distinct(.x$group_cat) < 2) {
        return(tibble(
          p_value   = NA_real_,
          statistic = NA_real_,
          eps_sq    = NA_real_,
          n_groups  = NA_integer_
        ))
      }

      kw <- kruskal.test(value ~ group_cat, data = .x)

      tibble(
        p_value   = kw$p.value,
        statistic = as.numeric(kw$statistic),
        eps_sq    = epsilon_squared(as.numeric(kw$statistic), nrow(.x)),
        n_groups  = as.integer(n_distinct(.x$group_cat))
      )
    }) %>%
    ungroup() %>%
    left_join(domain_map, by = "item") %>%
    mutate(
      predictor  = label,
      p_adj_bonf = p.adjust(p_value, method = "bonferroni"),
      p_adj_fdr  = p.adjust(p_value, method = "fdr")
    )

  sig_items <- kw_results %>% filter(p_value < 0.05) %>% pull(item)

  dunn_out <- if (length(sig_items) > 0) {
    map_df(sig_items, function(it) {
      df_sub <- item_long %>% filter(item == it)
      if (nrow(df_sub) < 3 || n_distinct(df_sub$group_cat) < 2) {
        return(tibble())
      }

      dt <- tryCatch(
        dunn.test(df_sub$value, df_sub$group_cat,
                  method = "bonferroni", kw = FALSE, table = FALSE),
        error = function(e) NULL
      )

      if (is.null(dt)) return(tibble())

      tibble(
        item       = it,
        item_label = unname(ITEM_LABELS[it]),
        comparison = dt$comparisons,
        z_stat     = dt$Z,
        p_value    = dt$P,
        p_adj      = dt$P.adjusted
      )
    })
  } else {
    NULL
  }

  list(kw = kw_results, dunn = dunn_out)
}

# ── Internal: domain-level KW + Dunn for a grouping variable ────────────
.domain_kw <- function(domain_means, group_var, label) {
  if (!group_var %in% names(domain_means)) {
    message("  Variable '", group_var, "' not found. Skipping.")
    return(list(kw = tibble(), dunn = NULL))
  }

  domain_cols <- c(
    "symptom_mean", "costs_mean", "emotions_mean", "social_mean",
    "cognition_mean", "treatment_mean", "qol_mean",
    "resources_mean", "impact_others_mean"
  )

  long <- domain_means %>%
    select(all_of(c(group_var, domain_cols))) %>%
    pivot_longer(all_of(domain_cols), names_to = "domain", values_to = "score") %>%
    drop_na(score, !!sym(group_var))

  kw_results <- long %>%
    group_by(domain) %>%
    group_modify(~ {
      if (n_distinct(.x[[group_var]]) < 2) {
        return(tibble(
          p_value   = NA_real_,
          statistic = NA_real_,
          eps_sq    = NA_real_,
          n_groups  = NA_integer_
        ))
      }

      kw <- kruskal.test(reformulate(group_var, response = "score"), data = .x)

      tibble(
        p_value   = kw$p.value,
        statistic = as.numeric(kw$statistic),
        eps_sq    = epsilon_squared(as.numeric(kw$statistic), nrow(.x)),
        n_groups  = as.integer(n_distinct(.x[[group_var]]))
      )
    }) %>%
    ungroup() %>%
    mutate(
      predictor = label,
      p_adj_fdr = p.adjust(p_value, method = "fdr")
    )

  sig_domains <- kw_results %>% filter(p_value < 0.05) %>% pull(domain)

  dunn_out <- if (length(sig_domains) > 0) {
    map_df(sig_domains, function(d) {
      df_sub <- long %>% filter(domain == d)
      if (nrow(df_sub) < 3 || n_distinct(df_sub[[group_var]]) < 2) {
        return(tibble())
      }

      dt <- tryCatch(
        dunn.test(df_sub$score, df_sub[[group_var]],
                  method = "bonferroni", kw = FALSE, table = FALSE),
        error = function(e) NULL
      )

      if (is.null(dt)) return(tibble())

      tibble(
        domain     = d,
        comparison = dt$comparisons,
        z_stat     = dt$Z,
        p_value    = dt$P,
        p_adj      = dt$P.adjusted
      )
    })
  } else {
    NULL
  }

  list(kw = kw_results, dunn = dunn_out)
}

# ── Internal: bundle domain + item results for one categorical factor ────
.demographic_bundle <- function(dat, domain_means, group_var, label) {
  list(
    domains = .domain_kw(domain_means, group_var, label),
    items   = .kw_with_posthoc(dat, group_var, label)
  )
}

# ── Internal: domain-level Spearman for continuous predictor ────────────
.domain_spearman <- function(domain_means, predictor_var, label) {
  if (!predictor_var %in% names(domain_means)) {
    message("  Variable '", predictor_var, "' not found. Skipping.")
    return(tibble())
  }

  domain_cols <- c(
    "symptom_mean", "costs_mean", "emotions_mean", "social_mean",
    "cognition_mean", "treatment_mean", "qol_mean",
    "resources_mean", "impact_others_mean"
  )

  map_df(domain_cols, function(dom) {
    df_tmp <- domain_means %>%
      select(all_of(c(predictor_var, dom))) %>%
      drop_na()

    if (nrow(df_tmp) < 3) {
      return(tibble(domain = dom, spearman_rho = NA_real_,
                    p_value = NA_real_, n = NA_integer_))
    }

    ct <- tryCatch(
      cor.test(df_tmp[[predictor_var]], df_tmp[[dom]],
               method = "spearman", exact = FALSE),
      error = function(e) NULL
    )

    if (is.null(ct)) {
      return(tibble(domain = dom, spearman_rho = NA_real_,
                    p_value = NA_real_, n = nrow(df_tmp)))
    }

    tibble(
      domain       = dom,
      spearman_rho = as.numeric(ct$estimate),
      p_value      = ct$p.value,
      n            = nrow(df_tmp)
    )
  }) %>%
    mutate(
      predictor = label,
      p_adj_fdr = p.adjust(p_value, method = "fdr")
    )
}

# ── Master function ──────────────────────────────────────────────────────
#' Run all demographic association analyses (single group).
#'
#' @param dat          Output of load_and_clean().
#' @param domain_means Output of compute_domain_means().
#' @return A named list of result tables.
run_clinical_demographic <- function(dat, domain_means) {
  out <- list()

  # Demographic categorical factors: domain + item level
  out$age_group  <- .demographic_bundle(dat, domain_means, "age_group",  "Age group")
  out$region     <- .demographic_bundle(dat, domain_means, "region",     "Region")
  out$race       <- .demographic_bundle(dat, domain_means,
                                        "race_ethnicity_collapsed",
                                        "Race/ethnicity (collapsed)")
  out$occupation <- .demographic_bundle(dat, domain_means, "occupation_group",
                                        "Occupation group")

  # Continuous item-level associations: Age
  out$age <- spearman_item_associations(dat$df_clean, "age", "Age", dat$domains)

  # Continuous domain-level associations: Age
  out$age_domain_cor <- .domain_spearman(domain_means, "age", "Age")

  out
}

# ── Pretty-print helper ──────────────────────────────────────────────────
print_clinical_demographic <- function(res) {

  # Domain-level categorical factors
  for (nm in c("age_group", "region", "race", "occupation")) {
    if (!is.null(res[[nm]]) && nrow(res[[nm]]$domains$kw) > 0) {
      cat("\n=== ", toupper(gsub("_", " ", nm)), ": Domain-Level KW ===\n", sep = "")
      print(res[[nm]]$domains$kw, n = Inf)
      if (!is.null(res[[nm]]$domains$dunn)) {
        cat("\n--- Dunn's Post-Hoc (", gsub("_", " ", nm), " x Domains) ---\n", sep = "")
        print(res[[nm]]$domains$dunn, n = 30)
      }
    }
  }

  # Continuous domain-level associations
  if (nrow(res$age_domain_cor) > 0) {
    cat("\n=== Age x Domain Means ===\n")
    print(res$age_domain_cor, n = Inf)
  }

  # Continuous item-level associations
  if (length(res$age) > 0 && nrow(res$age) > 0) {
    cat("\n=== Age x Item Ratings ===\n")
    print(
      res$age %>%
        arrange(p_value) %>%
        select(item_label, domain, rho, p_value, p_adj_fdr, n),
      n = 15
    )
  }

  # Item-level categorical factors
  for (nm in c("age_group", "region", "race", "occupation")) {
    if (!is.null(res[[nm]]) && nrow(res[[nm]]$items$kw) > 0) {
      cat("\n=== ", toupper(gsub("_", " ", nm)), ": Item-Level KW ===\n", sep = "")
      print(
        res[[nm]]$items$kw %>%
          arrange(p_value) %>%
          select(item_label, domain, statistic, eps_sq, p_value, p_adj_bonf),
        n = 15
      )
      if (!is.null(res[[nm]]$items$dunn)) {
        cat("\n--- Dunn's Post-Hoc (", gsub("_", " ", nm), " x Items) ---\n", sep = "")
        print(res[[nm]]$items$dunn, n = 20)
      }
    }
  }
}
