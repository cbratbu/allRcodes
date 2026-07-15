#' Clinical and demographic associations
#'
#' Severity: domain-level KW + Dunn, item-level KW + Dunn
#' Age group / SES / Region / Urban / Race/ethnicity (collapsed):
#'   domain-level KW + Dunn, item-level KW + Dunn
#' Age / MPO:
#'   domain-level Spearman + item-level Spearman
#' Severity x domain means: Spearman correlation matrix

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
        item_label = unname(ITEM_LABELS[it]),   # FIX (Bug 2): unname()
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

# ── Severity domains ─────────────────────────────────────────────────────
kw_severity_domains <- function(domain_means) {
  .domain_kw(domain_means, "history_severity", "Severity")
}

# ── Master function ──────────────────────────────────────────────────────
#' Run all clinical and demographic association analyses.
#'
#' @param dat          Output of load_and_clean().
#' @param domain_means Output of compute_domain_means().
#' @return A named list of result tables.
#'
#' FIX (Bug 3): Removed the redundant dm <- domain_means %>% mutate(...)
#'   block that re-derived history_severity_num. That column is already
#'   created in load_and_clean() and carried through to domain_means via
#'   df_clean. domain_means is used directly for the rcorr() call.
run_clinical_demographic <- function(dat, domain_means) {
  out <- list()

  # ── PWA-only subsets ────────────────────────────────────────────────────
  # history_severity and mpo are aphasia-specific: for FMC they refer to
  # their family member's aphasia, not their own experience. Including FMC
  # confounds both analyses. All severity and MPO work runs on PWA only.
  dat_pwa <- dat
  dat_pwa$df_clean <- dat$df_clean %>% filter(stakeholder_group == 1)

  domain_means_pwa <- domain_means %>% filter(stakeholder_group == 1)

  # Severity (PWA only)
  out$severity_domains <- kw_severity_domains(domain_means_pwa)
  out$severity_items   <- .kw_with_posthoc(dat_pwa, "history_severity", "Severity")

  # Severity x domain means (Spearman via rcorr)
  # history_severity_num already exists in domain_means from load_and_clean()
  spearman_input <- domain_means_pwa %>%
    select(
      history_severity_num,
      symptom_mean, costs_mean, emotions_mean, social_mean,
      cognition_mean, treatment_mean, qol_mean,
      resources_mean, impact_others_mean
    ) %>%
    drop_na(history_severity_num)

  sp           <- rcorr(as.matrix(spearman_input), type = "spearman")
  domain_names <- setdiff(names(spearman_input), "history_severity_num")

  out$severity_domain_cor <- tibble(
    domain       = domain_names,
    spearman_rho = sp$r["history_severity_num", domain_names],
    p_value      = sp$P["history_severity_num", domain_names]
  ) %>%
    mutate(p_adj_fdr = p.adjust(p_value, method = "fdr"))

  # Demographic categorical factors: domain + item level
  out$age_group <- .demographic_bundle(dat, domain_means, "age_group",  "Age group")
  out$ses       <- .demographic_bundle(dat, domain_means, "ses",        "SES")
  out$region    <- .demographic_bundle(dat, domain_means, "region",     "Region")
  out$urban     <- .demographic_bundle(dat, domain_means, "urban",      "Urbanicity")
  out$race      <- .demographic_bundle(dat, domain_means,
                                       "race_ethnicity_collapsed",
                                       "Race/ethnicity (collapsed)")

  # Continuous item-level associations
  # Age: run on all respondents AND stratified by group (patterns may differ)
  out$age     <- spearman_item_associations(dat$df_clean, "age", "Age", dat$domains)
  out$age_pwa <- spearman_item_associations(dat_pwa$df_clean, "age", "Age (PWA)", dat$domains)

  dat_fmc <- dat
  dat_fmc$df_clean <- dat$df_clean %>% filter(stakeholder_group == 2)
  out$age_fmc <- spearman_item_associations(dat_fmc$df_clean, "age", "Age (FMC)", dat$domains)

  # MPO: PWA only — FMC MPO refers to their family member's stroke, not theirs
  out$mpo <- spearman_item_associations(dat_pwa$df_clean, "mpo", "Months Post-Onset (PWA)",
                                        dat$domains)

  # Continuous domain-level associations
  out$age_domain_cor     <- .domain_spearman(domain_means,     "age", "Age")
  out$age_domain_cor_pwa <- .domain_spearman(domain_means_pwa, "age", "Age (PWA)")
  out$age_domain_cor_fmc <- .domain_spearman(
    domain_means %>% filter(stakeholder_group == 2), "age", "Age (FMC)"
  )
  out$mpo_domain_cor <- .domain_spearman(domain_means_pwa, "mpo", "Months Post-Onset (PWA)")

  out
}

# ── Pretty-print helper ──────────────────────────────────────────────────
print_clinical_demographic <- function(res) {

  cat("\n=== SEVERITY: Domain-Level KW ===\n")
  print(res$severity_domains$kw, n = Inf)
  if (!is.null(res$severity_domains$dunn)) {
    cat("\n--- Dunn's Post-Hoc (Severity x Domains) ---\n")
    print(res$severity_domains$dunn, n = 30)
  }

  cat("\n=== SEVERITY: Item-Level KW ===\n")
  print(
    res$severity_items$kw %>%
      arrange(p_value) %>%
      select(item_label, domain, statistic, eps_sq, p_value, p_adj_bonf),
    n = 20
  )
  if (!is.null(res$severity_items$dunn)) {
    cat("\n--- Dunn's Post-Hoc (Severity x Items) ---\n")
    print(res$severity_items$dunn, n = 30)
  }

  cat("\n=== Severity x Domain Means (Spearman) ===\n")
  print(res$severity_domain_cor)

  # Domain-level categorical factors
  for (nm in c("age_group", "ses", "region", "urban", "race")) {
    if (nrow(res[[nm]]$domains$kw) > 0) {
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
    cat("\n=== Age x Domain Means (all respondents) ===\n")
    print(res$age_domain_cor, n = Inf)
  }
  if (nrow(res$age_domain_cor_pwa) > 0) {
    cat("\n=== Age x Domain Means (PWA only) ===\n")
    print(res$age_domain_cor_pwa, n = Inf)
  }
  if (nrow(res$age_domain_cor_fmc) > 0) {
    cat("\n=== Age x Domain Means (FMC only) ===\n")
    print(res$age_domain_cor_fmc, n = Inf)
  }

  if (nrow(res$mpo_domain_cor) > 0) {
    cat("\n=== Months Post-Onset x Domain Means (PWA only) ===\n")
    print(res$mpo_domain_cor, n = Inf)
  }

  # Continuous item-level associations
  for (nm in c("age", "age_pwa", "age_fmc")) {
    if (length(res[[nm]]) > 0 && nrow(res[[nm]]) > 0) {
      cat("\n=== Age x Item Ratings (", res[[nm]]$predictor[1], ") ===\n", sep = "")
      print(
        res[[nm]] %>%
          arrange(p_value) %>%
          select(item_label, domain, rho, p_value, p_adj_fdr, n),
        n = 15
      )
    }
  }

  if (nrow(res$mpo) > 0) {
    cat("\n=== Months Post-Onset x Item Ratings (PWA only) ===\n")
    print(
      res$mpo %>%
        arrange(p_value) %>%
        select(item_label, domain, rho, p_value, p_adj_fdr, n),
      n = 15
    )
  }

  # Item-level categorical factors
  for (nm in c("age_group", "ses", "region", "urban", "race")) {
    if (nrow(res[[nm]]$items$kw) > 0) {
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
