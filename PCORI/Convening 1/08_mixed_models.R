#' ============================================================
#' Mixed ordinal models for PWA vs FMC and clinical/demographic factors
#' ============================================================
#'
#' Purpose:
#'   Adds publication-strength cumulative-link mixed models (CLMMs) to the
#'   existing nonparametric pipeline. These models account for repeated ratings
#'   from the same respondent and repeated measurement across survey items.
#'
#' Depends on:
#'   00_config.R, 01_load_clean.R, 02_scoring.R
#'
#' Required package:
#'   ordinal
#'
#' Model overview
#' ──────────────────────────────────────────────────────────────
#'  primary_group               : rating ~ respondent + (1|pid) + (1|item)
#'  adjusted_group_demographics : rating ~ respondent + demographics + (1|pid) + (1|item)
#'  group_by_domain             : rating ~ respondent * domain + demographics + (1|pid) + (1|item)
#'  group_by_known_new          : rating ~ respondent * known_new + demographics + (1|pid) + (1|item)
#'  pwa_clinical_demographic    : PWA only — rating ~ clinical/demographics + (1|pid) + (1|item)
#'
#'  ── NEW models ──────────────────────────────────────────────
#'  domain_main_effects         : ALL respondents — rating ~ domain + (1|pid) + (1|item)
#'                                  Main effect of domain, collapsing across PWA and FMC.
#'  pwa_domain_main_effects     : PWA only  — rating ~ domain + (1|pid) + (1|item)
#'  fmc_domain_main_effects     : FMC only  — rating ~ domain + (1|pid) + (1|item)
#'
#' Notes:
#'   - The outcome is the original 1-5 ordinal item rating.
#'   - respondent is the PWA vs FMC group variable created in load_and_clean().
#'   - severity and months post-onset are modeled only within PWA because they
#'     are aphasia-specific clinical variables not directly comparable between groups.

# ── Package check ──────────────────────────────────────────
.require_ordinal <- function() {
  if (!requireNamespace("ordinal", quietly = TRUE)) {
    stop(
      "Package 'ordinal' is required for mixed ordinal models. ",
      "Install it with install.packages('ordinal') and re-run the pipeline.",
      call. = FALSE
    )
  }
}

# ── Interpretability note ──────────────────────────────────
# The `impact_others` domain warrants special caution in any model that
# includes a PWA vs FMC group contrast (primary_group, adjusted_group_demographics,
# group_by_domain, domain_impact_others stratified model).
#
# FMC respondents ARE the carepartners described by these items — their ratings
# reflect direct lived experience. PWA respondents are rating perceived impact
# on others. The referent of the construct differs between groups, so a
# statistically significant "respondent" effect in this domain cannot be
# cleanly interpreted as a group difference in importance ratings — it may
# partly or wholly reflect this referent mismatch rather than genuine
# disagreement about priority.
#
# This does NOT affect the PWA-only or FMC-only domain models, where the
# referent is consistent within group.
.IMPACT_OTHERS_NOTE <- paste(
  "INTERPRETABILITY NOTE — impact_others domain:",
  "FMC respondents are the carepartners these items describe (direct experience);",
  "PWA respondents rate perceived impact on others (observer perspective).",
  "The PWA vs FMC contrast for this domain reflects a referent mismatch,",
  "not only a difference in perceived importance. Interpret group effects",
  "in this domain with caution. Within-group models (PWA-only, FMC-only)",
  "are not affected.",
  sep = "\n  "
)

# ── Build long-format ordinal data ─────────────────────────
#' Convert cleaned survey data to long format for mixed ordinal models.
#'
#' @param dat Output of load_and_clean().
#' @return Tibble with one row per respondent-item rating.
build_mixed_model_data <- function(dat) {
  all_items  <- unlist(dat$domains, use.names = FALSE)
  domain_map <- item_domain_lookup(dat$domains)

  dat$df_clean %>%
    mutate(participant_id = row_number()) %>%
    select(
      participant_id,
      any_of(c(
        "respondent", "stakeholder_group", "age", "age_group", "ses", "region",
        "urban", "race_ethnicity_collapsed", "history_severity",
        "history_severity_num", "mpo"
      )),
      all_of(all_items)
    ) %>%
    pivot_longer(all_of(all_items), names_to = "item", values_to = "value") %>%
    drop_na(value, respondent) %>%
    mutate(
      rating         = ordered(as.integer(value), levels = 1:5),
      respondent     = factor(respondent, levels = c("PWA", "FMC")),
      item           = factor(item),
      participant_id = factor(participant_id)
    ) %>%
    left_join(domain_map, by = "item") %>%
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    mutate(item_label = coalesce(item_label, as.character(item))) %>%
    left_join(
      tibble(item_label = KNOWN_NEW_DF$item_label,
             known_new  = KNOWN_NEW_DF$known_new),
      by = "item_label"
    ) %>%
    mutate(
      domain   = factor(domain),
      known_new = factor(known_new, levels = c("Known", "New"))
    )
}

# ── Internal helpers ───────────────────────────────────────
.has_usable_predictor <- function(data, var) {
  var %in% names(data) && n_distinct(data[[var]][!is.na(data[[var]])]) >= 2
}

.available_covariates <- function(data, candidates) {
  candidates[vapply(candidates, function(v) .has_usable_predictor(data, v), logical(1))]
}

.fit_clmm_safe <- function(formula, data, model_name) {
  .require_ordinal()

  needed_vars <- all.vars(formula)
  data_model  <- data %>%
    select(any_of(needed_vars)) %>%
    drop_na()

  if (nrow(data_model) == 0) {
    warning("No complete cases for model: ", model_name)
    return(NULL)
  }

  if ("respondent" %in% needed_vars && n_distinct(data_model$respondent) < 2) {
    warning("Model '", model_name, "' has fewer than 2 respondent groups after filtering.")
    return(NULL)
  }

  tryCatch(
    ordinal::clmm(
      formula = formula,
      data    = data_model,
      link    = "logit",
      Hess    = TRUE,
      nAGQ    = 1
    ),
    error = function(e) {
      warning("CLMM failed for ", model_name, ": ", conditionMessage(e))
      NULL
    }
  )
}

#' Tidy fixed effects from a CLMM.
#'
#' @param model     A fitted ordinal::clmm model.
#' @param model_name Name to attach to the output.
#' @return Tibble of fixed-effect estimates, odds ratios, and Wald CIs.
tidy_clmm_fixed <- function(model, model_name) {
  if (is.null(model)) return(tibble())

  coef_mat <- coef(summary(model))
  as_tibble(coef_mat, rownames = "term") %>%
    rename(
      estimate  = Estimate,
      std_error = `Std. Error`,
      z_value   = `z value`,
      p_value   = `Pr(>|z|)`
    ) %>%
    # Keep only predictor rows (exclude threshold rows like "1|2", "2|3", …)
    filter(!grepl("^[0-9]+\\|[0-9]+$", term)) %>%
    mutate(
      model      = model_name,
      odds_ratio = exp(estimate),
      ci_low     = exp(estimate - 1.96 * std_error),
      ci_high    = exp(estimate + 1.96 * std_error)
    ) %>%
    select(model, term, estimate, std_error, z_value, p_value,
           odds_ratio, ci_low, ci_high)
}

.model_nobs <- function(model) {
  if (is.null(model)) return(NA_integer_)
  as.integer(stats::nobs(model))
}

.model_aic <- function(model) {
  if (is.null(model)) return(NA_real_)
  as.numeric(stats::AIC(model))
}

# ── Master mixed-model runner ──────────────────────────────
#' Run mixed ordinal models.
#'
#' Includes three new domain main-effects models:
#'   domain_main_effects     — all respondents combined
#'   pwa_domain_main_effects — PWA only
#'   fmc_domain_main_effects — FMC only
#'
#' @param dat Output of load_and_clean().
#' @return Named list containing long data, model objects, fixed-effect tables,
#'         and model-fit diagnostics.
run_mixed_models <- function(dat) {
  .require_ordinal()

  long <- build_mixed_model_data(dat)

  # Demographic covariates usable for both PWA and FMC.
  # Excludes severity and MPO because those are clinical/PWA-specific.
  group_covars <- .available_covariates(
    long,
    c("age", "ses", "region", "urban", "race_ethnicity_collapsed")
  )

  # ── 1. Primary PWA vs FMC model ───────────────────────────
  primary_formula <- rating ~ respondent + (1 | participant_id) + (1 | item)
  primary_model   <- .fit_clmm_safe(primary_formula, long, "primary_group")

  # ── 2. Adjusted PWA vs FMC model (demographic covariates) ─
  adjusted_rhs <- paste(
    c("respondent", group_covars, "(1 | participant_id)", "(1 | item)"),
    collapse = " + "
  )
  adjusted_group_model <- .fit_clmm_safe(
    as.formula(paste("rating ~", adjusted_rhs)),
    long,
    "adjusted_group_demographics"
  )

  # ── 3. Group-by-domain interaction ────────────────────────
  #    Tests whether PWA vs FMC differences vary by domain.
  domain_rhs <- paste(
    c("respondent * domain", group_covars, "(1 | participant_id)", "(1 | item)"),
    collapse = " + "
  )
  domain_interaction_model <- .fit_clmm_safe(
    as.formula(paste("rating ~", domain_rhs)),
    long,
    "group_by_domain"
  )

  # ── 4. Novel vs Known × Group interaction ─────────────────
  novel_model <- NULL
  if (.has_usable_predictor(long, "known_new")) {
    novel_rhs <- paste(
      c("respondent * known_new", group_covars, "(1 | participant_id)", "(1 | item)"),
      collapse = " + "
    )
    novel_model <- .fit_clmm_safe(
      as.formula(paste("rating ~", novel_rhs)),
      long %>% drop_na(known_new),
      "group_by_known_new"
    )
  }

  # ── 5. Domain-stratified group models ────────────────────
  #    One adjusted PWA vs FMC model per domain.
  domain_models <- map(
    levels(droplevels(long$domain)),
    function(dom) {
      d      <- long %>% filter(domain == dom) %>% droplevels()
      covars <- .available_covariates(d, group_covars)
      rhs    <- paste(c("respondent", covars, "(1 | participant_id)", "(1 | item)"),
                      collapse = " + ")
      .fit_clmm_safe(as.formula(paste("rating ~", rhs)), d, paste0("domain_", dom))
    }
  )
  names(domain_models) <- levels(droplevels(long$domain))

  domain_fixed <- imap_dfr(domain_models, function(model, dom) {
    tidy_clmm_fixed(model, paste0("domain_", dom)) %>% mutate(domain = dom, .before = term)
  })

  # ── 6. PWA-only clinical model ────────────────────────────
  #    Severity and months post-onset within PWA only.
  pwa_long          <- long %>% filter(respondent == "PWA") %>% droplevels()
  pwa_clinical_covars <- .available_covariates(
    pwa_long,
    c("history_severity", "mpo", "age", "ses", "region", "urban", "race_ethnicity_collapsed")
  )

  pwa_clinical_model <- NULL
  if (length(intersect(pwa_clinical_covars, c("history_severity", "mpo"))) > 0) {
    pwa_rhs <- paste(c(pwa_clinical_covars, "(1 | participant_id)", "(1 | item)"),
                     collapse = " + ")
    pwa_clinical_model <- .fit_clmm_safe(
      as.formula(paste("rating ~", pwa_rhs)),
      pwa_long,
      "pwa_clinical_demographic"
    )
  }

  # ── 7. Domain main effects — ALL respondents combined ─────
  #    Collapses across PWA and FMC; estimates whether domains differ
  #    in overall rating regardless of respondent group.
  #    Reference domain is the first level of `domain` (alphabetical).
  domain_main_formula <- rating ~ domain + (1 | participant_id) + (1 | item)
  domain_main_model   <- .fit_clmm_safe(
    domain_main_formula,
    long,
    "domain_main_effects"
  )

  # ── 8. Domain main effects — PWA only ────────────────────
  pwa_domain_main_model <- .fit_clmm_safe(
    domain_main_formula,
    pwa_long,
    "pwa_domain_main_effects"
  )

  # ── 9. Domain main effects — FMC only ────────────────────
  fmc_long              <- long %>% filter(respondent == "FMC") %>% droplevels()
  fmc_domain_main_model <- .fit_clmm_safe(
    domain_main_formula,
    fmc_long,
    "fmc_domain_main_effects"
  )

  # ── Collect all models ─────────────────────────────────────
  model_list <- list(
    primary_group               = primary_model,
    adjusted_group_demographics = adjusted_group_model,
    group_by_domain             = domain_interaction_model,
    group_by_known_new          = novel_model,
    pwa_clinical_demographic    = pwa_clinical_model,
    domain_main_effects         = domain_main_model,
    pwa_domain_main_effects     = pwa_domain_main_model,
    fmc_domain_main_effects     = fmc_domain_main_model
  )

  fixed_effects <- imap_dfr(model_list, tidy_clmm_fixed)

  model_fit <- tibble(
    model = names(model_list),
    n_obs = vapply(model_list, .model_nobs, integer(1)),
    AIC   = vapply(model_list, .model_aic,  numeric(1))
  )

  list(
    long_data             = long,
    models                = model_list,
    domain_models         = domain_models,
    fixed_effects         = fixed_effects,
    domain_effects        = domain_fixed,
    model_fit             = model_fit,
    group_covariates_used = group_covars,
    pwa_covariates_used   = pwa_clinical_covars
  )
}

# ── Export helpers ─────────────────────────────────────────
#' Export mixed-model results to CSV.
#'
#' Writes three files:
#'   mixed_model_fixed_effects.csv          — all models' fixed effects
#'   mixed_model_domain_stratified_effects.csv — per-domain group models
#'   mixed_model_fit_statistics.csv         — n_obs and AIC per model
#'   mixed_model_domain_main_effects.csv    — domain main effects (all, PWA, FMC)
#'
#' @param mixed   Output of run_mixed_models().
#' @param out_dir Output directory.
export_mixed_model_results <- function(mixed, out_dir = "output") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write_csv(mixed$fixed_effects,
            file.path(out_dir, "mixed_model_fixed_effects.csv"))
  write_csv(mixed$domain_effects,
            file.path(out_dir, "mixed_model_domain_stratified_effects.csv"))
  write_csv(mixed$model_fit,
            file.path(out_dir, "mixed_model_fit_statistics.csv"))

  # Convenience export: domain main-effects models side by side
  domain_main_tbl <- mixed$fixed_effects %>%
    filter(model %in% c("domain_main_effects",
                        "pwa_domain_main_effects",
                        "fmc_domain_main_effects"))
  write_csv(domain_main_tbl,
            file.path(out_dir, "mixed_model_domain_main_effects.csv"))

  # Append interpretability note as trailing comment rows in the domain effects file
  note_rows <- tibble(
    model  = "# NOTE",
    domain = NA_character_,
    term   = .IMPACT_OTHERS_NOTE,
    estimate = NA_real_, std_error = NA_real_, z_value = NA_real_,
    p_value  = NA_real_, odds_ratio = NA_real_,
    ci_low   = NA_real_, ci_high   = NA_real_
  )
  domain_effects_annotated <- bind_rows(mixed$domain_effects, note_rows)
  write_csv(domain_effects_annotated,
            file.path(out_dir, "mixed_model_domain_stratified_effects.csv"))

  invisible(mixed)
}

#' Print concise mixed-model summary.
#'
#' @param mixed Output of run_mixed_models().
print_mixed_model_summary <- function(mixed) {
  cat("\n=== Mixed Ordinal Models: Model Fit ===\n")
  print(mixed$model_fit, n = Inf)

  cat("\n=== Mixed Ordinal Models: Fixed Effects (all models) ===\n")
  print(
    mixed$fixed_effects %>%
      arrange(model, p_value) %>%
      select(model, term, odds_ratio, ci_low, ci_high, p_value),
    n = Inf
  )

  cat("\n=== Domain Main Effects (all respondents) ===\n")
  print(
    mixed$fixed_effects %>%
      filter(model == "domain_main_effects") %>%
      arrange(p_value) %>%
      select(term, odds_ratio, ci_low, ci_high, p_value),
    n = Inf
  )

  cat("\n=== Domain Main Effects — PWA only ===\n")
  print(
    mixed$fixed_effects %>%
      filter(model == "pwa_domain_main_effects") %>%
      arrange(p_value) %>%
      select(term, odds_ratio, ci_low, ci_high, p_value),
    n = Inf
  )

  cat("\n=== Domain Main Effects — FMC only ===\n")
  print(
    mixed$fixed_effects %>%
      filter(model == "fmc_domain_main_effects") %>%
      arrange(p_value) %>%
      select(term, odds_ratio, ci_low, ci_high, p_value),
    n = Inf
  )

  cat("\n")
  cat(paste(rep("─", 60), collapse = ""), "\n")
  cat(.IMPACT_OTHERS_NOTE, "\n")
  cat(paste(rep("─", 60), collapse = ""), "\n")

  cat("\nDemographic covariates used in PWA vs FMC adjusted model:\n")
  cat(ifelse(length(mixed$group_covariates_used) == 0,
             "  None\n",
             paste0("  ", paste(mixed$group_covariates_used, collapse = ", "), "\n")))

  cat("\nPWA-only clinical/demographic covariates used:\n")
  cat(ifelse(length(mixed$pwa_covariates_used) == 0,
             "  None\n",
             paste0("  ", paste(mixed$pwa_covariates_used, collapse = ", "), "\n")))
}
