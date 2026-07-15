#' ============================================================
#' Mixed ordinal models (single group: Clinicians / Researchers)
#' ============================================================
#'
#' Purpose:
#'   Cumulative-link mixed models (CLMMs) that account for repeated ratings
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
#'  domain_main_effects         : rating ~ domain + (1|pid) + (1|item)
#'  adjusted_demographics       : rating ~ domain + demographics + (1|pid) + (1|item)
#'  occupation_main_effects     : rating ~ occupation_group + (1|pid) + (1|item)
#'  domain_by_occupation        : rating ~ domain * occupation_group + (1|pid) + (1|item)
#'
#' Notes:
#'   - The outcome is the original 1-5 ordinal item rating.
#'   - There is a single stakeholder group, so there is no respondent/group
#'     contrast. occupation_group is the primary stratifier of interest and
#'     enters both as a covariate (adjusted model) and as a main effect /
#'     interaction term.
#'   - Demographic covariates: age, region, race_ethnicity_collapsed,
#'     occupation_group (when usable).

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
        "age", "age_group", "region",
        "race_ethnicity_collapsed", "occupation_group"
      )),
      all_of(all_items)
    ) %>%
    pivot_longer(all_of(all_items), names_to = "item", values_to = "value") %>%
    drop_na(value) %>%
    mutate(
      rating         = ordered(as.integer(value), levels = 1:5),
      item           = factor(item),
      participant_id = factor(participant_id)
    ) %>%
    left_join(domain_map, by = "item") %>%
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    mutate(
      item_label = coalesce(item_label, as.character(item)),
      domain     = factor(domain)
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
#' Run mixed ordinal models (single group).
#'
#' @param dat Output of load_and_clean().
#' @return Named list containing long data, model objects, fixed-effect tables,
#'         and model-fit diagnostics.
run_mixed_models <- function(dat) {
  .require_ordinal()

  long <- build_mixed_model_data(dat)

  # Demographic covariates usable for adjustment.
  demo_covars <- .available_covariates(
    long,
    c("age", "region", "race_ethnicity_collapsed", "occupation_group")
  )

  # ── 1. Domain main effects ────────────────────────────────
  #    Reference domain is the first level of `domain` (alphabetical).
  domain_main_formula <- rating ~ domain + (1 | participant_id) + (1 | item)
  domain_main_model   <- .fit_clmm_safe(
    domain_main_formula, long, "domain_main_effects"
  )

  # ── 2. Adjusted model (domain + demographic covariates) ───
  adjusted_rhs <- paste(
    c("domain", demo_covars, "(1 | participant_id)", "(1 | item)"),
    collapse = " + "
  )
  adjusted_model <- .fit_clmm_safe(
    as.formula(paste("rating ~", adjusted_rhs)),
    long,
    "adjusted_demographics"
  )

  # ── 3. Occupation-group main effects ──────────────────────
  occupation_model <- NULL
  if (.has_usable_predictor(long, "occupation_group")) {
    occ_formula <- rating ~ occupation_group + (1 | participant_id) + (1 | item)
    occupation_model <- .fit_clmm_safe(
      occ_formula, long, "occupation_main_effects"
    )
  }

  # ── 4. Domain × occupation-group interaction ──────────────
  #    Tests whether domain priorities vary by occupation group.
  domain_occ_model <- NULL
  if (.has_usable_predictor(long, "occupation_group")) {
    domain_occ_rhs <- paste(
      c("domain * occupation_group", "(1 | participant_id)", "(1 | item)"),
      collapse = " + "
    )
    domain_occ_model <- .fit_clmm_safe(
      as.formula(paste("rating ~", domain_occ_rhs)),
      long,
      "domain_by_occupation"
    )
  }

  # ── 5. Domain-stratified occupation models ────────────────
  #    One occupation-group model per domain (when occupation is usable).
  domain_models <- list()
  if (.has_usable_predictor(long, "occupation_group")) {
    domain_models <- map(
      levels(droplevels(long$domain)),
      function(dom) {
        d <- long %>% filter(domain == dom) %>% droplevels()
        if (!.has_usable_predictor(d, "occupation_group")) return(NULL)
        rhs <- paste(c("occupation_group", "(1 | participant_id)", "(1 | item)"),
                     collapse = " + ")
        .fit_clmm_safe(as.formula(paste("rating ~", rhs)), d,
                       paste0("domain_", dom))
      }
    )
    names(domain_models) <- levels(droplevels(long$domain))
  }

  domain_fixed <- imap_dfr(domain_models, function(model, dom) {
    tidy_clmm_fixed(model, paste0("domain_", dom)) %>%
      mutate(domain = dom, .before = term)
  })

  # ── Collect all models ─────────────────────────────────────
  model_list <- list(
    domain_main_effects     = domain_main_model,
    adjusted_demographics   = adjusted_model,
    occupation_main_effects = occupation_model,
    domain_by_occupation    = domain_occ_model
  )

  fixed_effects <- imap_dfr(model_list, tidy_clmm_fixed)

  model_fit <- tibble(
    model = names(model_list),
    n_obs = vapply(model_list, .model_nobs, integer(1)),
    AIC   = vapply(model_list, .model_aic,  numeric(1))
  )

  list(
    long_data            = long,
    models               = model_list,
    domain_models        = domain_models,
    fixed_effects        = fixed_effects,
    domain_effects       = domain_fixed,
    model_fit            = model_fit,
    covariates_used      = demo_covars
  )
}

# ── Export helpers ─────────────────────────────────────────
#' Export mixed-model results to CSV.
#'
#' @param mixed   Output of run_mixed_models().
#' @param out_dir Output directory.
export_mixed_model_results <- function(mixed, out_dir = "output") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write_csv(mixed$fixed_effects,
            file.path(out_dir, "mixed_model_fixed_effects.csv"))
  write_csv(mixed$model_fit,
            file.path(out_dir, "mixed_model_fit_statistics.csv"))

  if (nrow(mixed$domain_effects) > 0) {
    write_csv(mixed$domain_effects,
              file.path(out_dir, "mixed_model_domain_stratified_effects.csv"))
  }

  # Domain main-effects model on its own
  domain_main_tbl <- mixed$fixed_effects %>%
    filter(model == "domain_main_effects")
  write_csv(domain_main_tbl,
            file.path(out_dir, "mixed_model_domain_main_effects.csv"))

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

  cat("\n=== Domain Main Effects ===\n")
  print(
    mixed$fixed_effects %>%
      filter(model == "domain_main_effects") %>%
      arrange(p_value) %>%
      select(term, odds_ratio, ci_low, ci_high, p_value),
    n = Inf
  )

  if (any(mixed$fixed_effects$model == "occupation_main_effects")) {
    cat("\n=== Occupation-Group Main Effects ===\n")
    print(
      mixed$fixed_effects %>%
        filter(model == "occupation_main_effects") %>%
        arrange(p_value) %>%
        select(term, odds_ratio, ci_low, ci_high, p_value),
      n = Inf
    )
  }

  cat("\nDemographic covariates used in the adjusted model:\n")
  cat(ifelse(length(mixed$covariates_used) == 0,
             "  None\n",
             paste0("  ", paste(mixed$covariates_used, collapse = ", "), "\n")))
}
