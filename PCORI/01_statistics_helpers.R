#' Statistical helpers
#'
#' Kruskal-Wallis (k-group, used here for the 3-way stakeholder-group
#' comparison AND for demographic factors), epsilon-squared effect size,
#' Dunn's post-hoc, and Spearman correlation wrappers.
#'
#' Ported from the original pipeline's 03_statistics.R with light
#' generalization (group_var is now a parameter everywhere rather than
#' hard-coded, since this pipeline reuses these helpers for both the
#' 3-way stakeholder-group comparison and demographic sub-analyses).

# ── Epsilon-squared for Kruskal-Wallis ─────────────────────
epsilon_squared <- function(H, n) {
  as.numeric(H / ((n^2 - 1) / (n + 1)))
}

# ── Spearman correlations: continuous predictor x all items ─
#' @param data            Cleaned data frame (one row per respondent).
#' @param predictor_var   Name of the continuous predictor column.
#' @param predictor_label Human-readable label.
#' @param items           Character vector of item column names.
#' @return A tibble with rho, p, n per item.
spearman_item_associations <- function(data, predictor_var, predictor_label,
                                       items = ALL_ITEMS_40) {
  if (!predictor_var %in% names(data)) {
    message("  Variable '", predictor_var, "' not found. Skipping.")
    return(tibble())
  }

  long <- data %>%
    select(all_of(c(predictor_var, items))) %>%
    pivot_longer(all_of(items), names_to = "item", values_to = "value") %>%
    rename(predictor = all_of(predictor_var))

  map_df(unique(long$item), function(it) {
    df_pair <- long %>%
      filter(item == it) %>%
      select(predictor, value) %>%
      drop_na()

    ct <- tryCatch(
      cor.test(df_pair$predictor, df_pair$value,
               method = "spearman", exact = FALSE),
      error = function(e) NULL
    )

    tibble(
      item    = it,
      rho     = if (is.null(ct)) NA_real_ else as.numeric(ct$estimate),
      p_value = if (is.null(ct)) NA_real_ else ct$p.value,
      n       = nrow(df_pair)
    )
  }) %>%
    mutate(
      predictor = predictor_label,
      p_adj_fdr = p.adjust(p_value, method = "fdr")
    ) %>%
    left_join(ITEM_MAP_40 %>% select(item, item_label, domain), by = "item") %>%
    mutate(item_label = coalesce(item_label, item))
}

# ── KW for a k-level categorical grouping variable x all items ─────
#' @param data         Cleaned data frame.
#' @param group_var    Name of the categorical column (2+ levels).
#' @param group_label  Human-readable label.
#' @param items        Character vector of item column names.
#' @return A tibble with KW stats per item.
kw_categorical_association <- function(data, group_var, group_label,
                                       items = ALL_ITEMS_40) {
  if (!group_var %in% names(data)) {
    message("  Variable '", group_var, "' not found. Skipping.")
    return(tibble())
  }

  data_long <- data %>%
    select(all_of(c(group_var, items))) %>%
    pivot_longer(all_of(items), names_to = "item", values_to = "value") %>%
    rename(group_cat = all_of(group_var)) %>%
    drop_na(value, group_cat)

  testable_items <- data_long %>%
    group_by(item) %>%
    filter(n_distinct(group_cat) >= 2) %>%
    pull(item) %>%
    unique()

  results <- map_df(testable_items, function(it) {
    df_sub <- data_long %>% filter(item == it)
    kw     <- tryCatch(kruskal.test(value ~ group_cat, data = df_sub),
                       error = function(e) NULL)
    if (is.null(kw)) {
      return(tibble(item = it, statistic = NA_real_,
                    p_value = NA_real_, eps_sq = NA_real_))
    }
    tibble(
      item      = it,
      statistic = as.numeric(kw$statistic),
      p_value   = kw$p.value,
      eps_sq    = epsilon_squared(as.numeric(kw$statistic), nrow(df_sub))
    )
  })

  if (nrow(results) == 0) return(tibble())

  results %>%
    mutate(
      predictor  = group_label,
      p_adj_bonf = p.adjust(p_value, method = "bonferroni"),
      p_adj_fdr  = p.adjust(p_value, method = "fdr")
    ) %>%
    left_join(ITEM_MAP_40 %>% select(item, item_label, domain), by = "item") %>%
    mutate(item_label = coalesce(item_label, item))
}

# ── Dunn's post-hoc wrapper (item-level) ───────────────────
#' @param sig_items  Character vector of item column names with p < .05.
#' @param data_long  Long-format data with columns: item, value, group_col.
#' @param group_col  Name of the grouping column.
#' @return A tibble of pairwise comparisons.
run_dunn_posthoc <- function(sig_items, data_long, group_col) {
  map_df(sig_items, function(item_name) {
    df_tmp <- data_long %>%
      filter(item == item_name) %>%
      drop_na(value, !!sym(group_col))

    if (nrow(df_tmp) < 3 || n_distinct(df_tmp[[group_col]]) < 2) {
      return(tibble())
    }

    dt <- tryCatch(
      dunn.test(df_tmp$value, df_tmp[[group_col]],
                method = "bonferroni", kw = FALSE, table = FALSE),
      error = function(e) NULL
    )
    if (is.null(dt)) return(tibble())

    tibble(
      item       = item_name,
      item_label = unname(ITEM_LABELS_40[item_name]),
      comparison = dt$comparisons,
      z_stat     = dt$Z,
      p_value    = dt$P,
      p_adj      = dt$P.adjusted
    )
  })
}

# ── Domain-level KW for a categorical grouping variable ────────────
#' @param domain_means  Data frame with one row per respondent and
#'                       columns like "<domain>_mean" plus group_var.
#' @param group_var     Grouping column name.
#' @param group_label   Human-readable label.
#' @return list(kw = tibble, dunn = tibble or NULL)
domain_kw_association <- function(domain_means, group_var, group_label) {
  if (!group_var %in% names(domain_means)) {
    message("  Variable '", group_var, "' not found. Skipping.")
    return(list(kw = tibble(), dunn = NULL))
  }

  domain_cols <- paste0(names(DOMAINS_40), "_mean")
  domain_cols <- intersect(domain_cols, names(domain_means))

  long <- domain_means %>%
    select(all_of(c(group_var, domain_cols))) %>%
    pivot_longer(all_of(domain_cols), names_to = "domain", values_to = "score") %>%
    drop_na(score, !!sym(group_var))

  kw_results <- long %>%
    group_by(domain) %>%
    group_modify(~ {
      if (n_distinct(.x[[group_var]]) < 2) {
        return(tibble(p_value = NA_real_, statistic = NA_real_,
                      eps_sq = NA_real_, n_groups = NA_integer_))
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
    mutate(predictor = group_label, p_adj_fdr = p.adjust(p_value, method = "fdr"))

  sig_domains <- kw_results %>% filter(p_value < 0.05) %>% pull(domain)

  dunn_out <- if (length(sig_domains) > 0) {
    map_df(sig_domains, function(d) {
      df_sub <- long %>% filter(domain == d)
      if (nrow(df_sub) < 3 || n_distinct(df_sub[[group_var]]) < 2) return(tibble())
      dt <- tryCatch(
        dunn.test(df_sub$score, df_sub[[group_var]],
                  method = "bonferroni", kw = FALSE, table = FALSE),
        error = function(e) NULL
      )
      if (is.null(dt)) return(tibble())
      tibble(domain = d, comparison = dt$comparisons,
             z_stat = dt$Z, p_value = dt$P, p_adj = dt$P.adjusted)
    })
  } else NULL

  list(kw = kw_results, dunn = dunn_out)
}

#' Item-level KW + Dunn for a categorical grouping variable (bundled).
item_kw_association <- function(data, group_var, group_label, items = ALL_ITEMS_40) {
  kw <- kw_categorical_association(data, group_var, group_label, items)
  if (nrow(kw) == 0) return(list(kw = tibble(), dunn = NULL))

  data_long <- data %>%
    select(all_of(c(group_var, items))) %>%
    pivot_longer(all_of(items), names_to = "item", values_to = "value") %>%
    rename(group_cat = all_of(group_var)) %>%
    drop_na(value, group_cat)

  sig_items <- kw %>% filter(p_value < 0.05) %>% pull(item)
  dunn <- if (length(sig_items) > 0) run_dunn_posthoc(sig_items, data_long, "group_cat") else NULL

  list(kw = kw, dunn = dunn)
}
