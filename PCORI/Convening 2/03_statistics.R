#' Statistical helpers
#'
#' Mann-Whitney U with rank-biserial r, epsilon-squared for KW,
#' Spearman correlation wrappers, Kruskal-Wallis by grouping variable.

# ── Mann-Whitney U with rank-biserial ─────────────────────
#' @param x  Numeric vector (outcome).
#' @param g  Grouping vector (exactly 2 levels expected).
#' @return   A one-row tibble.
wilcox_rank_biserial <- function(x, g) {
  df_tmp <- tibble(x = x, g = g) %>% drop_na()

  if (nrow(df_tmp) == 0 || n_distinct(df_tmp$g) != 2) {
    return(tibble(
      p_value       = NA_real_,
      statistic     = NA_real_,
      rank_biserial = NA_real_,
      n1            = NA_integer_,
      n2            = NA_integer_
    ))
  }

  test     <- suppressWarnings(wilcox.test(x ~ g, data = df_tmp, exact = FALSE))
  g_levels <- sort(unique(df_tmp$g))
  n1       <- sum(df_tmp$g == g_levels[1])
  n2       <- sum(df_tmp$g == g_levels[2])
  W        <- as.numeric(test$statistic)

  # Sign convention: rank_biserial = 1 - 2W/(n1*n2).
  # W counts pairs where group 1 > group 2.
  # Positive r  → group 2 tends to rate higher (for stakeholder_group: FMC higher).
  # Negative r  → group 1 tends to rate higher (PWA higher).
  # g_levels[1] / g_levels[2] printed in output for transparency.

  tibble(
    g1            = as.character(g_levels[1]),
    g2            = as.character(g_levels[2]),
    p_value       = test$p.value,
    statistic     = W,
    rank_biserial = 1 - (2 * W) / (n1 * n2),
    n1            = as.integer(n1),
    n2            = as.integer(n2)
  )
}

# ── Epsilon-squared for Kruskal-Wallis ─────────────────────
#' @param H  The KW chi-squared statistic.
#' @param n  Total sample size.
epsilon_squared <- function(H, n) {
  as.numeric(H / ((n^2 - 1) / (n + 1)))
}

# ── Spearman correlations: continuous predictor x all items ─
#' @param data            Cleaned data frame.
#' @param predictor_var   Name of the continuous predictor column.
#' @param predictor_label Human-readable label.
#' @param domains         Domain list (from load_and_clean).
#' @return A tibble with rho, p, n per item.
#'
#' FIX (Bug 1): cor.test() does not accept a 'use' argument — that belongs
#'   to cor(). Removed the invalid argument. cor.test() is now called once
#'   per item (previously called twice) with the result stored and both
#'   $estimate and $p.value extracted from the single object.
spearman_item_associations <- function(data, predictor_var, predictor_label,
                                       domains) {
  if (!predictor_var %in% names(data)) {
    message("  Variable '", predictor_var, "' not found. Skipping.")
    return(tibble())
  }

  item_cols  <- unlist(domains, use.names = FALSE)
  domain_map <- item_domain_lookup(domains)

  long <- data %>%
    select(all_of(c(predictor_var, item_cols))) %>%
    pivot_longer(all_of(item_cols), names_to = "item", values_to = "value") %>%
    rename(predictor = all_of(predictor_var))

  map_df(unique(long$item), function(it) {
    df_pair <- long %>%
      filter(item == it) %>%
      select(predictor, value) %>%
      drop_na()

    # Single call: store result, then extract estimate + p-value
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
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    left_join(domain_map, by = "item") %>%
    mutate(item_label = coalesce(item_label, item))
}

# ── KW for categorical grouping variable x all items ──────
#' @param data         Cleaned data frame.
#' @param group_var    Name of the categorical column.
#' @param group_label  Human-readable label.
#' @param domains      Domain list.
#' @return A tibble with KW stats per item.
kw_categorical_association <- function(data, group_var, group_label, domains) {
  if (!group_var %in% names(data)) {
    message("  Variable '", group_var, "' not found. Skipping.")
    return(tibble())
  }

  item_cols  <- unlist(domains, use.names = FALSE)
  domain_map <- item_domain_lookup(domains)

  data_long <- data %>%
    select(all_of(c(group_var, item_cols))) %>%
    pivot_longer(all_of(item_cols), names_to = "item", values_to = "value") %>%
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
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    left_join(domain_map, by = "item") %>%
    mutate(item_label = coalesce(item_label, item))
}

# ── Dunn's post-hoc wrapper ───────────────────────────────
#' Run Dunn's test for a set of significant items.
#'
#' @param sig_items  Character vector of item column names.
#' @param data_long  Long-format data with columns: item, value, group_col.
#' @param group_col  Name of the grouping column.
#' @return A tibble of pairwise comparisons.
#'
#' FIX (Bug 2): unname() applied to ITEM_LABELS lookup so that the
#'   item_label column does not carry spurious names as tibble attributes.
run_dunn_posthoc <- function(sig_items, data_long, group_col) {
  map_df(sig_items, function(item_name) {
    df_tmp <- data_long %>%
      filter(item == item_name) %>%
      drop_na(value, !!sym(group_col))

    if (nrow(df_tmp) < 3 || n_distinct(df_tmp[[group_col]]) < 2) {
      return(tibble())
    }

    dt <- dunn.test(df_tmp$value, df_tmp[[group_col]],
                    method = "bonferroni", kw = FALSE, table = FALSE)

    tibble(
      item       = item_name,
      item_label = unname(ITEM_LABELS[item_name]),   # FIX: unname()
      comparison = dt$comparisons,
      z_stat     = dt$Z,
      p_value    = dt$P,
      p_adj      = dt$P.adjusted
    )
  })
}
