#' ============================================================
#' Publication Visualization Functions
#' ============================================================
#'
#' Core descriptive figures for the aphasia priority analysis.
#' These functions use one shared publication theme, color palette,
#' label formatter, and export helper so all figures are visually
#' consistent across the manuscript/supplement.
#'
#' Depends on: 00_config.R

# ── Shared publication style ───────────────────────────────
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

PUB_DOMAIN_PALETTE <- OKABE_ITO

PUB_GROUP_PALETTE <- c(
  PWA = "#0072B2",
  FMC = "#D55E00",
  `Family/Caregiver` = "#D55E00",
  Family = "#D55E00"
)

PUB_SOURCE_PALETTE <- c(
  Known = "#0072B2",
  New = "#D55E00",
  `Environmental scan` = "#0072B2",
  `Patient-generated` = "#D55E00"
)

PUB_SIGNIF_PALETTE <- c(
  `FALSE` = "#BDBDBD",
  `TRUE`  = "#333333"
)

format_domain_label <- function(x) {
  x %>%
    as.character() %>%
    str_remove("_mean$") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

publication_theme <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", size = rel(1.15), margin = margin(b = 4)),
      plot.subtitle = element_text(size = rel(0.95), color = "grey25", margin = margin(b = 8)),
      plot.caption = element_text(size = rel(0.8), color = "grey35", hjust = 0),
      axis.title = element_text(face = "bold", color = "grey15"),
      axis.text = element_text(color = "grey20"),
      axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.25, color = "grey88"),
      panel.grid.major.y = element_line(linewidth = 0.25, color = "grey90"),
      strip.text = element_text(face = "bold", color = "grey15"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key.width = unit(0.9, "lines"),
      plot.margin = margin(8, 12, 8, 8)
    )
}

scale_color_domain <- function(...) {
  scale_color_manual(values = PUB_DOMAIN_PALETTE, ...)
}

scale_fill_domain <- function(...) {
  scale_fill_manual(values = PUB_DOMAIN_PALETTE, ...)
}

scale_color_group <- function(...) {
  scale_color_manual(values = PUB_GROUP_PALETTE, ...)
}

scale_fill_group <- function(...) {
  scale_fill_manual(values = PUB_GROUP_PALETTE, ...)
}

scale_fill_source <- function(...) {
  scale_fill_manual(values = PUB_SOURCE_PALETTE, ...)
}

scale_color_source <- function(...) {
  scale_color_manual(values = PUB_SOURCE_PALETTE, ...)
}

save_publication_plot <- function(plot, filename, width = 7, height = 5,
                                  dpi = 600, bg = "white") {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename, plot = plot, width = width, height = height,
         dpi = dpi, bg = bg)
  invisible(filename)
}

# ── Core descriptive visualizations ───────────────────────
#' Item-level priority matrix: prevalence x mean score, faceted by group.
plot_priority_matrix <- function(results_compare) {
  results_compare %>%
    mutate(domain = factor(domain, levels = names(PUB_DOMAIN_PALETTE))) %>%
    ggplot(aes(x = prevalence, y = mean_score,
               color = domain, size = high_priority)) +
    geom_point(alpha = 0.85) +
    facet_wrap(~ group, ncol = 2) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_size_continuous(
      range = c(1.8, 7.5),
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_color_domain(labels = format_domain_label) +
    publication_theme() +
    labs(
      title    = "Priority Matrix by Stakeholder Group",
      subtitle = "Each point is one item. X = response prevalence; Y = mean rating (1–5); size = % high-priority (4–5).\nComposite priority score (mean × prevalence) is used for item rankings elsewhere.",
      x = "Response prevalence",
      y = "Mean rating (1–5)",
      color = "Domain",
      size = "% high-priority ratings"
    )
}

#' Domain-level priority matrix.
plot_domain_matrix <- function(domain_summary) {
  domain_summary %>%
    mutate(domain = factor(domain, levels = names(PUB_DOMAIN_PALETTE))) %>%
    ggplot(aes(x = mean_prevalence, y = mean_rating,
               color = domain, size = mean_high_priority)) +
    geom_point(alpha = 0.9) +
    facet_wrap(~ group) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_size_continuous(
      range = c(3, 10),
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_color_domain(labels = format_domain_label) +
    publication_theme() +
    labs(
      title    = "Domain-Level Priority Matrix",
      subtitle = "Mean domain rating (y) by response prevalence (x); point size = % high-priority (4–5) ratings.\nItems are ranked by composite priority score (mean × prevalence) in bar charts.",
      x = "Mean response prevalence",
      y = "Mean domain rating (1–5)",
      color = "Domain",
      size = "% high-priority ratings"
    )
}

#' Domain ranking bar chart.
plot_domain_bars <- function(domain_summary) {
  domain_summary %>%
    mutate(domain_label = format_domain_label(domain)) %>%
    ggplot(aes(x = reorder(domain_label, mean_priority_score),
               y = mean_priority_score, fill = group)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    #coord_flip() +
    scale_fill_group() +
    publication_theme() +
    theme(
      axis.text.x = element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5
      )
    ) +    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title    = "Domain Priority Scores by Stakeholder Group",
      subtitle = "Composite priority score = mean rating × response prevalence, averaged across items in each domain.",
      x = NULL,
      y = "Composite priority score",
      fill = "Group"
    )
}

#' Lollipop chart of largest group differences in priority score.
plot_difference_lollipop <- function(results_compare, n_each = 10) {
  groups <- sort(unique(results_compare$group))

  if (length(groups) != 2) {
    stop("plot_difference_lollipop expects exactly 2 groups; found: ",
         paste(groups, collapse = ", "))
  }

  g1 <- groups[1]
  g2 <- groups[2]

  diff_plot <- results_compare %>%
    select(item_label, group, priority_score) %>%
    pivot_wider(names_from = group, values_from = priority_score) %>%
    mutate(diff = .data[[g1]] - .data[[g2]]) %>%
    arrange(diff)

  top_diff <- bind_rows(
    diff_plot %>% slice_min(diff, n = n_each),
    diff_plot %>% slice_max(diff, n = n_each)
  ) %>%
    mutate(direction = if_else(diff > 0, paste("Higher in", g1), paste("Higher in", g2)))

  ggplot(top_diff, aes(x = diff, y = reorder(item_label, diff), color = direction)) +
    geom_segment(aes(x = 0, xend = diff, yend = item_label), linewidth = 0.8) +
    geom_point(size = 2.6) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.4, color = "grey35") +
    scale_color_manual(values = setNames(
      c(PUB_GROUP_PALETTE[[g1]] %||% "#0072B2",
        PUB_GROUP_PALETTE[[g2]] %||% "#D55E00"),
      c(paste("Higher in", g1), paste("Higher in", g2))
    )) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = paste0("Largest Item-Level Priority Differences (", g1, " − ", g2, ")"),
      subtitle = "Composite priority score = mean rating × response prevalence.",
      x = "Difference in composite priority score (mean × prevalence)",
      y = NULL,
      color = NULL
    )
}

#' Top-N bar chart for one group.
plot_top_n <- function(top_items, title) {
  top_items %>%
    mutate(domain = factor(domain, levels = names(PUB_DOMAIN_PALETTE))) %>%
    ggplot(aes(x = reorder(item_label, priority_score),
               y = priority_score, fill = domain)) +
    geom_col(width = 0.72) +
    coord_flip() +
    scale_fill_domain(labels = format_domain_label) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(title = title, x = NULL, y = "Composite priority score", fill = "Domain")
}

#' Venn diagram of shared vs unique top-15 items.
#' Note: VennDiagram is not ggplot-based, but colors are aligned with the publication palette.
plot_top15_venn <- function(top15_pwa, top15_fmc) {
  shared <- intersect(top15_pwa$item_label, top15_fmc$item_label)

  draw.pairwise.venn(
    area1      = nrow(top15_pwa),
    area2      = nrow(top15_fmc),
    cross.area = length(shared),
    category   = c("PWA Top 15", "FMC Top 15"),
    fill       = c(PUB_GROUP_PALETTE[["PWA"]], PUB_GROUP_PALETTE[["FMC"]]),
    alpha      = 0.45,
    cex        = 1.1,
    cat.cex    = 1.1,
    cat.fontface = "bold",
    cat.pos    = c(-20, 20)
  )
}

#' Summarise results_compare at the domain level.
summarise_domains <- function(results_compare) {
  results_compare %>%
    group_by(domain, group) %>%
    summarise(
      mean_rating         = mean(mean_score,      na.rm = TRUE),  # avg of item mean ratings
      mean_prevalence     = mean(prevalence,       na.rm = TRUE),
      mean_high_priority  = mean(high_priority,    na.rm = TRUE),
      mean_priority_score = mean(priority_score,   na.rm = TRUE),  # composite = mean × prevalence
      .groups = "drop"
    )
}


plot_top15_combined_overlap_known_new <- function(results_compare) {
  group_top15 <- results_compare %>%
    group_by(group) %>%
    arrange(desc(priority_score), .by_group = TRUE) %>%
    slice_head(n = 15) %>%
    ungroup() %>%
    select(group, item)
  
  pwa_top15 <- group_top15 %>% filter(group == "PWA") %>% pull(item)
  fmc_top15 <- group_top15 %>% filter(group == "FMC") %>% pull(item)
  
  combined <- results_compare %>%
    group_by(item, item_label, domain, source, known_new) %>%
    summarise(mean_priority = mean(priority_score, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      top15_overlap = case_when(
        item %in% pwa_top15 & item %in% fmc_top15 ~ "PWA + FMC top 15",
        item %in% pwa_top15 ~ "PWA top 15 only",
        item %in% fmc_top15 ~ "FMC top 15 only",
        TRUE ~ "Overall top 15 only"
      )
    ) %>%
    arrange(desc(mean_priority)) %>%
    slice_head(n = 15)
  
  ggplot(combined, aes(
    x = reorder(item_label, mean_priority),
    y = mean_priority,
    fill = domain
  )) +
    geom_col(width = 0.72) +
    geom_point(
      aes(shape = known_new, color = top15_overlap),
      y = 0,
      size = 3.2,
      stroke = 1.1
    ) +
    coord_flip() +
    scale_fill_domain(labels = format_domain_label) +
    scale_color_manual(values = c(
      "PWA + FMC top 15" = "#000000",
      "PWA top 15 only" = PUB_GROUP_PALETTE[["PWA"]],
      "FMC top 15 only" = PUB_GROUP_PALETTE[["FMC"]],
      "Overall top 15 only" = "grey55"
    )) +
    scale_shape_manual(values = c(
      "Known" = 16,
      "New" = 17
    )) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = "Overall Top 15 Priority Impacts with PWA/FMC Overlap",
      subtitle = "Bars retain domain colors. Marker color indicates PWA/FMC top-15 overlap; marker shape indicates known vs new.",
      x = NULL,
      y = "Mean composite priority score (mean × prevalence)",
      fill = "Domain",
      color = "Top-15 overlap",
      shape = "Known/new"
    )
}

# ============================================================
# Statistical / association visualization extensions
# Merged here intentionally: 04_visualizations.R owns ALL plotting.
# ============================================================

#' ============================================================
#' Analysis Visualization Helpers
#' ============================================================
#'
#' ggplot-based visualizations for statistical tests, group
#' comparisons, clinical/demographic associations, and known vs new.
#'
#' Depends on:
#'   00_config.R, 03_statistics.R, 06_group_comparisons.R,
#'   07_clinical_demographic.R, 09_novel_vs_known.R

# ── Shared helpers ─────────────────────────────────────────
# Uses publication_theme(), save_publication_plot(), and palettes from
# 04_visualizations.R. That file must be sourced before this one.

.theme_analysis <- function(base_size = 11) publication_theme(base_size = base_size)

.save_plot <- function(plot, filename, width = 7.5, height = 5.5, dpi = 600) {
  save_publication_plot(plot, filename, width = width, height = height, dpi = dpi)
}

.p_label <- function(p) {
  case_when(
    is.na(p)     ~ "NA",
    p < 0.001    ~ "p < .001",
    TRUE         ~ paste0("p = ", signif(p, 2))
  )
}

.sig_label <- function(p) {
  case_when(
    is.na(p)  ~ "NA",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ "ns"
  )
}

# ── Statistical test visualizations ────────────────────────
#' Domain-level Mann-Whitney results: effect size vs FDR-adjusted p-value.
plot_domain_mw_tests <- function(domain_tests) {
  domain_tests %>%
    mutate(
      domain_clean = str_remove(domain, "_mean$"),
      sig = p_adj_fdr < 0.05,
      label = .sig_label(p_adj_fdr)
    ) %>%
    ggplot(aes(x = reorder(domain_clean, rank_biserial),
               y = rank_biserial, fill = sig)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_text(aes(label = label, hjust = ifelse(rank_biserial >= 0, -0.15, 1.15)),
              size = 3, show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = PUB_SIGNIF_PALETTE) +
    .theme_analysis() +
    labs(
      title = "Domain-Level PWA vs Family/Caregiver Tests",
      subtitle = "Rank-biserial effect size; stars use FDR-adjusted p-values",
      x = "Domain", y = "Rank-biserial r", fill = "FDR < .05"
    )
}

#' Item-level Mann-Whitney results: top items by adjusted p-value.
plot_item_mw_tests <- function(item_tests, top_n = 25) {
  item_tests %>%
    arrange(p_adj_fdr) %>%
    slice_head(n = top_n) %>%
    mutate(
      sig = p_adj_fdr < 0.05,
      item_label = fct_reorder(item_label, rank_biserial)
    ) %>%
    ggplot(aes(x = item_label, y = rank_biserial, fill = domain, alpha = sig)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_fill_domain(labels = format_domain_label) +
    scale_alpha_manual(values = c("FALSE" = 0.55, "TRUE" = 1)) +
    .theme_analysis() +
    labs(
      title = paste("Top", top_n, "Item-Level PWA vs Family/Caregiver Tests"),
      subtitle = "Rank-biserial effect size; positive values indicate higher ratings in group 2 from the test output",
      x = "Item", y = "Rank-biserial r", fill = "Domain", alpha = "FDR < .05"
    )
}

#' Generic volcano-style view for tests with p-values and effect sizes.
plot_test_volcano <- function(test_df, label_col = "item_label", effect_col = "rank_biserial",
                              p_col = "p_adj_fdr", color_col = "domain",
                              title = "Statistical Test Volcano Plot") {
  test_df %>%
    filter(!is.na(.data[[effect_col]]), !is.na(.data[[p_col]])) %>%
    mutate(
      neg_log10_p = -log10(pmax(.data[[p_col]], .Machine$double.xmin)),
      sig = .data[[p_col]] < 0.05
    ) %>%
    ggplot(aes(x = .data[[effect_col]], y = neg_log10_p,
               color = .data[[color_col]], shape = sig)) +
    geom_point(size = 2.4, alpha = 0.8) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "dotted") +
    scale_color_domain(labels = format_domain_label) +
    .theme_analysis() +
    labs(
      title = title,
      x = effect_col,
      y = paste0("-log10(", p_col, ")"),
      color = "Domain", shape = "p < .05"
    )
}

# ── Group comparison visualizations ────────────────────────
#' Compare PWA and Family/Caregiver priority scores for every item.
plot_group_priority_scatter <- function(results_compare) {
  wide <- results_compare %>%
    select(item_label, domain, group, priority_score) %>%
    pivot_wider(names_from = group, values_from = priority_score)

  groups <- setdiff(names(wide), c("item_label", "domain"))
  if (length(groups) != 2) stop("Expected exactly two groups in results_compare.")

  ggplot(wide, aes(x = .data[[groups[1]]], y = .data[[groups[2]]], color = domain)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(size = 3, alpha = 0.85) +
    scale_color_domain(labels = format_domain_label) +
    coord_equal() +
    .theme_analysis() +
    labs(
      title    = "Item Composite Priority Scores by Stakeholder Group",
      subtitle = "Composite priority score = mean rating × response prevalence. Dashed line = equal priority between groups.",
      x = paste0(groups[1], " composite priority score"),
      y = paste0(groups[2], " composite priority score"),
      color = "Domain"
    )
}

#' Heatmap of high-priority proportions by group and item.
plot_high_priority_heatmap <- function(results_compare, top_n = 30) {
  keep_items <- results_compare %>%
    group_by(item_label) %>%
    summarise(avg_priority = mean(priority_score, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(avg_priority)) %>%
    slice_head(n = top_n) %>%
    pull(item_label)

  results_compare %>%
    filter(item_label %in% keep_items) %>%
    mutate(item_label = fct_reorder(item_label, priority_score, .fun = mean)) %>%
    ggplot(aes(x = group, y = item_label, fill = high_priority)) +
    geom_tile(color = "white") +
    scale_fill_gradient(
      low = "#F2F2F2", high = PUB_GROUP_PALETTE[["PWA"]],
      labels = scales::percent_format(accuracy = 1)
    ) +
    .theme_analysis() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title    = paste("High-Priority Rating Proportion — Top", top_n, "Items"),
      subtitle = "Items ordered by mean composite priority score (mean × prevalence); fill = % rated 4 or 5.",
      x = "Group", y = "Item", fill = "% high-priority (4–5)"
    )
}

# ── Clinical and demographic visualizations ────────────────
#' Plot KW results for a clinical/demographic bundle at domain or item level.
plot_kw_results <- function(kw_df, level = c("item", "domain"), top_n = 25,
                            title = NULL) {
  level <- match.arg(level)
  label_col <- if (level == "domain") "domain" else "item_label"

  plot_df <- kw_df %>%
    filter(!is.na(p_value)) %>%
    arrange(p_value) %>%
    slice_head(n = top_n)

  plot_df <- plot_df %>%
    mutate(
      label = fct_reorder(.data[[label_col]], eps_sq),
      sig = if ("p_adj_bonf" %in% names(plot_df)) {
        coalesce(p_adj_fdr < 0.05, FALSE) | coalesce(p_adj_bonf < 0.05, FALSE)
      } else {
        coalesce(p_adj_fdr < 0.05, FALSE)
      }
    )

  plot_df %>%
    ggplot(aes(x = label, y = eps_sq, fill = sig)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = PUB_SIGNIF_PALETTE) +
    .theme_analysis() +
    labs(
      title = title %||% paste("Kruskal-Wallis", str_to_title(level), "Associations"),
      subtitle = "Epsilon-squared effect sizes; darker bars indicate adjusted p < .05",
      x = str_to_title(level), y = "Epsilon-squared", fill = "Adjusted p < .05"
    )
}

#' Plot continuous clinical/demographic Spearman associations.
plot_spearman_associations <- function(cor_df, label_col = c("item_label", "domain"),
                                       top_n = 25, title = NULL) {
  label_col <- match.arg(label_col)

  # Domain-level tables use `spearman_rho`; item-level tables use `rho`.
  rho_col <- if ("spearman_rho" %in% names(cor_df)) {
    "spearman_rho"
  } else if ("rho" %in% names(cor_df)) {
    "rho"
  } else {
    stop("Expected a Spearman correlation column named 'spearman_rho' or 'rho'.")
  }

  cor_df %>%
    filter(!is.na(.data[[rho_col]]), !is.na(p_value)) %>%
    mutate(abs_rho = abs(.data[[rho_col]])) %>%
    arrange(desc(abs_rho)) %>%
    slice_head(n = top_n) %>%
    mutate(
      label = fct_reorder(.data[[label_col]], .data[[rho_col]]),
      sig = p_adj_fdr < 0.05
    ) %>%
    ggplot(aes(x = label, y = .data[[rho_col]], fill = sig)) +
    geom_col() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    scale_fill_manual(values = PUB_SIGNIF_PALETTE) +
    .theme_analysis() +
    labs(
      title = title %||% "Spearman Associations",
      subtitle = "Top associations by absolute rho; darker bars indicate FDR < .05",
      x = NULL, y = "Spearman rho", fill = "FDR < .05"
    )
}

#' Boxplots for item ratings by a categorical clinical/demographic variable.
plot_item_by_group_boxplot <- function(dat, group_var, item, title = NULL) {
  item_label <- coalesce(unname(ITEM_LABELS[item]), item)

  dat$df_clean %>%
    select(all_of(c(group_var, item))) %>%
    rename(group_cat = all_of(group_var), value = all_of(item)) %>%
    drop_na(group_cat, value) %>%
    ggplot(aes(x = group_cat, y = value)) +
    geom_boxplot(fill = "#D9D9D9", color = "grey25", alpha = 0.85, outlier.alpha = 0.45) +
    geom_jitter(width = 0.12, alpha = 0.25, size = 1) +
    .theme_analysis() +
    theme(legend.position = "none") +
    labs(
      title = title %||% paste(item_label, "by", group_var),
      x = group_var, y = "Rating"
    )
}

# ── Novel vs known visualizations ──────────────────────────
#' Known/new item metrics by source.
plot_known_new_metrics <- function(novel) {
  metric_labels <- c(
    mean_priority_score = "Composite priority score\n(mean × prevalence)",
    mean_impact         = "Mean rating (1–5)",
    mean_prevalence     = "Response prevalence",
    mean_high_priority  = "% high-priority ratings (4–5)"
  )

  novel$item_metrics_known_vs_new %>%
    pivot_longer(
      cols = c(mean_priority_score, mean_impact, mean_prevalence, mean_high_priority),
      names_to = "metric", values_to = "value"
    ) %>%
    mutate(metric = recode(metric, !!!metric_labels)) %>%
    ggplot(aes(x = known_new, y = value, fill = known_new)) +
    geom_col(width = 0.7) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_fill_source() +
    .theme_analysis() +
    theme(legend.position = "none", axis.text.x = element_text(angle = 0)) +
    labs(
      title    = "Known vs New Item Metrics",
      subtitle = "Composite priority score (mean × prevalence) is the primary ranking metric.",
      x = "Item source", y = NULL
    )
}

#' Overall top 10 known/new composition.
plot_known_new_top10 <- function(novel) {
  novel$top10 %>%
    mutate(item_label = fct_reorder(item_label, mean_priority)) %>%
    ggplot(aes(x = item_label, y = mean_priority, fill = known_new)) +
    geom_col() +
    coord_flip() +
    scale_fill_source() +
    .theme_analysis() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = "Overall Top 10 Items by Known/New Source",
      subtitle = "Items ranked by mean composite priority score (mean × prevalence) across groups.",
      x = "Item", y = "Mean composite priority score (mean × prevalence)", fill = "Source"
    )
}



# ── Mixed-model forest plots ───────────────────────────────
#' Forest plot for fixed effects from cumulative-link mixed models.
#'
#' @param mixed Output of run_mixed_models(), or a fixed-effects tibble with
#'   odds_ratio, ci_low, ci_high, p_value, term, and model columns.
#' @param models Character vector of model names to include. Defaults to all.
#' @param title Plot title.
#' @return A ggplot object.
plot_mixed_model_forest <- function(mixed, models = NULL,
                                    title = "Mixed-Effects Ordinal Regression: Fixed Effects") {
  fixed <- if (is.list(mixed) && "fixed_effects" %in% names(mixed)) {
    mixed$fixed_effects
  } else {
    mixed
  }

  if (is.null(fixed) || nrow(fixed) == 0) {
    return(
      ggplot() +
        publication_theme() +
        labs(title = title, subtitle = "No mixed-model fixed effects available.", x = NULL, y = NULL)
    )
  }

  if (!is.null(models)) {
    fixed <- fixed %>% filter(model %in% models)
  }

  plot_df <- fixed %>%
    filter(
      !is.na(odds_ratio), !is.na(ci_low), !is.na(ci_high),
      is.finite(odds_ratio), is.finite(ci_low), is.finite(ci_high)
    ) %>%
    mutate(
      model_label = model %>%
        str_replace_all("_", " ") %>%
        str_to_title(),
      term_label = term %>%
        str_replace("^respondent", "Group: ") %>%
        str_replace("^known_new", "Source: ") %>%
        str_replace("^domain", "Domain: ") %>%
        str_replace("^history_severity", "Severity: ") %>%
        str_replace("^race_ethnicity_collapsed", "Race/ethnicity: ") %>%
        str_replace("^age_group", "Age group: ") %>%
        str_replace("^urban", "Urbanicity: ") %>%
        str_replace("^region", "Region: ") %>%
        str_replace("^ses", "SES: ") %>%
        str_replace("^mpo", "Months post-onset") %>%
        str_replace("^age", "Age") %>%
        str_replace_all(":", ": ") %>%
        str_replace_all("\\.", " ") %>%
        str_squish(),
      sig = p_value < 0.05,
      term_label = fct_reorder(term_label, odds_ratio)
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot() +
        publication_theme() +
        labs(title = title, subtitle = "No plottable mixed-model fixed effects available.", x = NULL, y = NULL)
    )
  }

  ggplot(plot_df, aes(x = odds_ratio, y = term_label, color = sig)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.18, linewidth = 0.7) +
    geom_point(size = 2.4) +
    facet_wrap(~ model_label, scales = "free_y") +
    scale_x_log10() +
    scale_color_manual(values = PUB_SIGNIF_PALETTE) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") +
    labs(
      title = title,
      subtitle = "Odds ratios > 1 indicate higher odds of endorsing a higher rating; bars show Wald 95% CIs.",
      x = "Odds ratio (log scale)",
      y = NULL,
      color = "p < .05"
    )
}

#' Domain-specific forest plot for the PWA vs FMC group effect.
#'
#' @param mixed Output of run_mixed_models(), or a domain-effects tibble.
#' @param title Plot title.
#' @return A ggplot object.
plot_mixed_domain_forest <- function(mixed,
                                     title = "Domain-Specific PWA vs FMC Mixed-Model Effects") {
  domain_effects <- if (is.list(mixed) && "domain_effects" %in% names(mixed)) {
    mixed$domain_effects
  } else {
    mixed
  }

  if (is.null(domain_effects) || nrow(domain_effects) == 0) {
    return(
      ggplot() +
        publication_theme() +
        labs(title = title, subtitle = "No domain-stratified mixed-model effects available.", x = NULL, y = NULL)
    )
  }

  plot_df <- domain_effects %>%
    filter(
      term == "respondentFMC",
      !is.na(odds_ratio), !is.na(ci_low), !is.na(ci_high),
      is.finite(odds_ratio), is.finite(ci_low), is.finite(ci_high)
    ) %>%
    mutate(
      domain_label = fct_reorder(format_domain_label(domain), odds_ratio),
      sig = p_value < 0.05
    )

  if (nrow(plot_df) == 0) {
    return(
      ggplot() +
        publication_theme() +
        labs(title = title, subtitle = "No plottable PWA vs FMC domain effects available.", x = NULL, y = NULL)
    )
  }

  ggplot(plot_df, aes(x = odds_ratio, y = domain_label, color = sig)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey45") +
    geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), height = 0.18, linewidth = 0.7) +
    geom_point(size = 2.8) +
    scale_x_log10() +
    scale_color_manual(values = PUB_SIGNIF_PALETTE) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") +
    labs(
      title = title,
      subtitle = "FMC vs PWA odds ratios from domain-stratified cumulative-link mixed models.",
      x = "Odds ratio for FMC vs PWA (log scale)",
      y = "Domain",
      color = "p < .05"
    )
}


# ── Distribution plots for nonparametric tests ─────────────
# These functions pair the statistical tests with distribution views.
# Violin + box + jitter is used consistently for publication figures.

.group_label_vector <- function(df) {
  if ("respondent" %in% names(df)) {
    as.character(df$respondent)
  } else if ("group" %in% names(df)) {
    as.character(df$group)
  } else if ("stakeholder_group" %in% names(df)) {
    case_when(
      df$stakeholder_group == 1 ~ "PWA",
      df$stakeholder_group == 2 ~ "FMC",
      TRUE ~ as.character(df$stakeholder_group)
    )
  } else {
    stop("Expected respondent, group, or stakeholder_group column.")
  }
}

.format_test_label <- function(p, prefix = "FDR") {
  case_when(
    is.na(p)  ~ paste0(prefix, " p = NA"),
    p < 0.001 ~ paste0(prefix, " p < .001"),
    TRUE      ~ paste0(prefix, " p = ", signif(p, 2))
  )
}

.distribution_layers <- function() {
  list(
    geom_violin(trim = FALSE, alpha = 0.35, linewidth = 0.35, color = "grey35"),
    geom_boxplot(width = 0.16, outlier.shape = NA, alpha = 0.85, linewidth = 0.35, color = "grey25"),
    geom_jitter(width = 0.08, height = 0.03, alpha = 0.28, size = 0.75, color = "grey20")
  )
}

#' Violin + box plots for domain-level Mann-Whitney comparisons.
plot_mw_domain_distributions <- function(domain_means, domain_tests) {
  domain_cols <- names(domain_means)[str_detect(names(domain_means), "_mean$")]

  plot_df <- domain_means %>%
    mutate(group_label = factor(.group_label_vector(.), levels = c("PWA", "FMC"))) %>%
    select(group_label, all_of(domain_cols)) %>%
    pivot_longer(all_of(domain_cols), names_to = "domain", values_to = "score") %>%
    drop_na(group_label, score) %>%
    mutate(domain_label = format_domain_label(domain))

  ann <- plot_df %>%
    group_by(domain, domain_label) %>%
    summarise(y = min(5.25, max(score, na.rm = TRUE) + 0.35), .groups = "drop") %>%
    left_join(domain_tests %>% select(domain, p_value, p_adj_fdr, rank_biserial), by = "domain") %>%
    mutate(
      x = "PWA",
      label = paste0(.format_test_label(p_adj_fdr, "FDR"), "\nr = ", round(rank_biserial, 2))
    )

  ggplot(plot_df, aes(x = group_label, y = score, fill = group_label)) +
    .distribution_layers() +
    geom_text(data = ann, aes(x = x, y = y, label = label), inherit.aes = FALSE,
              hjust = 0, vjust = 1, size = 2.7, lineheight = 0.9) +
    facet_wrap(~ domain_label, ncol = 3) +
    scale_fill_group(drop = FALSE) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5.35)) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0), legend.position = "none") +
    labs(
      title = "Domain Ratings by Stakeholder Group",
      subtitle = "Violin + box plots show distributions; annotations show Mann-Whitney FDR p-values and rank-biserial r.",
      x = "Stakeholder group", y = "Domain mean rating"
    )
}

#' Violin + box plots for top item-level Mann-Whitney comparisons.
plot_mw_item_distributions <- function(dat, item_tests, top_n = 12) {
  top_tests <- item_tests %>%
    filter(!is.na(p_adj_fdr)) %>%
    arrange(p_adj_fdr, p_value) %>%
    slice_head(n = top_n) %>%
    mutate(item_label = coalesce(item_label, item))

  keep_items <- top_tests$item

  plot_df <- dat$df_clean %>%
    mutate(group_label = factor(.group_label_vector(.), levels = c("PWA", "FMC"))) %>%
    select(group_label, all_of(keep_items)) %>%
    pivot_longer(all_of(keep_items), names_to = "item", values_to = "rating") %>%
    drop_na(group_label, rating) %>%
    left_join(top_tests %>% select(item, item_label, domain), by = "item") %>%
    mutate(item_label = fct_reorder(item_label, rating, .fun = median, na.rm = TRUE))

  ann <- plot_df %>%
    group_by(item, item_label) %>%
    summarise(y = 5.35, .groups = "drop") %>%
    left_join(top_tests %>% select(item, p_adj_fdr, rank_biserial), by = "item") %>%
    mutate(
      x = "PWA",
      label = paste0(.format_test_label(p_adj_fdr, "FDR"), "\nr = ", round(rank_biserial, 2))
    )

  ggplot(plot_df, aes(x = group_label, y = rating, fill = group_label)) +
    .distribution_layers() +
    geom_text(data = ann, aes(x = x, y = y, label = label), inherit.aes = FALSE,
              hjust = 0, vjust = 1, size = 2.5, lineheight = 0.9) +
    facet_wrap(~ item_label, ncol = 3) +
    scale_fill_group(drop = FALSE) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5.45)) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0), legend.position = "none") +
    labs(
      title = paste0("Top ", top_n, " Item Ratings by Stakeholder Group"),
      subtitle = "Violin + box plots show distributions; annotations show Mann-Whitney FDR p-values and rank-biserial r.",
      x = "Stakeholder group", y = "Item rating"
    )
}

#' Violin + box plots for domain-level Kruskal-Wallis tests.
plot_kw_domain_distributions <- function(domain_means, kw_df, group_var, label, top_n = 9) {
  if (!group_var %in% names(domain_means) || is.null(kw_df) || nrow(kw_df) == 0) {
    return(ggplot() + publication_theme() + labs(title = paste(label, "domain distributions"), subtitle = "No data available."))
  }

  keep_domains <- kw_df %>%
    filter(!is.na(p_value)) %>%
    arrange(p_adj_fdr, p_value) %>%
    slice_head(n = top_n) %>%
    pull(domain)

  plot_df <- domain_means %>%
    select(all_of(c(group_var, keep_domains))) %>%
    pivot_longer(all_of(keep_domains), names_to = "domain", values_to = "score") %>%
    rename(group_cat = all_of(group_var)) %>%
    drop_na(group_cat, score) %>%
    mutate(
      group_cat = as.factor(group_cat),
      domain_label = format_domain_label(domain)
    )

  first_levels <- plot_df %>%
    group_by(domain) %>%
    summarise(x = levels(droplevels(group_cat))[1], .groups = "drop")

  ann <- plot_df %>%
    group_by(domain, domain_label) %>%
    summarise(y = min(5.25, max(score, na.rm = TRUE) + 0.35), .groups = "drop") %>%
    left_join(first_levels, by = "domain") %>%
    left_join(kw_df %>% select(domain, p_value, p_adj_fdr, eps_sq), by = "domain") %>%
    mutate(label_txt = paste0(.format_test_label(p_adj_fdr, "FDR"), "\nε² = ", round(eps_sq, 2)))

  ggplot(plot_df, aes(x = group_cat, y = score, fill = group_cat)) +
    .distribution_layers() +
    geom_text(data = ann, aes(x = x, y = y, label = label_txt), inherit.aes = FALSE,
              hjust = 0, vjust = 1, size = 2.5, lineheight = 0.9) +
    facet_wrap(~ domain_label, ncol = 3) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5.35)) +
    publication_theme() +
    theme(legend.position = "none") +
    labs(
      title = paste0("Domain Ratings by ", label),
      subtitle = "Violin + box plots show distributions; annotations show Kruskal-Wallis FDR p-values and epsilon-squared.",
      x = label, y = "Domain mean rating"
    )
}

#' Violin + box plots for item-level Kruskal-Wallis tests.
plot_kw_item_distributions <- function(dat, kw_df, group_var, label, top_n = 12) {
  if (!group_var %in% names(dat$df_clean) || is.null(kw_df) || nrow(kw_df) == 0) {
    return(ggplot() + publication_theme() + labs(title = paste(label, "item distributions"), subtitle = "No data available."))
  }

  top_tests <- kw_df %>%
    filter(!is.na(p_adj_fdr)) %>%
    arrange(p_adj_fdr, p_value) %>%
    slice_head(n = top_n) %>%
    mutate(item_label = coalesce(item_label, item))

  keep_items <- top_tests$item

  plot_df <- dat$df_clean %>%
    select(all_of(c(group_var, keep_items))) %>%
    pivot_longer(all_of(keep_items), names_to = "item", values_to = "rating") %>%
    rename(group_cat = all_of(group_var)) %>%
    drop_na(group_cat, rating) %>%
    mutate(group_cat = as.factor(group_cat)) %>%
    left_join(top_tests %>% select(item, item_label, domain), by = "item") %>%
    mutate(item_label = fct_reorder(item_label, rating, .fun = median, na.rm = TRUE))

  first_levels <- plot_df %>%
    group_by(item) %>%
    summarise(x = levels(droplevels(group_cat))[1], .groups = "drop")

  ann <- plot_df %>%
    group_by(item, item_label) %>%
    summarise(y = 5.35, .groups = "drop") %>%
    left_join(first_levels, by = "item") %>%
    left_join(top_tests %>% select(item, p_value, p_adj_fdr, eps_sq), by = "item") %>%
    mutate(label_txt = paste0(.format_test_label(p_adj_fdr, "FDR"), "\nε² = ", round(eps_sq, 2)))

  ggplot(plot_df, aes(x = group_cat, y = rating, fill = group_cat)) +
    .distribution_layers() +
    geom_text(data = ann, aes(x = x, y = y, label = label_txt), inherit.aes = FALSE,
              hjust = 0, vjust = 1, size = 2.45, lineheight = 0.9) +
    facet_wrap(~ item_label, ncol = 3) +
    scale_y_continuous(breaks = 1:5, limits = c(1, 5.45)) +
    publication_theme() +
    theme(legend.position = "none") +
    labs(
      title = paste0("Top ", top_n, " Item Ratings by ", label),
      subtitle = "Violin + box plots show distributions; annotations show Kruskal-Wallis FDR p-values and epsilon-squared.",
      x = label, y = "Item rating"
    )
}


# ── One-call exporter ──────────────────────────────────────
#' Generate and save the main analysis visualization set.
#'
#' @return Invisibly returns a named character vector of saved file paths.
export_analysis_visualizations <- function(results, domain_summary, domain_tests,
                                           item_tests, hp, clin_demo, novel,
                                           dat = NULL, domain_means = NULL,
                                           mixed = NULL,
                                           out_dir = "output/figures") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    domain_mw_tests = plot_domain_mw_tests(domain_tests),
    item_mw_tests = plot_item_mw_tests(item_tests, top_n = 25),
    item_mw_volcano = plot_test_volcano(
      item_tests,
      title = "Item-Level PWA vs Family/Caregiver Test Volcano Plot"
    ),
    group_priority_scatter = plot_group_priority_scatter(results),
    high_priority_heatmap = plot_high_priority_heatmap(results, top_n = 30),
    known_new_metrics = plot_known_new_metrics(novel),
    known_new_top10 = plot_known_new_top10(novel)
  )

  # Distribution views for Mann-Whitney tests.
  if (!is.null(domain_means)) {
    plots$mw_domain_violin_box <- plot_mw_domain_distributions(domain_means, domain_tests)
  }
  if (!is.null(dat)) {
    plots$mw_item_violin_box <- plot_mw_item_distributions(dat, item_tests, top_n = 12)
  }

  # Mixed-model forest plots, when mixed-model output is supplied.
  if (!is.null(mixed)) {
    plots$mixed_model_forest <- plot_mixed_model_forest(
      mixed,
      models = c("primary_group", "adjusted_group_demographics", "pwa_clinical_demographic")
    )
    plots$mixed_model_domain_forest <- plot_mixed_domain_forest(mixed)
  }

  # Clinical/demographic KW views
  clinical_kw <- list(
    severity_domain_kw = clin_demo$severity_domains$kw,
    severity_item_kw   = clin_demo$severity_items$kw,
    age_domain_kw      = clin_demo$age_group$domains$kw,
    age_item_kw        = clin_demo$age_group$items$kw,
    ses_domain_kw      = clin_demo$ses$domains$kw,
    ses_item_kw        = clin_demo$ses$items$kw,
    region_domain_kw   = clin_demo$region$domains$kw,
    region_item_kw     = clin_demo$region$items$kw,
    urban_domain_kw    = clin_demo$urban$domains$kw,
    urban_item_kw      = clin_demo$urban$items$kw,
    race_domain_kw     = clin_demo$race$domains$kw,
    race_item_kw       = clin_demo$race$items$kw
  )

  for (nm in names(clinical_kw)) {
    df <- clinical_kw[[nm]]
    if (!is.null(df) && nrow(df) > 0) {
      lvl <- if (str_detect(nm, "domain")) "domain" else "item"
      plots[[nm]] <- plot_kw_results(
        df,
        level = lvl,
        top_n = ifelse(lvl == "domain", 20, 25),
        title = str_replace_all(str_to_title(nm), "_", " ")
      )
    }
  }

  # Distribution views for Kruskal-Wallis tests.
  # Severity is PWA-only in clin_demo, so use a PWA-only dat object for item plots.
  if (!is.null(dat) && !is.null(domain_means)) {
    dat_pwa <- dat
    dat_pwa$df_clean <- dat$df_clean %>% filter(stakeholder_group == 1)
    domain_means_pwa <- domain_means %>% filter(stakeholder_group == 1)

    kw_specs <- list(
      severity = list(group_var = "history_severity", label = "Aphasia Severity", dat_obj = dat_pwa, dm_obj = domain_means_pwa,
                      domains = clin_demo$severity_domains$kw, items = clin_demo$severity_items$kw),
      age_group = list(group_var = "age_group", label = "Age Group", dat_obj = dat, dm_obj = domain_means,
                       domains = clin_demo$age_group$domains$kw, items = clin_demo$age_group$items$kw),
      ses = list(group_var = "ses", label = "SES", dat_obj = dat, dm_obj = domain_means,
                 domains = clin_demo$ses$domains$kw, items = clin_demo$ses$items$kw),
      region = list(group_var = "region", label = "Region", dat_obj = dat, dm_obj = domain_means,
                    domains = clin_demo$region$domains$kw, items = clin_demo$region$items$kw),
      urban = list(group_var = "urban", label = "Urbanicity", dat_obj = dat, dm_obj = domain_means,
                   domains = clin_demo$urban$domains$kw, items = clin_demo$urban$items$kw),
      race = list(group_var = "race_ethnicity_collapsed", label = "Race/Ethnicity", dat_obj = dat, dm_obj = domain_means,
                  domains = clin_demo$race$domains$kw, items = clin_demo$race$items$kw)
    )

    for (nm in names(kw_specs)) {
      spec <- kw_specs[[nm]]
      if (!is.null(spec$domains) && nrow(spec$domains) > 0 && spec$group_var %in% names(spec$dm_obj)) {
        plots[[paste0("kw_", nm, "_domain_violin_box")]] <- plot_kw_domain_distributions(
          spec$dm_obj, spec$domains, spec$group_var, spec$label, top_n = 9
        )
      }
      if (!is.null(spec$items) && nrow(spec$items) > 0 && spec$group_var %in% names(spec$dat_obj$df_clean)) {
        plots[[paste0("kw_", nm, "_item_violin_box")]] <- plot_kw_item_distributions(
          spec$dat_obj, spec$items, spec$group_var, spec$label, top_n = 12
        )
      }
    }
  }

  # Continuous association views
  continuous <- list(
    severity_domain_spearman = clin_demo$severity_domain_cor,
    age_domain_spearman_all  = clin_demo$age_domain_cor,
    age_domain_spearman_pwa  = clin_demo$age_domain_cor_pwa,
    age_domain_spearman_fmc  = clin_demo$age_domain_cor_fmc,
    mpo_domain_spearman_pwa  = clin_demo$mpo_domain_cor,
    age_item_spearman_all    = clin_demo$age,
    age_item_spearman_pwa    = clin_demo$age_pwa,
    age_item_spearman_fmc    = clin_demo$age_fmc,
    mpo_item_spearman_pwa    = clin_demo$mpo
  )

  for (nm in names(continuous)) {
    df <- continuous[[nm]]
    if (!is.null(df) && nrow(df) > 0) {
      label_col <- if ("item_label" %in% names(df)) "item_label" else "domain"
      plots[[nm]] <- plot_spearman_associations(
        df,
        label_col = label_col,
        top_n = ifelse(label_col == "domain", 20, 25),
        title = str_replace_all(str_to_title(nm), "_", " ")
      )
    }
  }

  paths <- map_chr(names(plots), function(nm) {
    width <- case_when(
      str_detect(nm, "violin_box") & str_detect(nm, "item") ~ 10,
      str_detect(nm, "violin_box") & str_detect(nm, "domain") ~ 9,
      TRUE ~ 7.5
    )
    height <- case_when(
      str_detect(nm, "violin_box") & str_detect(nm, "item") ~ 8.5,
      str_detect(nm, "violin_box") & str_detect(nm, "domain") ~ 7,
      TRUE ~ 5.5
    )
    .save_plot(plots[[nm]], file.path(out_dir, paste0(nm, ".png")), width = width, height = height)
  })
  names(paths) <- names(plots)

  invisible(paths)
}

plot_top15_combined <- function(results_compare) {
  combined <- results_compare %>%
    group_by(item, item_label, domain, source, known_new) %>%
    summarise(
      mean_priority = mean(priority_score, na.rm = TRUE),
      mean_score = mean(mean_score, na.rm = TRUE),
      mean_prevalence = mean(prevalence, na.rm = TRUE),
      mean_high_priority = mean(high_priority, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_priority)) %>%
    slice_head(n = 15)
  
  ggplot(combined, aes(x = reorder(item_label, mean_priority),
                       y = mean_priority, fill = domain)) +
    geom_col(width = 0.72) +
    coord_flip() +
    scale_fill_domain(labels = format_domain_label) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = "Top 15 Priority Impacts Across All Participants",
      subtitle = "Items ranked by mean composite priority score (mean rating × prevalence) across PWA and Family/Caregiver groups.",
      x = NULL,
      y = "Mean composite priority score (mean × prevalence)",
      fill = "Domain"
    )
}


plot_mpo_domain_trends <- function(dat, domain_means) {
  domain_cols <- c(
    "symptom_mean", "costs_mean", "emotions_mean", "social_mean",
    "cognition_mean", "treatment_mean", "qol_mean",
    "resources_mean", "impact_others_mean"
  )
  
  domain_means %>%
    filter(stakeholder_group == 1, !is.na(mpo)) %>%
    select(mpo, all_of(domain_cols)) %>%
    pivot_longer(-mpo, names_to = "domain", values_to = "score") %>%
    drop_na(score) %>%
    mutate(domain = str_remove(domain, "_mean$")) %>%
    ggplot(aes(x = mpo, y = score)) +
    geom_point(alpha = 0.35, size = 1.5) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 0.8) +
    facet_wrap(~ format_domain_label(domain), ncol = 3) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = "Domain Mean Ratings by Months Post-Onset",
      subtitle = "PWA only; smoothed trends show whether ratings change over recovery time. Note: composite priority score also depends on prevalence.",
      x = "Months post-onset",
      y = "Domain mean rating (1–5)"
    )
}

plot_mpo_top_item_trends <- function(dat, mpo_assoc, top_n = 12) {
  keep_items <- mpo_assoc %>%
    filter(!is.na(rho)) %>%
    mutate(abs_rho = abs(rho)) %>%
    arrange(desc(abs_rho)) %>%
    slice_head(n = top_n) %>%
    pull(item)
  
  all_items <- unlist(dat$domains, use.names = FALSE)
  
  dat$df_clean %>%
    filter(stakeholder_group == 1, !is.na(mpo)) %>%
    select(mpo, all_of(intersect(all_items, keep_items))) %>%
    pivot_longer(-mpo, names_to = "item", values_to = "value") %>%
    drop_na(value) %>%
    left_join(
      tibble(item = names(ITEM_LABELS), item_label = unname(ITEM_LABELS)),
      by = "item"
    ) %>%
    mutate(item_label = coalesce(item_label, item)) %>%
    ggplot(aes(x = mpo, y = value)) +
    geom_jitter(height = 0.08, alpha = 0.25, size = 1.2) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 0.8) +
    facet_wrap(~ item_label, ncol = 3) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = "Item Ratings by Months Post-Onset",
      subtitle = paste("PWA only; top", top_n, "items by absolute MPO–rating association (Spearman rho). Note: composite score also depends on prevalence."),
      x = "Months post-onset",
      y = "Item rating (1–5)"
    )
}

# ── Estimated marginal means / predicted probabilities ─────

.require_emmeans <- function() {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop(
      "Package 'emmeans' is required. Install with install.packages('emmeans').",
      call. = FALSE
    )
  }
}

get_clmm_predicted_probs <- function(mixed, model_name, specs) {
  .require_emmeans()
  
  model <- mixed$models[[model_name]]
  
  if (is.null(model)) {
    return(tibble())
  }
  
  emmeans::emmeans(
    model,
    specs = specs,
    mode = "prob"
  ) %>%
    as_tibble() %>%
    mutate(model = model_name, .before = 1)
}

plot_predicted_rating_probs <- function(pred_df,
                                        title = "Predicted Rating Probabilities") {
  if (is.null(pred_df) || nrow(pred_df) == 0) {
    return(
      ggplot() +
        publication_theme() +
        labs(title = title, subtitle = "No predicted probabilities available.")
    )
  }
  
  rating_col <- intersect(names(pred_df), c("rating", "response", "y.level"))[1]
  
  if (is.na(rating_col)) {
    stop("Could not identify the rating-category column in emmeans output.")
  }
  
  group_cols <- setdiff(
    names(pred_df),
    c("model", rating_col, "prob", "SE", "df", "asymp.LCL", "asymp.UCL",
      "lower.CL", "upper.CL")
  )
  
  pred_df %>%
    mutate(
      rating = factor(.data[[rating_col]], levels = sort(unique(.data[[rating_col]]))),
      group_label = interaction(across(all_of(group_cols)), sep = " / ", drop = TRUE)
    ) %>%
    ggplot(aes(x = rating, y = prob, fill = group_label)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(
      aes(ymin = asymp.LCL, ymax = asymp.UCL),
      position = position_dodge(width = 0.8),
      width = 0.18,
      linewidth = 0.4
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = title,
      subtitle = "Model-estimated probability of each ordinal rating category.",
      x = "Rating category",
      y = "Predicted probability",
      fill = NULL
    )
}

plot_high_priority_predicted_prob <- function(pred_df,
                                              title = "Predicted Probability of High-Priority Ratings") {
  if (is.null(pred_df) || nrow(pred_df) == 0) {
    return(
      ggplot() +
        publication_theme() +
        labs(title = title, subtitle = "No predicted probabilities available.")
    )
  }
  
  rating_col <- intersect(names(pred_df), c("rating", "response", "y.level"))[1]
  
  high_df <- pred_df %>%
    mutate(rating_num = suppressWarnings(as.numeric(as.character(.data[[rating_col]])))) %>%
    filter(rating_num %in% c(4, 5)) %>%
    group_by(across(-c(prob, SE, asymp.LCL, asymp.UCL, rating_num, all_of(rating_col)))) %>%
    summarise(
      prob_high = sum(prob, na.rm = TRUE),
      .groups = "drop"
    )
  
  group_cols <- setdiff(names(high_df), c("model", "prob_high"))
  
  high_df %>%
    mutate(group_label = interaction(across(all_of(group_cols)), sep = " / ", drop = TRUE)) %>%
    ggplot(aes(x = group_label, y = prob_high, fill = group_label)) +
    geom_col(width = 0.65, show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 25, hjust = 1)) +
    labs(
      title = title,
      subtitle = "Predicted probability of rating an item as 4 or 5.",
      x = NULL,
      y = "Predicted Pr(rating 4–5)"
    )
}