#' Publication visualizations: core impact set, group comparisons, demographics
#'
#' Depends on: 00_config.R (OKABE_ITO, GROUP_PALETTE, GROUP_LABELS, format_domain_label)

# Self-sourcing guard (see 02_load_clean.R for the full explanation).
if (!exists("OKABE_ITO")) {
  if (file.exists("R/00_config.R")) source("R/00_config.R") else stop(
    "OKABE_ITO not found and R/00_config.R not found from the current ",
    "working directory (", getwd(), "). Run setwd() to the project root, ",
    "or source main.R from the top instead of this file on its own."
  )
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
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linewidth = 0.25, color = "grey88"),
      panel.grid.major.y = element_line(linewidth = 0.25, color = "grey90"),
      strip.text = element_text(face = "bold", color = "grey15"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.margin = margin(8, 12, 8, 8)
    )
}

#' Placeholder plot shown when there's no non-missing data to visualize
#' (e.g. an item-rating export with demographics but no responses yet).
.empty_plot <- function(msg = "No data available yet.") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 5, color = "grey40") +
    theme_void()
}

scale_fill_domain  <- function(...) scale_fill_manual(values = OKABE_ITO, ...)
scale_fill_group   <- function(...) scale_fill_manual(values = GROUP_PALETTE, labels = GROUP_LABELS, ...)
scale_color_group  <- function(...) scale_color_manual(values = GROUP_PALETTE, labels = GROUP_LABELS, ...)

save_publication_plot <- function(plot, filename, width = 7, height = 5, dpi = 600, bg = "white") {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename, plot = plot, width = width, height = height, dpi = dpi, bg = bg)
  invisible(filename)
}

#' Full 40-item core impact set, ranked, colored by domain.
plot_core_impact_set <- function(core_set, title = "Core Impact Set — All Respondents") {
  if (all(is.na(core_set$priority_score))) {
    return(.empty_plot("No item ratings yet — nothing to rank."))
  }
  core_set %>%
    filter(!is.na(priority_score)) %>%
    mutate(domain = factor(domain, levels = names(OKABE_ITO))) %>%
    ggplot(aes(x = reorder(item_label, priority_score), y = priority_score, fill = domain)) +
    geom_col(width = 0.72) +
    coord_flip() +
    scale_fill_domain(labels = format_domain_label) +
    publication_theme() +
    labs(title = title,
         subtitle = "All 40 items ranked by composite priority score (mean rating x prevalence).",
         x = NULL, y = "Composite priority score", fill = "Domain")
}

#' Top-N items faceted by group, for direct visual comparison.
plot_top_n_by_group <- function(group_results, n = 15,
                                title = "Top Priority Items by Interest-holder Group") {
  top_by_group <- group_results %>%
    filter(!is.na(priority_score)) %>%
    group_by(group) %>%
    slice_max(priority_score, n = n) %>%
    ungroup() %>%
    mutate(
      group = factor(group, levels = GROUP_LEVELS),
      domain = factor(domain, levels = names(OKABE_ITO))
    )
  
  if (nrow(top_by_group) == 0) {
    return(.empty_plot("No item ratings yet — nothing to rank by group."))
  }
  
  ggplot(top_by_group, aes(x = reorder_within(item_label, priority_score, group),
                           y = priority_score, fill = domain)) +
    geom_col(width = 0.72) +
    coord_flip() +
    tidytext_reorder_scale() +
    facet_wrap(~ group, scales = "free_y",
               labeller = as_labeller(GROUP_LABELS)) +
    scale_fill_domain(labels = format_domain_label) +
    publication_theme() +
    labs(title = title,
         subtitle = paste0("Top ", n, " items by composite priority score, within each group."),
         x = NULL, y = "Composite priority score", fill = "Domain") }

# Lightweight stand-in for tidytext::reorder_within/scale_x_reordered so this
# pipeline doesn't need an extra dependency.
reorder_within <- function(x, by, within) {
  new_x <- paste(x, within, sep = "___")
  stats::reorder(new_x, by)
}
tidytext_reorder_scale <- function() {
  scale_x_discrete(labels = function(x) sub("___.*$", "", x))
}

#' Dumbbell / slope plot: how a single item's priority score compares
#' across the three groups. Good for the top-N most divergent items.
plot_group_divergence <- function(comparison_table, n = 15,
                                  title = "Items Where Groups Diverge Most") {
  top_div <- comparison_table %>%
    filter(!is.na(max_group_rank_gap)) %>%
    arrange(desc(max_group_rank_gap)) %>%
    slice_head(n = n)
  
  if (nrow(top_div) == 0) {
    return(.empty_plot("No item ratings yet — nothing to compare across groups."))
  }
  
  long <- top_div %>%
    select(item_label, starts_with("priority_")) %>%
    pivot_longer(starts_with("priority_"), names_to = "group", values_to = "priority_score") %>%
    mutate(group = str_remove(group, "^priority_"),
           group = factor(group, levels = GROUP_LEVELS)) %>%
    filter(!is.na(priority_score))
  
  ggplot(long, aes(x = priority_score, y = fct_reorder(item_label, priority_score, .fun = max),
                   color = group)) +
    geom_line(aes(group = item_label), color = "grey75", linewidth = 0.6) +
    geom_point(size = 3) +
    scale_color_group() +
    publication_theme() +
    labs(title = title,
         subtitle = "Items with the largest rank gap between groups' composite priority scores.",
         x = "Composite priority score", y = NULL, color = "Group")
}

#' Bump/slope chart: rank of the top items overall across PWA / FMC / CR.
#' "Top" is determined by each item's pooled composite priority score
#' (i.e. matches core_impact_overall / score_pooled() — top n items
#' overall, not any single group's ranking), then each of those items'
#' rank *within its own group* is plotted across the three groups so you
#' can see how much it shifts.
#'
#' @param group_results   Output of score_by_group() (item x group rows,
#'                        with columns item, item_label, group, priority_score).
#' @param pooled_results  Output of score_pooled() — used to pick which
#'                        items count as "top n" (pooled priority_score,
#'                        matching core_impact_overall). If omitted, falls
#'                        back to an unweighted average of the three
#'                        groups' priority scores instead.
#' @param top_n           How many items to include.
#' @param title           Plot title.
plot_rank_slope <- function(group_results, pooled_results = NULL, top_n = 15,
                            title = paste0("Rank Shift: Top ", top_n, " Items Across Groups")) {
  ranked <- group_results %>%
    filter(!is.na(priority_score), group %in% GROUP_LEVELS) %>%
    mutate(group = factor(group, levels = GROUP_LEVELS)) %>%
    group_by(group) %>%
    mutate(rank = min_rank(desc(priority_score))) %>%
    ungroup()
  
  if (nrow(ranked) == 0) {
    return(.empty_plot("No item ratings yet — nothing to rank across groups."))
  }
  
  n_items <- n_distinct(ranked$item)
  
  if (!is.null(pooled_results)) {
    item_avg <- pooled_results %>%
      filter(!is.na(priority_score)) %>%
      distinct(item, item_label, priority_score) %>%
      rename(avg_priority = priority_score) %>%
      slice_max(avg_priority, n = top_n) %>%
      arrange(desc(avg_priority))
  } else {
    item_avg <- ranked %>%
      group_by(item, item_label) %>%
      summarise(avg_priority = mean(priority_score, na.rm = TRUE), .groups = "drop") %>%
      slice_max(avg_priority, n = top_n) %>%
      arrange(desc(avg_priority))
  }
  
  plot_df <- ranked %>%
    filter(item %in% item_avg$item) %>%
    mutate(item_label = factor(item_label, levels = item_avg$item_label))
  
  ggplot(plot_df, aes(x = group, y = rank, group = item_label, color = item_label)) +
    geom_line(linewidth = 0.7, alpha = 0.8) +
    geom_point(size = 2.2) +
    scale_x_discrete(labels = GROUP_LABELS) +
    scale_y_reverse(breaks = scales::pretty_breaks(), limits = c(n_items, 1)) +
    scale_color_viridis_d(option = "turbo") +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0), legend.position = "right") +
    guides(color = guide_legend(ncol = 1)) +
    labs(
      title    = title,
      subtitle = paste0("Top ", top_n, " items by ",
                        if (!is.null(pooled_results)) "pooled" else "average",
                        " composite priority score",
                        if (!is.null(pooled_results)) "" else " across PWA, FMC, and CR",
                        ". All ", n_items, " scored items."),
      x = NULL, y = paste0("Rank (1 = highest priority, ", n_items, " = lowest priority)"),
      color = "Item"
    )
}

#' Domain-level grouped bar chart across the three groups.
plot_domain_by_group <- function(domain_summary_by_group,
                                 title = "Domain Priority Scores by Interest-holder Group") {
  if (all(is.na(domain_summary_by_group$mean_priority_score))) {
    return(.empty_plot("No item ratings yet — nothing to summarize by domain."))
  }
  domain_summary_by_group %>%
    filter(!is.na(mean_priority_score)) %>%
    mutate(domain_label = format_domain_label(domain),
           group = factor(group, levels = GROUP_LEVELS)) %>%
    ggplot(aes(x = domain_label, y = mean_priority_score, fill = group)) +
    geom_col(position = position_dodge(width = 0.75), width = 0.68) +
    coord_flip() +
    scale_fill_group() +
    publication_theme() +
    labs(title = title, x = NULL, y = "Mean composite priority score", fill = "Group")
}

#' Summarise item results at the domain level, by group.
summarise_domains_by_group <- function(group_results) {
  group_results %>%
    group_by(domain, group) %>%
    summarise(
      mean_rating         = mean(mean_score, na.rm = TRUE),
      mean_prevalence     = mean(prevalence, na.rm = TRUE),
      mean_high_priority  = mean(high_priority, na.rm = TRUE),
      mean_priority_score = mean(priority_score, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Stacked bar chart: top items, stacked by interest-holder group contribution,
#' with individual group values labeled inside bars and total labeled above.
#'
#' @param group_results  Output of score_by_group().
#' @param n              Number of top items to show (by total across groups).
#' @param metric         "n_high_priority" (default; count of respondents per
#'                       group who rated the item 4-5 — matches the
#'                       reference chart's integer-count style) or
#'                       "priority_score" (sum of composite priority scores
#'                       across groups instead of a respondent count).
#' @param title          Plot title.
plot_stacked_top_impacts <- function(group_results, n = 10,
                                     metric = c("n_high_priority", "priority_score"),
                                     title = "Top Impacts by Interest-holder Group") {
  metric <- match.arg(metric)
  
  df <- group_results %>%
    filter(!is.na(.data[[metric]])) %>%
    mutate(group = factor(group, levels = GROUP_LEVELS))
  
  if (nrow(df) == 0) return(.empty_plot("No item ratings yet — nothing to stack."))
  
  totals <- df %>%
    group_by(item_label) %>%
    summarise(total = sum(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total)) %>%
    slice_head(n = n) %>%
    mutate(
      total_label = if (metric == "n_high_priority") {
        as.character(round(total))
      } else {
        as.character(round(total, 1))
      }
    )
  
  plot_df <- df %>%
    filter(item_label %in% totals$item_label) %>%
    mutate(
      item_label = factor(item_label, levels = totals$item_label),
      segment_label = case_when(
        is.na(.data[[metric]]) | .data[[metric]] == 0 ~ "",
        metric == "n_high_priority" ~ as.character(round(.data[[metric]])),
        TRUE ~ as.character(round(.data[[metric]], 1))
      )
    )
  
  y_label <- if (metric == "n_high_priority") {
    "Respondents rating 4-5, by group"
  } else {
    "Composite priority score (summed), by group"
  }
  
  ggplot(plot_df, aes(x = item_label, y = .data[[metric]], fill = group)) +
    geom_col(width = 0.7) +
    # White numeric labels inside each stacked segment
    geom_text(
      aes(label = segment_label),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold",
      size = 3.2
    ) +
    # Overall total labeled above each bar
    geom_text(
      data = totals,
      aes(x = item_label, y = total, label = total_label),
      inherit.aes = FALSE,
      vjust = -0.4,
      fontface = "bold",
      size = 3.6
    ) +
    scale_fill_group() +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
    labs(title = title, x = NULL, y = y_label, fill = "Group")
}

# ── Sequential gradient palette used by the two charts below, matching
#    the reference figures' light-salmon-to-dark-plum look. ─────────────
.ranking_gradient <- function(n) {
  colorRampPalette(c("#e3efef", "#076f6e", "#473245"))(n)
}

#' "Final ranking" funnel chart: horizontal bars, longest (highest-scoring)
#' at top, each numbered and labeled inside the bar, on a light-to-dark
#' gradient — styled after a reference "Final Group Ranking" figure.
#'
#' Works on either domain-level data (pass the output of
#' summarise_domains_pooled(), label_col = "domain_label") or item-level
#' data (pass core_impact_overall / core_impact_by_group[[g]],
#' label_col = "item_label").
#'
#' @param df         A data frame with a label column and a score column.
#' @param label_col  Name of the label column (e.g. "domain_label" or "item_label").
#' @param score_col  Name of the score column to rank by (e.g. "priority_score").
#' @param n          Optional: keep only the top n rows.
#' @param title      Plot title.
#' @param subtitle   Plot subtitle (e.g. "N = 150 Respondents").
plot_ranking_funnel <- function(df, label_col = "domain_label", score_col = "priority_score",
                                n = NULL,
                                title = "Final Ranking of Impacts",
                                subtitle = NULL) {
  df2 <- df %>% filter(!is.na(.data[[score_col]])) %>% arrange(desc(.data[[score_col]]))
  if (!is.null(n)) df2 <- df2 %>% slice_head(n = n)
  if (nrow(df2) == 0) return(.empty_plot("No data available yet to rank."))
  
  df2 <- df2 %>%
    mutate(
      rank        = row_number(),
      rank_label  = paste0(rank, "   ", .data[[label_col]]),
      plot_order  = factor(rank_label, levels = rev(rank_label))
    )
  
  pal <- setNames(.ranking_gradient(nrow(df2)), as.character(df2$rank))
  
  ggplot(df2, aes(x = plot_order, y = .data[[score_col]], fill = factor(rank))) +
    geom_col(width = 0.78, show.legend = FALSE) +
    geom_text(
      aes(label = rank_label, y = max(.data[[score_col]]) * 0.015),
      hjust = 0, color = "white", fontface = "bold", size = 4.2
    ) +
    scale_fill_manual(values = pal) +
    coord_flip() +
    theme_void(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title    = element_text(face = "bold", size = rel(1.15), color = "grey10",
                                   margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.9), color = "grey35", margin = margin(b = 10)),
      plot.margin   = margin(10, 14, 10, 10)
    ) +
    labs(title = title, subtitle = subtitle)
}

#' "Calculated weighting" pie chart: each item/domain's share of the total
#' composite priority score, styled after a reference "Calculated Group
#' Weighting" figure (swing-weighting-style pie with a percentage legend).
#'
#' Works on either domain-level or item-level data — same arguments as
#' plot_ranking_funnel().
#'
#' @param df         A data frame with a label column and a score column.
#' @param label_col  Name of the label column.
#' @param score_col  Name of the score column (weights are this column's
#'                    share of the sum across the rows shown).
#' @param n          Optional: keep only the top n rows before computing shares.
#' @param title      Plot title.
#' @param subtitle   Plot subtitle (defaults to "Total = 100%").
plot_weight_pie <- function(df, label_col = "domain_label", score_col = "priority_score",
                            n = NULL,
                            title = "Calculated Weighting of Impacts",
                            subtitle = "Total = 100%") {
  df2 <- df %>% filter(!is.na(.data[[score_col]])) %>% arrange(desc(.data[[score_col]]))
  if (!is.null(n)) df2 <- df2 %>% slice_head(n = n)
  if (nrow(df2) == 0) return(.empty_plot("No data available yet to weight."))
  
  df2 <- df2 %>%
    mutate(
      weight      = .data[[score_col]] / sum(.data[[score_col]]) * 100,
      legend_text = paste0(.data[[label_col]], "   ", round(weight), "%"),
      plot_label  = factor(.data[[label_col]], levels = .data[[label_col]])
    )
  
  pal <- setNames(.ranking_gradient(nrow(df2)), levels(df2$plot_label))
  legend_labels <- setNames(df2$legend_text, df2$plot_label)
  
  ggplot(df2, aes(x = "", y = weight, fill = plot_label)) +
    geom_col(width = 1, color = "white", linewidth = 0.6) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = pal, labels = legend_labels[levels(df2$plot_label)]) +
    theme_void(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title    = element_text(face = "bold", size = rel(1.15), color = "grey10",
                                   margin = margin(b = 2)),
      plot.subtitle = element_text(size = rel(0.9), color = "grey35", margin = margin(b = 10)),
      legend.position = "right",
      legend.title    = element_blank(),
      legend.text     = element_text(size = rel(0.85)),
      plot.margin     = margin(10, 14, 10, 10)
    ) +
    labs(title = title, subtitle = subtitle)
}

plot_kw_results <- function(kw_df, top_n = 15, title = "") {
  kw_df %>%
    arrange(p_value) %>%
    slice_head(n = top_n) %>%
    mutate(label = coalesce(item_label, domain),
           sig = p_value < 0.05) %>%
    ggplot(aes(x = reorder(label, -log10(p_value)), y = -log10(p_value), fill = sig)) +
    geom_col() +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "grey40") +
    coord_flip() +
    scale_fill_manual(values = c(`TRUE` = "#0072B2", `FALSE` = "grey70")) +
    publication_theme() +
    labs(title = title, x = NULL, y = expression(-log[10](p)), fill = "p < .05")
}
