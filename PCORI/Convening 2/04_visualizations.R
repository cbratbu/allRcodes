#' ============================================================
#' Publication Visualization Functions (single group: Clinicians / Researchers)
#' ============================================================
#'
#' Core descriptive and association figures for the priority analysis.
#' One shared publication theme, palette, label formatter, and export
#' helper keep all figures visually consistent.
#'
#' Because this pipeline has a single stakeholder group, all PWA-vs-FMC
#' comparison figures from the original pipeline (group facets, Venn,
#' difference lollipop, group Mann-Whitney plots, group scatter) are
#' omitted. Figures here describe the one group and its demographic
#' associations.
#'
#' Depends on: 00_config.R

# ── Shared publication style ───────────────────────────────
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x)) y else x

PUB_DOMAIN_PALETTE <- OKABE_ITO

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

# Single accent color for single-group bars/points.
PUB_ACCENT <- "#0072B2"

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

scale_color_domain <- function(...) scale_color_manual(values = PUB_DOMAIN_PALETTE, ...)
scale_fill_domain  <- function(...) scale_fill_manual(values = PUB_DOMAIN_PALETTE, ...)
scale_fill_source  <- function(...) scale_fill_manual(values = PUB_SOURCE_PALETTE, ...)
scale_color_source <- function(...) scale_color_manual(values = PUB_SOURCE_PALETTE, ...)

save_publication_plot <- function(plot, filename, width = 7, height = 5,
                                  dpi = 600, bg = "white") {
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  ggsave(filename, plot = plot, width = width, height = height,
         dpi = dpi, bg = bg)
  invisible(filename)
}

# ── Core descriptive visualizations ───────────────────────
#' Item-level priority matrix: prevalence x mean score.
plot_priority_matrix <- function(results) {
  results %>%
    mutate(domain = factor(domain, levels = names(PUB_DOMAIN_PALETTE))) %>%
    ggplot(aes(x = prevalence, y = mean_score,
               color = domain, size = high_priority)) +
    geom_point(alpha = 0.85) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_size_continuous(
      range = c(1.8, 7.5),
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_color_domain(labels = format_domain_label) +
    publication_theme() +
    labs(
      title    = "Priority Matrix — Clinicians / Researchers",
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
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_y_continuous(limits = c(1, 5), breaks = 1:5) +
    scale_size_continuous(
      range = c(3, 10),
      labels = scales::percent_format(accuracy = 1)
    ) +
    scale_color_domain(labels = format_domain_label) +
    publication_theme() +
    labs(
      title    = "Domain Priority Matrix — Clinicians / Researchers",
      subtitle = "Each point is one domain. X = mean prevalence; Y = mean rating (1–5); size = mean % high-priority.",
      x = "Mean response prevalence",
      y = "Mean rating (1–5)",
      color = "Domain",
      size = "Mean % high-priority"
    )
}

#' Domain-level priority score bars.
plot_domain_bars <- function(domain_summary) {
  domain_summary %>%
    mutate(domain_label = format_domain_label(domain)) %>%
    ggplot(aes(x = reorder(domain_label, mean_priority_score),
               y = mean_priority_score, fill = domain_label)) +
    geom_col(width = 0.68, show.legend = FALSE) +
    coord_flip() +
    scale_fill_manual(values = setNames(
      PUB_DOMAIN_PALETTE, format_domain_label(names(PUB_DOMAIN_PALETTE))
    )) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title    = "Domain Priority Scores — Clinicians / Researchers",
      subtitle = "Composite priority score = mean rating × response prevalence, averaged across items in each domain.",
      x = NULL,
      y = "Composite priority score"
    )
}

#' Top-N bar chart.
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

#' Summarise results at the domain level.
summarise_domains <- function(results) {
  results %>%
    group_by(domain, group) %>%
    summarise(
      mean_rating         = mean(mean_score,      na.rm = TRUE),
      mean_prevalence     = mean(prevalence,       na.rm = TRUE),
      mean_high_priority  = mean(high_priority,    na.rm = TRUE),
      mean_priority_score = mean(priority_score,   na.rm = TRUE),
      .groups = "drop"
    )
}

#' Top-15 items overall, colored by domain, shaped by known/new source.
plot_top15_combined <- function(results) {
  combined <- results %>%
    group_by(item, item_label, domain, source, known_new) %>%
    summarise(
      mean_priority = mean(priority_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_priority)) %>%
    slice_head(n = 15)

  ggplot(combined, aes(x = reorder(item_label, mean_priority),
                       y = mean_priority, fill = domain)) +
    geom_col(width = 0.72) +
    geom_point(aes(shape = known_new), y = 0, size = 3.0, stroke = 1.0, color = "grey20") +
    coord_flip() +
    scale_fill_domain(labels = format_domain_label) +
    scale_shape_manual(values = c("Known" = 16, "New" = 17)) +
    publication_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    labs(
      title = "Top 15 Priority Impacts — Clinicians / Researchers",
      subtitle = "Items ranked by composite priority score (mean rating × prevalence). Marker shape indicates known vs new.",
      x = NULL,
      y = "Composite priority score (mean × prevalence)",
      fill = "Domain",
      shape = "Known/new"
    )
}

# ============================================================
# Statistical / association visualizations
# ============================================================

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

# ── Clinical and demographic visualizations ────────────────
#' Plot KW results for a demographic bundle at domain or item level.
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

#' Plot continuous Spearman associations.
plot_spearman_associations <- function(cor_df, label_col = c("item_label", "domain"),
                                       top_n = 25, title = NULL) {
  label_col <- match.arg(label_col)

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

#' Boxplots for item ratings by a categorical demographic variable.
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

# ── Mixed-model forest plot ────────────────────────────────
#' Forest plot for fixed effects from cumulative-link mixed models.
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
        str_replace("^occupation_group", "Occupation: ") %>%
        str_replace("^domain", "Domain: ") %>%
        str_replace("^race_ethnicity_collapsed", "Race/ethnicity: ") %>%
        str_replace("^age_group", "Age group: ") %>%
        str_replace("^region", "Region: ") %>%
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

# ── Novel (New) item visualizations ────────────────────────
#' Overall top 10 items (all classified New for this instrument).
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
      title = "Overall Top 10 Items by Source",
      subtitle = "Items ranked by composite priority score (mean × prevalence). All items are patient-generated (New) for this instrument.",
      x = "Item", y = "Composite priority score (mean × prevalence)", fill = "Source"
    )
}

# ── One-call exporter ──────────────────────────────────────
#' Generate and save the analysis visualization set (single group).
#'
#' @return Invisibly returns a named character vector of saved file paths.
export_analysis_visualizations <- function(results, domain_summary,
                                            clin_demo, novel,
                                            dat = NULL, domain_means = NULL,
                                            mixed = NULL,
                                            out_dir = "output/figures") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  plots <- list(
    known_new_top10 = plot_known_new_top10(novel)
  )

  # Mixed-model forest plots, when mixed-model output is supplied.
  if (!is.null(mixed)) {
    plots$mixed_model_forest <- plot_mixed_model_forest(
      mixed,
      models = c("domain_main_effects", "adjusted_demographics",
                 "occupation_main_effects")
    )
  }

  # Demographic KW views (domain + item) for each categorical factor.
  clinical_kw <- list(
    age_domain_kw        = clin_demo$age_group$domains$kw,
    age_item_kw          = clin_demo$age_group$items$kw,
    region_domain_kw     = clin_demo$region$domains$kw,
    region_item_kw       = clin_demo$region$items$kw,
    race_domain_kw       = clin_demo$race$domains$kw,
    race_item_kw         = clin_demo$race$items$kw,
    occupation_domain_kw = clin_demo$occupation$domains$kw,
    occupation_item_kw   = clin_demo$occupation$items$kw
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
  if (!is.null(dat) && !is.null(domain_means)) {
    kw_specs <- list(
      age_group = list(group_var = "age_group", label = "Age Group",
                       domains = clin_demo$age_group$domains$kw, items = clin_demo$age_group$items$kw),
      region = list(group_var = "region", label = "Region",
                    domains = clin_demo$region$domains$kw, items = clin_demo$region$items$kw),
      race = list(group_var = "race_ethnicity_collapsed", label = "Race/Ethnicity",
                  domains = clin_demo$race$domains$kw, items = clin_demo$race$items$kw),
      occupation = list(group_var = "occupation_group", label = "Occupation Group",
                        domains = clin_demo$occupation$domains$kw, items = clin_demo$occupation$items$kw)
    )

    for (nm in names(kw_specs)) {
      spec <- kw_specs[[nm]]
      if (!is.null(spec$domains) && nrow(spec$domains) > 0 && spec$group_var %in% names(domain_means)) {
        plots[[paste0("kw_", nm, "_domain_violin_box")]] <- plot_kw_domain_distributions(
          domain_means, spec$domains, spec$group_var, spec$label, top_n = 9
        )
      }
      if (!is.null(spec$items) && nrow(spec$items) > 0 && spec$group_var %in% names(dat$df_clean)) {
        plots[[paste0("kw_", nm, "_item_violin_box")]] <- plot_kw_item_distributions(
          dat, spec$items, spec$group_var, spec$label, top_n = 12
        )
      }
    }
  }

  # Continuous association views (Age).
  continuous <- list(
    age_domain_spearman = clin_demo$age_domain_cor,
    age_item_spearman   = clin_demo$age
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
