#' Novel versus literature-derived impact analysis
#'
#' Two comparisons:
#'   1. Known vs New  — do items from the environmental scan rate differently
#'      from novel items surfaced by focus groups (patient-generated)?
#'   2. Known PR vs Known PG — among Known items only, do those documented
#'      in peer-reviewed literature rate differently from those documented in
#'      patient-generated sources (blogs, YouTube, Spotify podcasts, etc.)?
#'      Items with mixed provenance ("PR; PG") are included in a supplementary
#'      Kruskal-Wallis but excluded from the binary PR-vs-PG MW test.
#'
#' Note: results_compare already carries known_new, source_type, and
#' source_category from score_all() — no second join needed here.
#'
#' @param results_compare  Full scored results (both groups).
#' @param dat              Output of load_and_clean().
#' @return Named list — see section headers below for contents.
run_novel_vs_known <- function(results_compare, dat) {
  out <- list()

  # ── Long-format ratings with source annotation ────────────
  # known_new_cols built from KNOWN_NEW_DF for joining onto item_long
  # (item_long is built from the raw survey, which lacks source columns)
  label_to_col <- tibble(
    item       = names(ITEM_LABELS),
    item_label = unname(ITEM_LABELS)
  )

  known_new_cols <- KNOWN_NEW_DF %>%
    left_join(label_to_col, by = "item_label") %>%
    select(item, known_new, source_type, source_category)

  item_long <- dat$df_clean %>%
    select(stakeholder_group, all_of(unlist(dat$domains, use.names = FALSE))) %>%
    pivot_longer(-stakeholder_group, names_to = "item", values_to = "value") %>%
    left_join(known_new_cols, by = "item") %>%
    drop_na(value, known_new)

  # results_compare already has known_new / source_type / source_category
  # from score_all(); use it directly — no re-join needed.
  results_annotated <- results_compare %>%
    filter(!is.na(known_new))

  # ═══════════════════════════════════════════════════════════
  # 1. KNOWN vs NEW
  # ═══════════════════════════════════════════════════════════

  # ── MW test ───────────────────────────────────────────────
  out$mw_known_vs_new <- wilcox_rank_biserial(item_long$value, item_long$known_new)

  # ── Rating summary ────────────────────────────────────────
  out$summary_known_vs_new <- item_long %>%
    group_by(known_new) %>%
    summarise(
      n_ratings   = n(),
      mean_rating = mean(value, na.rm = TRUE),
      sd_rating   = sd(value,   na.rm = TRUE),
      median      = median(value, na.rm = TRUE),
      .groups     = "drop"
    )

  # ── Item-level metrics ────────────────────────────────────
  out$item_metrics_known_vs_new <- results_annotated %>%
    group_by(known_new) %>%
    summarise(
      n_items             = n_distinct(item),
      mean_priority_score = mean(priority_score, na.rm = TRUE),
      mean_impact         = mean(mean_score,      na.rm = TRUE),
      mean_prevalence     = mean(prevalence,       na.rm = TRUE),
      mean_high_priority  = mean(high_priority,    na.rm = TRUE),
      .groups             = "drop"
    )

  # ── Overall top 10 with source label ─────────────────────
  overall_top10 <- results_annotated %>%
    group_by(item, item_label, known_new, source_type, source_category) %>%
    summarise(mean_priority = mean(priority_score, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(mean_priority)) %>%
    slice_head(n = 10)

  out$top10 <- overall_top10

  n_novel     <- sum(overall_top10$known_new == "New",   na.rm = TRUE)
  n_known     <- sum(overall_top10$known_new == "Known", na.rm = TRUE)
  total_novel <- sum(results_annotated$known_new == "New",   na.rm = TRUE) /
    n_distinct(results_annotated$group)
  total_known <- sum(results_annotated$known_new == "Known", na.rm = TRUE) /
    n_distinct(results_annotated$group)

  out$top10_counts <- tibble(
    novel_in_top10      = n_novel,
    known_in_top10      = n_known,
    total_novel         = total_novel,
    total_known         = total_known,
    base_rate_novel_pct = round(total_novel / (total_novel + total_known) * 100, 1),
    top10_novel_pct     = round(n_novel / 10 * 100, 1)
  )

  # ── Group-specific top 10 ─────────────────────────────────
  out$top10_pwa <- results_annotated %>%
    filter(group == "PWA") %>%
    arrange(desc(priority_score)) %>%
    slice_head(n = 10) %>%
    select(item_label, known_new, source_type, source_category,
           priority_score, mean_score)

  out$top10_fmc <- results_annotated %>%
    filter(group == "FMC") %>%
    arrange(desc(priority_score)) %>%
    slice_head(n = 10) %>%
    select(item_label, known_new, source_type, source_category,
           priority_score, mean_score)

  # ═══════════════════════════════════════════════════════════
  # 2. KNOWN PR vs KNOWN PG (among Known items only)
  # ═══════════════════════════════════════════════════════════

  # Ratings restricted to Known items
  item_long_known <- item_long %>%
    filter(known_new == "Known")

  # Binary MW: PR-only vs PG-only (excludes "PR; PG" mixed items)
  item_long_known_binary <- item_long_known %>%
    filter(source_type %in% c("PR", "PG"))

  out$mw_pr_vs_pg <- wilcox_rank_biserial(
    item_long_known_binary$value,
    item_long_known_binary$source_type
  )

  # Three-level KW: PR / PG / PR; PG (all Known items)
  kw_known <- kruskal.test(value ~ source_type, data = item_long_known)
  out$kw_known_source <- tibble(
    chi_squared = kw_known$statistic,
    df          = kw_known$parameter,
    p_value     = kw_known$p.value
  )

  # Post-hoc Dunn (Bonferroni) across PR / PG / PR; PG
  dunn_known <- dunn.test::dunn.test(
    item_long_known$value,
    item_long_known$source_type,
    method  = "bonferroni",
    kw      = FALSE,
    label   = TRUE,
    table   = FALSE
  )
  out$dunn_known_source <- tibble(
    comparison = dunn_known$comparisons,
    z          = dunn_known$Z,
    p_adj      = dunn_known$P.adjusted
  )

  # ── Rating summary: PR / PG / PR; PG ─────────────────────
  out$summary_known_source <- item_long_known %>%
    group_by(source_type) %>%
    summarise(
      n_ratings   = n(),
      mean_rating = mean(value, na.rm = TRUE),
      sd_rating   = sd(value,   na.rm = TRUE),
      median      = median(value, na.rm = TRUE),
      .groups     = "drop"
    ) %>%
    arrange(source_type)

  # ── Item-level metrics: PR / PG / PR; PG ─────────────────
  out$item_metrics_known_source <- results_annotated %>%
    filter(known_new == "Known", !is.na(source_type)) %>%
    group_by(source_type) %>%
    summarise(
      n_items             = n_distinct(item),
      mean_priority_score = mean(priority_score, na.rm = TRUE),
      mean_impact         = mean(mean_score,      na.rm = TRUE),
      mean_prevalence     = mean(prevalence,       na.rm = TRUE),
      mean_high_priority  = mean(high_priority,    na.rm = TRUE),
      .groups             = "drop"
    ) %>%
    arrange(source_type)

  out
}
