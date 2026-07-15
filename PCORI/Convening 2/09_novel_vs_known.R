#' Novel versus literature-derived impact analysis (single group)
#'
#' In the Clinicians/Researchers instrument every item is classified as a
#' NEW (patient-generated) impact, so there is no Known comparison group.
#' The Known-vs-New and PR-vs-PG contrasts from the original pipeline are
#' therefore not estimable; this module detects the single-class case and
#' reports descriptive New-item metrics and the overall top-10 instead,
#' while keeping the same return structure so downstream code does not break.
#'
#' @param results_compare  Full scored results (single group).
#' @param dat              Output of load_and_clean().
#' @return Named list — see section headers below for contents.
run_novel_vs_known <- function(results_compare, dat) {
  out <- list()

  label_to_col <- tibble(
    item       = names(ITEM_LABELS),
    item_label = unname(ITEM_LABELS)
  )

  known_new_cols <- KNOWN_NEW_DF %>%
    left_join(label_to_col, by = "item_label") %>%
    select(item, known_new, source_type, source_category)

  item_long <- dat$df_clean %>%
    select(all_of(unlist(dat$domains, use.names = FALSE))) %>%
    pivot_longer(everything(), names_to = "item", values_to = "value") %>%
    left_join(known_new_cols, by = "item") %>%
    drop_na(value, known_new)

  results_annotated <- results_compare %>%
    filter(!is.na(known_new))

  n_source_classes <- n_distinct(item_long$known_new)
  out$single_class  <- n_source_classes < 2

  # ═══════════════════════════════════════════════════════════
  # 1. KNOWN vs NEW
  # ═══════════════════════════════════════════════════════════

  # Between-source MW test only makes sense with ≥2 source classes.
  out$mw_known_vs_new <- if (out$single_class) {
    message("  Only one source class present (all New) — skipping Known-vs-New MW test.")
    tibble(
      g1 = NA_character_, g2 = NA_character_, p_value = NA_real_,
      statistic = NA_real_, rank_biserial = NA_real_,
      n1 = NA_integer_, n2 = NA_integer_
    )
  } else {
    wilcox_rank_biserial(item_long$value, item_long$known_new)
  }

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

  n_novel <- sum(overall_top10$known_new == "New",   na.rm = TRUE)
  n_known <- sum(overall_top10$known_new == "Known", na.rm = TRUE)
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

  # ═══════════════════════════════════════════════════════════
  # 2. KNOWN PR vs KNOWN PG — not estimable (no Known items)
  # ═══════════════════════════════════════════════════════════
  na_mw <- tibble(
    g1 = NA_character_, g2 = NA_character_, p_value = NA_real_,
    statistic = NA_real_, rank_biserial = NA_real_,
    n1 = NA_integer_, n2 = NA_integer_
  )

  item_long_known <- item_long %>% filter(known_new == "Known")

  if (nrow(item_long_known) == 0) {
    message("  No Known items present — skipping PR-vs-PG and known-source analyses.")
    out$mw_pr_vs_pg            <- na_mw
    out$kw_known_source        <- tibble(chi_squared = NA_real_, df = NA_real_, p_value = NA_real_)
    out$dunn_known_source      <- tibble(comparison = character(0), z = numeric(0), p_adj = numeric(0))
    out$summary_known_source   <- tibble()
    out$item_metrics_known_source <- tibble()
    return(out)
  }

  # (Retained for completeness if a future instrument reintroduces Known items.)
  item_long_known_binary <- item_long_known %>%
    filter(source_type %in% c("PR", "PG"))

  out$mw_pr_vs_pg <- if (n_distinct(item_long_known_binary$source_type) == 2) {
    wilcox_rank_biserial(item_long_known_binary$value, item_long_known_binary$source_type)
  } else {
    na_mw
  }

  kw_known <- tryCatch(
    kruskal.test(value ~ source_type, data = item_long_known),
    error = function(e) NULL
  )
  out$kw_known_source <- if (is.null(kw_known)) {
    tibble(chi_squared = NA_real_, df = NA_real_, p_value = NA_real_)
  } else {
    tibble(chi_squared = kw_known$statistic, df = kw_known$parameter,
           p_value = kw_known$p.value)
  }

  out$dunn_known_source <- tibble(comparison = character(0), z = numeric(0), p_adj = numeric(0))

  out$summary_known_source <- item_long_known %>%
    group_by(source_type) %>%
    summarise(
      n_ratings = n(), mean_rating = mean(value, na.rm = TRUE),
      sd_rating = sd(value, na.rm = TRUE), median = median(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(source_type)

  out$item_metrics_known_source <- results_annotated %>%
    filter(known_new == "Known", !is.na(source_type)) %>%
    group_by(source_type) %>%
    summarise(
      n_items = n_distinct(item),
      mean_priority_score = mean(priority_score, na.rm = TRUE),
      mean_impact = mean(mean_score, na.rm = TRUE),
      mean_prevalence = mean(prevalence, na.rm = TRUE),
      mean_high_priority = mean(high_priority, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(source_type)

  out
}
