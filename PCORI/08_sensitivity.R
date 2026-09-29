#' Sensitivity analyses for the core impact set
#'
#' Checks whether the top-10 core set changes when:
#'   (1) the three groups are weighted equally (instead of pooling all
#'       respondents, where C/R contribute 34% and rate about 1 point higher);
#'   (2) records with no usable or uniform ratings are excluded.
#'
#' Equal weighting is done three ways so the result does not hinge on one
#' definition:
#'   a. mean of the three within-group ranks (lower = higher priority)
#'   b. mean of within-group z-standardized composite scores
#'   c. mean of the three unstandardized within-group composite scores
#'
#' Depends on: 00_config.R, 02_load_clean.R (load_and_clean), 03_scoring.R
#' (calc_metrics, score_pooled, score_by_group), 04_core_impact_set.R
#' Usage: source after main.R has created `dat`, `core_impact_overall`,
#'        `group_results`, or source the modules and run the block at the end.

TOP_N <- 10

#' Equal-group-weight rankings of the 40 items.
#'
#' @param group_results Output of score_by_group().
#' @return One row per item with the three equal-weight scores and ranks.
equal_weight_rankings <- function(group_results) {
  group_results %>%
    group_by(group) %>%
    mutate(
      rank_within = min_rank(desc(priority_score)),
      z_within    = as.numeric(scale(priority_score))
    ) %>%
    ungroup() %>%
    group_by(item, item_label, domain) %>%
    summarise(
      mean_rank      = mean(rank_within, na.rm = TRUE),
      mean_z         = mean(z_within, na.rm = TRUE),
      mean_composite = mean(priority_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      rank_by_mean_rank      = min_rank(mean_rank),
      rank_by_mean_z         = min_rank(desc(mean_z)),
      rank_by_mean_composite = min_rank(desc(mean_composite))
    )
}

#' Compare a sensitivity top-N with the primary (pooled) top-N.
#'
#' @param primary  core_impact_overall (has `item`, `rank`).
#' @param alt      Data frame with `item` and an integer rank column.
#' @param rank_col Name of the rank column in `alt`.
#' @param n        Size of the core set.
compare_top_n <- function(primary, alt, rank_col, n = TOP_N) {
  p <- primary %>% filter(rank <= n) %>% pull(item)
  a <- alt %>% filter(.data[[rank_col]] <= n) %>% pull(item)
  tibble(
    version        = rank_col,
    n_retained     = length(intersect(p, a)),
    dropped        = paste(alt$item_label[alt$item %in% setdiff(p, a)] %>%
                             { if (length(.) == 0) NA_character_ else . },
                           collapse = "; "),
    added          = paste(alt$item_label[alt$item %in% setdiff(a, p)] %>%
                             { if (length(.) == 0) NA_character_ else . },
                           collapse = "; ")
  )
}

run_sensitivity <- function(dat, exclude_record_ids = c(471, 159, 606), n = TOP_N) {

  # Primary results (all respondents, pooled composite)
  pooled  <- score_pooled(dat)
  primary <- build_core_impact_set(pooled)
  grp     <- score_by_group(dat)

  # (1) Equal group weighting -------------------------------------------
  eq <- equal_weight_rankings(grp)
  eq_compare <- bind_rows(
    compare_top_n(primary, eq, "rank_by_mean_rank", n),
    compare_top_n(primary, eq, "rank_by_mean_z", n),
    compare_top_n(primary, eq, "rank_by_mean_composite", n)
  )

  # (2) Exclude records with no usable / uniform ratings ------------------
  dat_ex <- dat
  dat_ex$df_clean <- dat$df_clean %>% filter(!record_id %in% exclude_record_ids)
  primary_ex <- build_core_impact_set(score_pooled(dat_ex)) %>%
    rename(rank_excl = rank) %>%
    select(item, item_label, rank_excl, priority_score_excl = priority_score)

  excl_compare <- primary %>%
    select(item, item_label, rank, priority_score) %>%
    left_join(primary_ex %>% select(item, rank_excl, priority_score_excl),
              by = "item") %>%
    mutate(score_change = priority_score_excl - priority_score) %>%
    arrange(rank)

  same_set   <- setequal(primary$item[primary$rank <= n],
                         excl_compare$item[excl_compare$rank_excl <= n])
  same_order <- identical(
    primary %>% filter(rank <= n) %>% arrange(rank) %>% pull(item),
    excl_compare %>% filter(rank_excl <= n) %>% arrange(rank_excl) %>% pull(item)
  )

  list(
    equal_weight_rankings = eq %>% arrange(rank_by_mean_rank),
    equal_weight_compare  = eq_compare,
    exclusion_compare     = excl_compare,
    exclusion_same_top_n_set   = same_set,
    exclusion_same_top_n_order = same_order
  )
}

# ── Run ────────────────────────────────────────────────────────────────
# Requires `dat` from main.R (dat <- load_and_clean(path = "data/convening_3.csv")).
if (exists("dat")) {
  sens <- run_sensitivity(dat)

  cat("\n=== Equal group weighting: overlap of top", TOP_N,
      "with the primary (pooled) core set ===\n")
  print(sens$equal_weight_compare)

  cat("\n=== Excluding records", paste(c(471, 159, 606), collapse = ", "),
      "(no usable or uniform ratings) ===\n")
  cat("Same top", TOP_N, "set:", sens$exclusion_same_top_n_set,
      "| same order:", sens$exclusion_same_top_n_order, "\n")
  print(sens$exclusion_compare %>% filter(rank <= TOP_N + 2), n = TOP_N + 2)

  write.csv(sens$equal_weight_rankings,
            "output/sensitivity_equal_group_weights.csv", row.names = FALSE)
  write.csv(sens$exclusion_compare,
            "output/sensitivity_exclusions.csv", row.names = FALSE)
}
