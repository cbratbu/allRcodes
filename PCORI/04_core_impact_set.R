#' Core Impact Set construction
#'
#' Produces:
#'   1. build_core_impact_set()       — all 40 items ranked, pooled sample
#'   2. build_group_core_impact_sets()— all 40 items ranked, within each group
#'   3. build_core_impact_comparison()— wide table: rank per group + overall
#'                                       rank + how much groups disagree
#'   4. build_group_rank_agreement()  — pairwise Spearman rank correlations
#'                                       between groups' item rankings
#'                                       (consensus vs divergence)
#'
#' "Core impact set" here = every item ranked, most to least important by
#' composite priority score. If you want a specific top-N cutoff (e.g. a
#' "top 15" core set) instead of the full ranked list of 40, use
#' dplyr::slice_head(n = N) on the output of these functions.

#' Rank all 40 items on the pooled sample.
#'
#' @param pooled_results  Output of score_pooled().
#' @return pooled_results with an added `rank` column (1 = highest priority),
#'         sorted most -> least important.
build_core_impact_set <- function(pooled_results) {
  pooled_results %>%
    mutate(rank = min_rank(desc(priority_score))) %>%
    arrange(rank) %>%
    select(rank, item, item_label, domain, mean_score, prevalence,
           high_priority, priority_score, n_respondents, n_answered)
}

#' Rank all 40 items separately within each stakeholder group.
#'
#' @param group_results  Output of score_by_group().
#' @return A named list (PWA / FMC / CR), each a ranked tibble like
#'         build_core_impact_set()'s output.
build_group_core_impact_sets <- function(group_results) {
  set_names(GROUP_LEVELS) %>%
    map(function(g) {
      group_results %>%
        filter(group == g) %>%
        mutate(rank = min_rank(desc(priority_score))) %>%
        arrange(rank) %>%
        select(rank, item, item_label, domain, mean_score, prevalence,
               high_priority, priority_score, n_respondents, n_answered)
    })
}

#' Wide comparison table: rank of every item within each group + overall,
#' plus a measure of how much the groups disagree on that item's priority.
#'
#' @param pooled_core   Output of build_core_impact_set().
#' @param group_cores   Output of build_group_core_impact_sets().
#' @return A tibble, one row per item, sorted by overall_rank.
build_core_impact_comparison <- function(pooled_core, group_cores) {
  base <- pooled_core %>%
    select(item, item_label, domain,
           overall_rank = rank, overall_priority_score = priority_score)

  for (g in GROUP_LEVELS) {
    gcore <- group_cores[[g]] %>%
      select(item, !!paste0("rank_", g) := rank,
             !!paste0("priority_", g) := priority_score)
    base <- base %>% left_join(gcore, by = "item")
  }

  rank_cols <- paste0("rank_", GROUP_LEVELS)

  base %>%
    rowwise() %>%
    mutate(
      # NA (rather than a -Inf/Inf warning) if a group's rank is missing
      # for this item (e.g. nobody in that group answered it).
      max_group_rank_gap = {
        vals <- c_across(all_of(rank_cols))
        vals <- vals[!is.na(vals)]
        if (length(vals) < 2) NA_real_ else max(vals) - min(vals)
      }
    ) %>%
    ungroup() %>%
    arrange(overall_rank) %>%
    mutate(
      agreement_flag = case_when(
        is.na(max_group_rank_gap) ~ "Insufficient data",
        max_group_rank_gap <= 5   ~ "High agreement",
        max_group_rank_gap <= 15  ~ "Moderate agreement",
        TRUE                      ~ "Divergent across groups"
      )
    )
}

#' Pairwise Spearman correlation between groups' full item rankings.
#' High rho = groups broadly agree on what matters most; low/negative rho =
#' groups prioritize very differently.
#'
#' @param group_cores  Output of build_group_core_impact_sets().
#' @return A tibble with one row per group pair.
build_group_rank_agreement <- function(group_cores) {
  pairs <- combn(GROUP_LEVELS, 2, simplify = FALSE)

  map_df(pairs, function(p) {
    a <- group_cores[[p[1]]] %>% select(item, rank_a = rank)
    b <- group_cores[[p[2]]] %>% select(item, rank_b = rank)
    joined <- inner_join(a, b, by = "item") %>% drop_na(rank_a, rank_b)

    rho <- if (nrow(joined) < 3) {
      NA_real_
    } else {
      tryCatch(
        suppressWarnings(cor(joined$rank_a, joined$rank_b, method = "spearman")),
        error = function(e) NA_real_
      )
    }

    tibble(
      group_1 = p[1],
      group_2 = p[2],
      spearman_rho = rho,
      n_items = nrow(joined)
    )
  })
}

#' Top-N convenience wrapper (e.g., "top 15 core impact items").
#'
#' @param ranked_set  Output of build_core_impact_set() or one element of
#'                     build_group_core_impact_sets().
#' @param n           Number of items to keep.
top_n_core <- function(ranked_set, n = 15) {
  ranked_set %>% filter(rank <= n)
}
