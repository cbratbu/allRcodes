#' Scoring: item-level composite priority scores
#'
#' composite priority_score = mean_score x prevalence
#'   mean_score  = mean rating among those who answered (1-5)
#'   prevalence  = proportion of respondents who answered (non-missing)
#'   high_priority = proportion rating 4 or 5 (among all respondents)
#'
#' Computed three ways:
#'   - score_pooled():      all respondents together, group = "All"
#'   - score_by_group():    separately within each of PWA / FMC / CR
#'
#' Depends on: 00_config.R (ITEM_MAP_40, GROUP_LEVELS)

# Self-sourcing guard (see 02_load_clean.R for the full explanation).
if (!exists("ITEM_MAP_40")) {
  if (file.exists("R/00_config.R")) source("R/00_config.R") else stop(
    "ITEM_MAP_40 not found and R/00_config.R not found from the current ",
    "working directory (", getwd(), "). Run setwd() to the project root, ",
    "or source main.R from the top instead of this file on its own."
  )
}

#' Compute per-item metrics for one subset of rows.
#'
#' @param data    Data frame (already filtered to the desired subset).
#' @param items   Character vector of item column names.
#' @param group_label  Label to stamp on the "group" column.
#' @return A tibble with one row per item.
calc_metrics <- function(data, items, group_label) {
  data %>%
    select(all_of(items)) %>%
    pivot_longer(everything(), names_to = "item", values_to = "value") %>%
    group_by(item) %>%
    summarise(
      n_respondents    = n(),
      n_answered       = sum(!is.na(value)),
      # NA (not NaN) when nobody answered, so downstream ranking/correlation
      # treats the item as missing rather than propagating NaN.
      mean_score       = if (sum(!is.na(value)) == 0) NA_real_ else mean(value, na.rm = TRUE),
      prevalence       = mean(!is.na(value)),
      n_high_priority  = sum(value %in% c(4, 5), na.rm = TRUE),
      high_priority    = if (sum(!is.na(value)) == 0) NA_real_ else mean(value %in% c(4, 5), na.rm = TRUE),
      priority_score   = mean_score * prevalence,
      group            = group_label,
      .groups          = "drop"
    ) %>%
    left_join(ITEM_MAP_40 %>% select(item, item_label, domain), by = "item") %>%
    mutate(item_label = coalesce(item_label, item))
}

#' Score all 40 items on the full pooled sample (group = "All").
#'
#' @param dat  Output of load_and_clean().
#' @return A tibble of item-level results.
score_pooled <- function(dat) {
  calc_metrics(dat$df_clean, dat$items, "All")
}

#' Score all 40 items separately within each stakeholder group.
#'
#' @param dat  Output of load_and_clean().
#' @return A tibble of item-level results with one block of rows per group.
score_by_group <- function(dat) {
  map_df(GROUP_LEVELS, function(g) {
    sub <- dat$df_clean %>% filter(stakeholder_group == g)
    if (nrow(sub) == 0) return(tibble())
    calc_metrics(sub, dat$items, g)
  })
}

#' Compute per-person domain means (rowMeans across each domain's items).
#'
#' @param dat  Output of load_and_clean().
#' @return df_clean with added "<domain>_mean" columns.
compute_domain_means <- function(dat) {
  df <- dat$df_clean
  for (dom in names(dat$domains)) {
    cols <- dat$domains[[dom]]
    if (length(cols) == 0) next
    df[[paste0(dom, "_mean")]] <- rowMeans(df[cols], na.rm = TRUE)
  }
  df
}

#' Build an item -> domain lookup tibble.
item_domain_lookup <- function(domains = DOMAINS_40) {
  tibble(
    item   = unlist(domains, use.names = FALSE),
    domain = rep(names(domains), lengths(domains))
  )
}

#' Domain-level summary on the pooled sample (all groups combined) — one
#' row per domain with a mean composite priority score across its items.
#' Used for the aggregate "final ranking" / "weighting" style charts.
#'
#' @param pooled_results  Output of score_pooled().
#' @return A tibble: domain, domain_label, priority_score, mean_score.
summarise_domains_pooled <- function(pooled_results) {
  pooled_results %>%
    group_by(domain) %>%
    summarise(
      priority_score = mean(priority_score, na.rm = TRUE),
      mean_score      = mean(mean_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(domain_label = format_domain_label(domain))
}
