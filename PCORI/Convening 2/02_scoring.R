#' Scoring: item-level metrics, domain means, top-N extraction
#'
#' Single stakeholder group (Clinicians / Researchers).
#'
#' Depends on: 00_config.R (ITEM_LABELS, KNOWN_NEW_DF)

# Label used throughout for the single stakeholder group.
GROUP_LABEL <- "C/R"

#' Compute per-item priority metrics.
#'
#' @param data         Data frame.
#' @param items        Character vector of column names.
#' @param domain_name  Name of the domain.
#' @param group_label  Label for the stakeholder group.
#' @return A tibble with one row per item.
calc_metrics <- function(data, items, domain_name, group_label = GROUP_LABEL) {
  data %>%
    select(all_of(items)) %>%
    pivot_longer(everything(), names_to = "item", values_to = "value") %>%
    group_by(item) %>%
    summarise(
      mean_score     = mean(value, na.rm = TRUE),
      prevalence     = mean(!is.na(value)),
      high_priority  = mean(value %in% c(4, 5), na.rm = TRUE),
      priority_score = mean_score * prevalence,
      domain         = domain_name,
      group          = group_label,
      .groups        = "drop"
    )
}

#' Score all domains for the single stakeholder group.
#'
#' @param dat  Output of load_and_clean().
#' @return A tibble with item-level results, plus labels and new metadata.
score_all <- function(dat) {
  results <- map2_df(dat$domains, names(dat$domains),
                     ~ calc_metrics(dat$df_clean, .x, .y, GROUP_LABEL))

  results %>%
    mutate(
      item_label = coalesce(ITEM_LABELS[item], item)
    ) %>%
    left_join(KNOWN_NEW_DF, by = "item_label") %>%
    mutate(
      source = case_when(
        known_new == "Known" ~ "Environmental scan",
        known_new == "New"   ~ "Patient-generated",
        TRUE                 ~ "Unknown"
      )
    )
}

#' Compute per-person domain means (for domain-level analyses).
#'
#' Uses rowMeans() instead of rowwise() + c_across() for performance.
#'
#' @param dat  Output of load_and_clean().
#' @return The df_clean tibble with added *_mean columns.
compute_domain_means <- function(dat) {
  df <- dat$df_clean

  df$symptom_mean       <- rowMeans(df[dat$domains$symptom],       na.rm = TRUE)
  df$costs_mean         <- rowMeans(df[dat$domains$costs],         na.rm = TRUE)
  df$emotions_mean      <- rowMeans(df[dat$domains$emotions],      na.rm = TRUE)
  df$social_mean        <- rowMeans(df[dat$domains$social],        na.rm = TRUE)
  df$cognition_mean     <- rowMeans(df[dat$domains$cognition],     na.rm = TRUE)
  df$treatment_mean     <- rowMeans(df[dat$domains$treatment],     na.rm = TRUE)
  df$qol_mean           <- rowMeans(df[dat$domains$qol],           na.rm = TRUE)
  df$resources_mean     <- rowMeans(df[dat$domains$resources],     na.rm = TRUE)
  df$impact_others_mean <- rowMeans(df[dat$domains$impact_others], na.rm = TRUE)

  df
}

#' Extract the top-N items by priority score.
#'
#' @param results  Item-level results tibble.
#' @param n        Number of items.
#' @return A tibble sorted by descending priority_score.
top_n_items <- function(results, n = 15) {
  results %>%
    arrange(desc(priority_score)) %>%
    slice_head(n = n) %>%
    mutate(item_label = coalesce(ITEM_LABELS[item], item))
}

#' Build an item → domain lookup tibble.
item_domain_lookup <- function(domains) {
  tibble(
    item   = unlist(domains, use.names = FALSE),
    domain = rep(names(domains), lengths(domains))
  )
}
