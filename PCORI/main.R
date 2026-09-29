#' ============================================================
#' Core Impact Set Analysis — PWA vs Family/Caregivers vs Clinicians/Researchers
#' ============================================================
#'
#'
#' Usage:
#'   setwd("path/to/this/folder")
#'   source("main.R")
#'
#' Requires:
#'   data/convening_3.csv

# ── Source all modules ─────────────────────────────────────
source("R/00_config.R")
source("R/01_statistics_helpers.R")
source("R/02_load_clean.R")
source("R/03_scoring.R")
source("R/04_core_impact_set.R")
source("R/05_group_comparisons.R")
source("R/06_demographics.R")
source("R/07_visualizations.R")
source("R/08_sensitivity.R")

dir.create("output", showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# ═══════════════════════════════════════════════════════════
# 1. LOAD & CLEAN
# ═══════════════════════════════════════════════════════════
dat <- load_and_clean(path = "data/convening_3.csv")
domain_means <- compute_domain_means(dat)

# ═══════════════════════════════════════════════════════════
# 2. SCORING
# ═══════════════════════════════════════════════════════════
pooled_results <- score_pooled(dat)     # all respondents together
group_results  <- score_by_group(dat)   # PWA / FMC / CR separately

# ═══════════════════════════════════════════════════════════
# 3. CORE IMPACT SET(S)
# ═══════════════════════════════════════════════════════════
core_impact_overall <- build_core_impact_set(pooled_results)
core_impact_by_group <- build_group_core_impact_sets(group_results)
core_impact_comparison <- build_core_impact_comparison(core_impact_overall, core_impact_by_group)
group_rank_agreement <- build_group_rank_agreement(core_impact_by_group)

cat("\n=== FINAL CORE IMPACT SET (all 40 items, all respondents) ===\n")
print(core_impact_overall %>% select(rank, item_label, domain, priority_score), n = 40)

for (g in GROUP_LEVELS) {
  cat("\n=== CORE IMPACT SET —", GROUP_LABELS[[g]], "===\n")
  print(core_impact_by_group[[g]] %>% select(rank, item_label, domain, priority_score), n = 40)
}

cat("\n=== Cross-Group Rank Agreement (Spearman rho on item rankings) ===\n")
print(group_rank_agreement)

cat("\n=== Items Where Groups Diverge Most (largest rank gap) ===\n")
print(
  core_impact_comparison %>%
    arrange(desc(max_group_rank_gap)) %>%
    select(item_label, domain, overall_rank, starts_with("rank_"), max_group_rank_gap, agreement_flag),
  n = 15
)

# ═══════════════════════════════════════════════════════════
# 4. STATISTICAL COMPARISON ACROSS GROUPS (PWA vs FMC vs CR)
# ═══════════════════════════════════════════════════════════
group_comparisons <- run_group_comparisons(dat, domain_means)
print_group_comparisons(group_comparisons)

# ═══════════════════════════════════════════════════════════
# 5. DEMOGRAPHIC / REGIONAL / SES ASSOCIATIONS
# ═══════════════════════════════════════════════════════════
demographics <- run_demographics(dat, domain_means)

print_demographics_bundle(demographics$pooled, header = "POOLED (ALL GROUPS)")
for (g in GROUP_LEVELS) {
  print_demographics_bundle(demographics$by_group[[g]], header = GROUP_LABELS[[g]])
}

# Screening-level group x demographic interaction checks
interaction_region <- group_demographic_interaction_screen(dat, "region", "Region")
interaction_ses     <- group_demographic_interaction_screen(dat, "ses_category", "SES category")
interaction_urban    <- group_demographic_interaction_screen(dat, "urban_category", "Urbanicity")
interaction_race    <- group_demographic_interaction_screen(dat, "race_ethnicity_collapsed", "Race/ethnicity")

cat("\n=== Group x Region interaction screen (top 15 by p-value) ===\n")
if (nrow(interaction_region) > 0) {
  print(interaction_region %>% arrange(p_value) %>%
          select(item_label, domain, statistic, eps_sq, p_value, p_adj_bonf), n = 15)
}
# ═══════════════════════════════════════════════════════════
# 6. SENSITIVITY ANALYSES (equal group weights; exclusions)
# ═══════════════════════════════════════════════════════════
sensitivity <- run_sensitivity(dat)

cat("\n=== Equal group weighting: overlap of top 10 with the primary (pooled) core set ===\n")
print(sensitivity$equal_weight_compare)

cat("\n=== Excluding records 471, 159, 606 (no usable or uniform ratings) ===\n")
cat("Same top 10 set:", sensitivity$exclusion_same_top_n_set,
    "| same order:", sensitivity$exclusion_same_top_n_order, "\n")
print(sensitivity$exclusion_compare %>% filter(rank <= 12), n = 12)
# ═══════════════════════════════════════════════════════════
# 7. VISUALIZATIONS
# ═══════════════════════════════════════════════════════════
p_core_impact   <- plot_core_impact_set(core_impact_overall)
p_top15_by_grp  <- plot_top_n_by_group(group_results, n = 10)
p_divergence    <- plot_group_divergence(core_impact_comparison, n = 15)
domain_summary_by_group <- summarise_domains_by_group(group_results)
p_domain_by_group <- plot_domain_by_group(domain_summary_by_group)
p_stacked_top <- plot_stacked_top_impacts(group_results, n = 10)
p_rank_slope <- plot_rank_slope(group_results, pooled_results = pooled_results, top_n = 10)
domain_scores_overall <- summarise_domains_pooled(pooled_results)
n_total <- nrow(dat$df_clean)
p_domain_funnel <- plot_ranking_funnel(
  domain_scores_overall, label_col = "domain_label", score_col = "priority_score",
  title = "Final Ranking of Impact Domains",
  subtitle = paste0("N = ", n_total, " Respondents")
)


# Item-level versions of the same two charts — top 10 items instead of the
# 9 domains, pooled across all respondents.
p_item_funnel <- plot_ranking_funnel(
  core_impact_overall, label_col = "item_label", score_col = "priority_score", n = 10,
  title = "Final Ranking of Top 10 Impacts",
  subtitle = paste0("N = ", n_total, " Respondents")
)

print(p_core_impact)
print(p_top15_by_grp)
print(p_divergence)
print(p_domain_by_group)
print(p_stacked_top)
print(p_rank_slope)
print(p_domain_funnel)
print(p_item_funnel)

save_publication_plot(p_core_impact,     "output/figures/core_impact_set.png",       width = 8,  height = 9)
save_publication_plot(p_top15_by_grp,    "output/figures/top15_by_group.png",        width = 11, height = 6)
save_publication_plot(p_divergence,      "output/figures/group_divergence.png",      width = 8,  height = 6)
save_publication_plot(p_domain_by_group, "output/figures/domain_by_group.png",       width = 8,  height = 5.5)
save_publication_plot(p_stacked_top,     "output/figures/stacked_top_impacts.png",   width = 9,  height = 7)
save_publication_plot(p_rank_slope,      "output/figures/rank_slope.png",            width = 9,  height = 7)
save_publication_plot(p_domain_funnel,   "output/figures/domain_ranking_funnel.png", width = 7,  height = 6)
save_publication_plot(p_item_funnel,     "output/figures/item_ranking_funnel.png",   width = 8,  height = 6.5)

# ═══════════════════════════════════════════════════════════
#8. EXPORT
# ═══════════════════════════════════════════════════════════
write_csv(core_impact_overall,     "output/core_impact_set_overall.csv")
write_csv(bind_rows(core_impact_by_group, .id = "group"),
          "output/core_impact_sets_by_group.csv")
write_csv(core_impact_comparison,  "output/core_impact_set_comparison.csv")
write_csv(group_rank_agreement,    "output/group_rank_agreement.csv")

write_csv(group_comparisons$item$kw,   "output/group_comparison_item_kw.csv")
if (!is.null(group_comparisons$item$dunn))
  write_csv(group_comparisons$item$dunn, "output/group_comparison_item_dunn.csv")
write_csv(group_comparisons$domain$kw, "output/group_comparison_domain_kw.csv")
if (!is.null(group_comparisons$domain$dunn))
  write_csv(group_comparisons$domain$dunn, "output/group_comparison_domain_dunn.csv")

# Demographics: pooled + per-group, one CSV per categorical factor
for (var in names(CATEGORICAL_DEMOGRAPHICS)) {
  if (!is.null(demographics$pooled[[var]])) {
    write_csv(demographics$pooled[[var]]$items$kw,
              paste0("output/demo_pooled_", var, "_item_kw.csv"))
    write_csv(demographics$pooled[[var]]$domains$kw,
              paste0("output/demo_pooled_", var, "_domain_kw.csv"))
  }
  for (g in GROUP_LEVELS) {
    if (!is.null(demographics$by_group[[g]][[var]])) {
      write_csv(demographics$by_group[[g]][[var]]$items$kw,
                paste0("output/demo_", g, "_", var, "_item_kw.csv"))
      write_csv(demographics$by_group[[g]][[var]]$domains$kw,
                paste0("output/demo_", g, "_", var, "_domain_kw.csv"))
    }
  }
}

write_csv(interaction_region, "output/interaction_group_x_region.csv")
write_csv(interaction_ses,    "output/interaction_group_x_ses.csv")
write_csv(interaction_urban,  "output/interaction_group_x_urban.csv")
write_csv(interaction_race,   "output/interaction_group_x_race.csv")

# Continuous demographic associations (Age, education, MPO), pooled + per-group
for (var in names(CONTINUOUS_DEMOGRAPHICS)) {
  ct_pooled <- demographics$pooled$continuous[[var]]
  if (!is.null(ct_pooled) && nrow(ct_pooled) > 0) {
    write_csv(ct_pooled, paste0("output/demo_pooled_", var, "_item_spearman.csv"))
  }
  for (g in GROUP_LEVELS) {
    ct_g <- demographics$by_group[[g]]$continuous[[var]]
    if (!is.null(ct_g) && nrow(ct_g) > 0) {
      write_csv(ct_g, paste0("output/demo_", g, "_", var, "_item_spearman.csv"))
    }
  }
}

cat("\nAll done. See output/ for CSVs and output/figures/ for plots.\n")