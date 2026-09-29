# Core Impact Set Analysis — Convening 3

Designed to compare **three** stakeholder groups on a **curated 40-item subset** of the instrument, and to
build a ranked "Core Impact Set" both overall and per group.

## What this does

1. **Loads** one CSV containing all three groups, distinguished by a group
   column.
2. **Scores** each of the 40 items with the same composite priority score as
   your original pipeline: `mean_score x prevalence`.
3. **Builds the Core Impact Set**:
   - `core_impact_set_overall.csv` — all 40 items, ranked most → least
     important, pooled across all respondents.
   - `core_impact_sets_by_group.csv` — the same ranking done separately
     within each of PWA / FMC / CR.
   - `core_impact_set_comparison.csv` — one row per item with its rank in
     each group side by side, plus a `max_group_rank_gap` and
     `agreement_flag` column flagging items the groups rank very
     differently.
   - `group_rank_agreement.csv` — pairwise Spearman correlation of each
     group's full ranking against the others (a single "how much do these
     two groups agree overall" number).
4. **Tests whether groups differ statistically** on each item and domain
   (Kruskal-Wallis across all 3 groups, Dunn's post-hoc pairwise where
   significant) — `group_comparison_*` files.
5. **Tests demographic/regional/SES associations** — region, income/SES,
   race/ethnicity, age group, and age (continuous) — run three ways:
   - pooled across all groups (`demo_pooled_*`)
   - within each group separately (`demo_PWA_*`, `demo_FMC_*`, `demo_CR_*`)
   - a screening-level group x demographic interaction check
     (`interaction_group_x_*.csv`) — flags items where the *combination* of
     group and demographic level matters, as a first pass before a full
     interaction model.
6. **Exports figures**: the full ranked core impact set, top-15 per group
   side by side, a divergence plot of the items groups disagree on most, and
   domain-level scores by group.



## File structure

```
R/00_config.R              item map, group labels/colors, domain palette
R/01_statistics_helpers.R  KW, Dunn, Spearman, epsilon-squared (ported)
R/02_load_clean.R          CSV loading, group normalization, demographics
R/03_scoring.R             composite priority scores (pooled + per-group)
R/04_core_impact_set.R     ranking, per-group ranking, cross-group comparison
R/05_group_comparisons.R   3-group KW/Dunn on every item and domain
R/06_demographics.R        region/SES/race/age associations, pooled + per-group
R/07_visualizations.R      all plots
R/08_sensitivity.R         sensitivity analysis
main.R                     runs everything end to end
data/convening_3.csv        <- put your data here (real file already matched)
```

## A note on "composite score"

`priority_score = mean_score x prevalence`. This rewards items that are
*both* rated highly *and* answered by most respondents (an item only a few
people rated highly won't outrank one broadly endorsed at a slightly lower
level). Two other rankings are computed alongside it for context —
`mean_score` alone and `high_priority` (% rating 4-5) alone — visible in
every output table, in case you want to sanity-check the composite ranking
against a simpler one.

