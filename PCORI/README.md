# Clinician / Researcher Priority Analysis

Adapted from the original PWA-vs-FMC aphasia priority pipeline for a **single
stakeholder group: Clinicians / Researchers**, with the updated item set from
the `Clinicians___Researchers` template.

## How to run

```r
setwd("path/to/cr-analysis")
source("main.R")
```

Place your completed data file at `data/clinicians_researchers.csv` (same
columns as `data/clinicians_researchers_TEMPLATE.csv`). Requires the same R
packages as before plus `ordinal`: tidyverse, janitor, ggplot2, viridis,
Hmisc, dunn.test, ordinal.

## What changed vs. the original pipeline

**Single group.** All PWA-vs-FMC comparisons were removed — no
`stakeholder_group` split, no `respondent` factor, no group Mann-Whitney
tests, and no group-stratified (`_pwa` / `_fmc`) outputs. Scoring, rankings,
sensitivity, and associations all run on the full respondent sample
(`group = "C/R"`).

**Items updated.** `DOMAIN_LABELS` in `R/00_config.R` was rebuilt to match the
new template's column order (100 items vs. the original instrument). New items
were given readable labels auto-derived from the column names; the few with
non-obvious names were labelled by hand (e.g. `fc_inner_speech` → "Inner
speech", `te_portal` → "Patient portal use"). Column-to-label alignment is
positional within each domain and was verified to match exactly.

**All items classified New.** Per request, every item is tagged
`known_new = "New"`, `source_type = "New – PG"`. `KNOWN_NEW_DF` is now built
programmatically from `DOMAIN_LABELS` so it stays in sync with the item set.
Because there is no Known group, the Known-vs-New / PR-vs-PG tests in
`R/09_novel_vs_known.R` detect the single source class and skip those
contrasts gracefully, still reporting New-item metrics and the overall top-10.

**Demographics.**
- Kept: **Age** (continuous Spearman + `age_group` categorical KW/Dunn),
  **Race/ethnicity** (collapsed, KW/Dunn), **Region** (KW/Dunn).
- **Region** is derived from `state` using the four US Census regions
  (`R/01_load_clean.R`), since the template carries `state` rather than a
  pre-coded `region`.
- Dropped: **SES** (per request), plus urbanicity, severity, and months
  post-onset (aphasia-specific clinical variables not collected here).
- Added: **`occupation_group`** as a new categorical demographic —
  domain-level and item-level Kruskal-Wallis + Dunn post-hoc, *and* as a
  covariate / main-effect / interaction term in the mixed ordinal models
  (`R/08_mixed_models.R`).

**Mixed models** were reduced to single-group models: domain main effects, a
demographic-adjusted model, an occupation-group main-effects model, a
domain × occupation interaction, and domain-stratified occupation models.

## Module map

| File | Role |
|------|------|
| `R/00_config.R` | Libraries, domains, item labels, all-New `KNOWN_NEW_DF` |
| `R/01_load_clean.R` | Load/clean, age/race/ethnicity, region-from-state, occupation_group |
| `R/02_scoring.R` | Item metrics, domain means, top-N (single group) |
| `R/03_statistics.R` | Stat helpers (unchanged, general-purpose) |
| `R/04_visualizations.R` | Single-group figures + association plots + exporter |
| `R/05_sensitivity.R` | Ranking-method sensitivity (unchanged) |
| `R/06_group_comparisons.R` | Single-group descriptive summaries |
| `R/07_clinical_demographic.R` | Age / region / race / occupation associations |
| `R/08_mixed_models.R` | Cumulative-link mixed models (single group) |
| `R/09_novel_vs_known.R` | New-item metrics (single source class) |
| `main.R` | Orchestrates the full pipeline |

## Note on validation

Syntax of all files was parse-checked, and the hand-modified logic (domain
detection, item-label alignment for all 100 items, label uniqueness for the
join, all-New classification, and the state→region map) was verified in R. A
full end-to-end execution was not possible in the build environment because
CRAN package installation was unavailable there; run `source("main.R")` in
your own R environment with the packages above installed.
