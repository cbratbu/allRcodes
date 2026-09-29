#' Data loading, cleaning, and demographic preparation
#'
#' Three stakeholder groups (PWA / FMC / CR) in a single CSV, distinguished
#' by a group-label column. Only the 40 items in ITEM_MAP_40 are used.
#'
#' Depends on: 00_config.R (ITEM_MAP_40, normalize_group, DOMAINS_40)

# Self-sourcing guard: if this file is run/sourced on its own (or after a
# session restart that only re-ran part of main.R), auto-load 00_config.R
# rather than failing later with a cryptic "could not find function" error.
# Assumes the working directory is the project root (same assumption as
# main.R's `source("R/00_config.R")`).
if (!exists("normalize_group")) {
  if (file.exists("R/00_config.R")) {
    source("R/00_config.R")
  } else {
    stop(
      "normalize_group() not found and R/00_config.R not found from the ",
      "current working directory (", getwd(), "). Run setwd() to the ",
      "project root first, or source main.R from the top instead of ",
      "running this file on its own."
    )
  }
}

# ── US state -> Census region lookup (reused from original pipeline) ──
.US_STATE_REGION <- c(
  CT = "Northeast", ME = "Northeast", MA = "Northeast", NH = "Northeast",
  RI = "Northeast", VT = "Northeast", NJ = "Northeast", NY = "Northeast",
  PA = "Northeast",
  IL = "Midwest", IN = "Midwest", MI = "Midwest", OH = "Midwest",
  WI = "Midwest", IA = "Midwest", KS = "Midwest", MN = "Midwest",
  MO = "Midwest", NE = "Midwest", ND = "Midwest", SD = "Midwest",
  DE = "South", FL = "South", GA = "South", MD = "South", NC = "South",
  SC = "South", VA = "South", DC = "South", WV = "South", AL = "South",
  KY = "South", MS = "South", TN = "South", AR = "South", LA = "South",
  OK = "South", TX = "South",
  AZ = "West", CO = "West", ID = "West", MT = "West", NV = "West",
  NM = "West", UT = "West", WY = "West", AK = "West", CA = "West",
  HI = "West", OR = "West", WA = "West"
)

#' Read a CSV robustly against duplicate raw column headers.
#'
#' readr's default name-repair renames BOTH copies of a duplicated header
#' (e.g. "fs_maintain_friendships_v2" appearing twice becomes
#' "...56"/"...57"), which silently breaks exact-name matching against
#' ITEM_MAP_40 even when one copy holds real data. This reads with base R
#' (which tolerates duplicate names) and, for each set of duplicates, keeps
#' the column with the most non-missing values (ties broken by first
#' occurrence) under its original name, dropping the rest.
#'
#' @param path Path to the CSV file.
#' @return A tibble with de-duplicated column names, all columns read as
#'         character (numeric/logical coercion happens downstream where
#'         each field is actually used).
read_csv_dedupe <- function(path) {
  raw <- utils::read.csv(
    path, check.names = FALSE, colClasses = "character",
    stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8"
  )
  raw_names <- names(raw)
  dupes <- unique(raw_names[duplicated(raw_names)])
  
  if (length(dupes) > 0) {
    keep <- rep(TRUE, ncol(raw))
    for (nm in dupes) {
      idx <- which(raw_names == nm)
      n_nonmissing <- vapply(idx, function(i) sum(!is.na(raw[[i]])), integer(1))
      winner <- idx[which.max(n_nonmissing)]
      loser  <- setdiff(idx, winner)
      keep[loser] <- FALSE
      message(
        "Duplicate column header '", nm, "' found in ", basename(path),
        " (", length(idx), " copies). Keeping the copy with ",
        max(n_nonmissing), " non-missing value(s) (column ", winner,
        "), dropping the other(s) (column", if (length(loser) > 1) "s" else "",
        " ", paste(loser, collapse = ", "), ")."
      )
    }
    raw <- raw[, keep, drop = FALSE]
  }
  
  tibble::as_tibble(raw)
}

#' Load and clean the raw survey CSV.
#'
#' Expects (case-insensitive, will be run through janitor::clean_names()):
#'   - a group column (default "stakeholder_group" or "group") whose values
#'     identify PWA / FMC / CR respondents (see normalize_group() in
#'     00_config.R for accepted codings)
#'   - the 40 item columns named in ITEM_MAP_40$item
#'   - optional demographics: age, state OR region, race, ethnicity
#'     (or race_ethnicity), gender, education_years, adi_national/ruca
#'     (or income/ses as a fallback)
#'
#' The item-rating export (e.g. PCORIDatabase-Convening3_DATA_*.csv) does
#' NOT include state/ADI/RUCA — those live on the separate "history" form.
#' Pass `demographics_path` to merge them in by `record_id`.
#'
#' @param path              Path to the item-rating CSV (e.g. Convening3 export).
#' @param demographics_path Optional path to the "history" form CSV/export,
#'                          merged in by record_id for state/ADI/RUCA
#'                          (and any other demographic fields not already
#'                          present in `path`).
#' @param group_col         Name of the raw group column (pre-clean_names,
#'                          case-insensitive match attempted automatically).
#' @return A list with df_clean, items, item_labels, domains.
load_and_clean <- function(path, demographics_path = NULL, group_col = NULL) {
  df <- read_csv_dedupe(path) %>%
    clean_names()
  
  # ── Optionally merge in the separate demographics/history export ──
  # (needed for state/ADI/RUCA, which live on a different REDCap form).
  if (!is.null(demographics_path)) {
    if (!"record_id" %in% names(df)) {
      warning(
        "demographics_path was supplied but '", path, "' has no record_id ",
        "column to merge on. Skipping merge."
      )
    } else {
      demo <- read_csv_dedupe(demographics_path) %>%
        clean_names()
      
      if (!"record_id" %in% names(demo)) {
        warning(
          "demographics_path '", demographics_path, "' has no record_id ",
          "column. Skipping merge."
        )
      } else {
        # Only bring in columns not already present (besides the join key),
        # so item-file demographics (already-populated) always take priority.
        new_cols <- setdiff(names(demo), names(df))
        demo_sub <- demo %>% select(record_id, all_of(new_cols))
        df <- df %>% left_join(demo_sub, by = "record_id")
        message(
          "Merged demographics_path on record_id. Added columns: ",
          paste(new_cols, collapse = ", ")
        )
      }
    }
  }
  
  # ── Identify + normalize the group column ────────────────
  candidate_cols <- c(group_col, "stakeholder_group", "group", "respondent_group")
  candidate_cols <- unique(candidate_cols[!is.na(candidate_cols)])
  found_group_col <- candidate_cols[candidate_cols %in% names(df)][1]
  
  if (is.na(found_group_col)) {
    stop(
      "No group column found. Expected one of: ",
      paste(candidate_cols, collapse = ", "),
      ". Pass group_col = \"your_column_name\" to load_and_clean()."
    )
  }
  
  raw_group_values <- df[[found_group_col]]
  
  df <- df %>%
    mutate(stakeholder_group = normalize_group(.data[[found_group_col]]))
  
  n_unmatched <- sum(is.na(df$stakeholder_group))
  if (n_unmatched > 0) {
    warning(
      n_unmatched, " row(s) had a group value that didn't match PWA/FMC/CR ",
      "and were set to NA. Check normalize_group() in 00_config.R if your ",
      "raw codings differ from what's handled there."
    )
  }
  
  # Always print the raw-value -> normalized-group crosstab, so the group
  # coding is visibly self-verifying on every run rather than something
  # you have to trust blindly (e.g. confirms "1" -> PWA, "2" -> FMC,
  # "3"/"4" -> CR, or flags it immediately if a future export's coding
  # doesn't match what normalize_group() expects).
  cat("\nGroup coding check — raw '", found_group_col, "' value -> normalized group:\n", sep = "")
  print(table(raw_value = raw_group_values, normalized_group = df$stakeholder_group,
              useNA = "ifany"))
  
  # ── Item columns present ──────────────────────────────────
  items_present <- intersect(ALL_ITEMS_40, names(df))
  items_missing <- setdiff(ALL_ITEMS_40, names(df))
  if (length(items_missing) > 0) {
    warning(
      length(items_missing), " of 40 expected item columns not found in CSV:\n  ",
      paste(items_missing, collapse = ", "),
      "\nEdit ITEM_MAP_40 in 00_config.R to match your actual column names."
    )
  }
  
  # ── Age ────────────────────────────────────────────────
  if ("age" %in% names(df)) {
    df <- df %>%
      mutate(
        age       = suppressWarnings(as.numeric(age)),
        age_group = case_when(
          age >= 18 & age <= 39 ~ "18–39",
          age >= 40 & age <= 59 ~ "40–59",
          age >= 60             ~ "60+",
          TRUE                  ~ NA_character_
        ),
        age_group = factor(age_group, levels = c("18–39", "40–59", "60+"))
      )
  }
  
  # ── Region ──────────────────────────────────────────────
  # convening_3.csv provides "region" directly (northeast/midwest/
  # southeast/west) — use it as-is rather than deriving from state.
  # Falls back to deriving from a numeric state code (STATE_CODE_LOOKUP)
  # only if no region column is present.
  if ("region" %in% names(df)) {
    df <- df %>%
      mutate(
        region = na_if(trimws(as.character(region)), ""),
        region = str_to_title(region),
        region = factor(region, levels = c("Northeast", "Midwest", "Southeast", "South", "West"))
      )
  } else if ("state" %in% names(df)) {
    df <- df %>%
      mutate(
        state_raw  = trimws(as.character(state)),
        state_abbr = ifelse(
          grepl("^[0-9]+$", state_raw),
          unname(STATE_CODE_LOOKUP[state_raw]),
          toupper(state_raw)
        ),
        region     = unname(.US_STATE_REGION[state_abbr]),
        region     = factor(region, levels = c("Northeast", "Midwest", "South", "West"))
      )
  }
  
  # ── Harmonize race/ethnicity ───────────────────────────
  if (!"race_ethnicity" %in% names(df)) {
    if ("race" %in% names(df)) {
      df <- df %>% mutate(race_ethnicity = race)
    } else if ("ethnicity" %in% names(df)) {
      df <- df %>% mutate(race_ethnicity = ethnicity)
    }
  }
  
  if ("race" %in% names(df)) {
    df <- df %>% mutate(race = suppressWarnings(as.numeric(race)))
  }
  if ("ethnicity" %in% names(df)) {
    df <- df %>%
      mutate(
        ethnicity       = suppressWarnings(as.numeric(ethnicity)),
        ethnicity_label = case_when(
          is.na(ethnicity) ~ NA_character_,
          ethnicity == 1   ~ "Hispanic",
          ethnicity == 0   ~ "Non-Hispanic",
          TRUE             ~ NA_character_
        ),
        ethnicity_label = factor(ethnicity_label, levels = c("Non-Hispanic", "Hispanic"))
      )
  }
  if (all(c("race", "ethnicity") %in% names(df))) {
    df <- df %>%
      mutate(
        race_ethnicity_collapsed = case_when(
          is.na(race) | is.na(ethnicity) ~ NA_character_,
          race == 1 & ethnicity == 0     ~ "White",
          TRUE                            ~ "Racially/ethnically minoritized"
        ),
        race_ethnicity_collapsed = factor(
          race_ethnicity_collapsed,
          levels = c("White", "Racially/ethnically minoritized")
        )
      )
  }
  
  # ── Socioeconomic status / geography ────────────────────
  # convening_3.csv provides SES and urban/rural status DIRECTLY as
  # categorical fields (not ADI/RUCA numeric codes as in the earlier
  # "history" form dictionary) — use them as-is. ADI/RUCA/income are
  # still supported as fallbacks for other datasets that use them instead.
  if ("ses" %in% names(df) && any(toupper(trimws(as.character(df$ses))) %in%
                                  c("LOW", "MIDDLE", "HIGH"))) {
    # SES already categorical (low/middle/high) — use directly.
    df <- df %>%
      mutate(
        ses_category = na_if(trimws(as.character(ses)), ""),
        ses_category = str_to_title(ses_category),
        ses_category = factor(ses_category, levels = c("Low", "Middle", "High"))
      )
  } else if ("adi_national" %in% names(df)) {
    df <- df %>%
      mutate(
        adi_national = suppressWarnings(as.numeric(adi_national)),
        ses_category = case_when(
          adi_national <= 3  ~ "Low disadvantage (ADI 1-3)",
          adi_national <= 7  ~ "Moderate disadvantage (ADI 4-7)",
          adi_national <= 10 ~ "High disadvantage (ADI 8-10)",
          TRUE               ~ NA_character_
        ),
        ses_category = factor(
          ses_category,
          levels = c("Low disadvantage (ADI 1-3)", "Moderate disadvantage (ADI 4-7)",
                     "High disadvantage (ADI 8-10)")
        )
      )
  } else if ("income" %in% names(df)) {
    df <- df %>%
      mutate(
        income       = suppressWarnings(as.numeric(income)),
        ses_category = case_when(
          income <= 2 ~ "Lower income",
          income == 3 ~ "Middle income",
          income >= 4 ~ "Higher income",
          TRUE        ~ NA_character_
        ),
        ses_category = factor(ses_category,
                              levels = c("Lower income", "Middle income", "Higher income"))
      )
  }
  
  if ("urban" %in% names(df) && any(toupper(trimws(as.character(df$urban))) %in%
                                    c("URBAN", "RURAL"))) {
    # Urbanicity already categorical (urban/rural) — use directly.
    df <- df %>%
      mutate(
        urban_category = na_if(trimws(as.character(urban)), ""),
        urban_category = str_to_title(urban_category),
        urban_category = factor(urban_category, levels = c("Urban", "Rural"))
      )
  } else if ("ruca" %in% names(df)) {
    # Standard USDA RUCA primary-code collapse: 1-3 Urban core,
    # 4-6 Large town/micropolitan, 7-10 Rural. Adjust if your RUCA field
    # uses secondary (decimal) codes.
    df <- df %>%
      mutate(
        ruca            = suppressWarnings(as.numeric(ruca)),
        urban_category  = case_when(
          ruca >= 1 & ruca <= 3  ~ "Urban core",
          ruca >= 4 & ruca <= 6  ~ "Large town/micropolitan",
          ruca >= 7 & ruca <= 10 ~ "Rural",
          TRUE                   ~ NA_character_
        ),
        urban_category  = factor(
          urban_category,
          levels = c("Urban core", "Large town/micropolitan", "Rural")
        )
      )
  }
  
  if ("education_years" %in% names(df)) {
    df <- df %>% mutate(education_years = suppressWarnings(as.numeric(education_years)))
  }
  if ("gender" %in% names(df)) {
    df <- df %>%
      mutate(
        gender_label = case_when(
          as.character(gender) == "1" ~ "Male",
          as.character(gender) == "2" ~ "Female",
          as.character(gender) == "3" ~ "Other",
          TRUE ~ as.character(gender)
        ),
        gender_label = factor(gender_label, levels = c("Male", "Female", "Other"))
      )
  }
  
  # ── Occupation group (CR respondents only) ──────────────
  # Populated for clinicians/researchers (blank for PWA/FMC). Values like
  # "Researcher_Faculty", "OT_PT" — underscores replaced with spaces/
  # slashes for readability.
  if ("occupation_group" %in% names(df)) {
    df <- df %>%
      mutate(
        occupation_group = na_if(trimws(as.character(occupation_group)), ""),
        occupation_group = case_when(
          occupation_group == "OT_PT" ~ "OT/PT",
          !is.na(occupation_group)    ~ str_replace_all(occupation_group, "_", " "),
          TRUE ~ NA_character_
        ),
        occupation_group = factor(occupation_group)
      )
  }
  
  # ── Self-reported aphasia severity (PWA/FMC respondents only) ──
  if ("severity" %in% names(df)) {
    df <- df %>%
      mutate(
        severity = na_if(trimws(as.character(severity)), ""),
        severity = factor(severity, levels = c("Mild", "Moderate", "Severe"))
      )
  }
  
  # ── Months post-onset (MPO; PWA/FMC respondents only) ───
  if ("mpo" %in% names(df)) {
    df <- df %>% mutate(mpo = suppressWarnings(as.numeric(mpo)))
  }
  # ── Survey item cleaning ───────────────────────────────
  # Coerce to numeric; treat 0 and 6 as skip/invalid -> NA (same convention
  # as the original pipeline: 0 = not applicable, 6 = don't know on a 1-5
  # scale). Verify against your REDCap field's actual coding (Field Type /
  # Choices) and adjust `na_codes` below if your scale differs.
  na_codes <- c(0, 6)
  df_clean <- df %>%
    mutate(
      across(all_of(items_present), ~ suppressWarnings(as.numeric(.))),
      across(all_of(items_present), ~ ifelse(. %in% na_codes, NA, .))
    ) %>%
    filter(!is.na(stakeholder_group))
  
  cat("\nLoaded", nrow(df_clean), "respondents with a valid group label:\n")
  print(table(df_clean$stakeholder_group, useNA = "ifany"))
  
  domains_present <- lapply(DOMAINS_40, function(cols) intersect(cols, items_present))
  
  list(
    df_clean    = df_clean,
    items       = items_present,
    item_labels = ITEM_LABELS_40[items_present],
    domains     = domains_present
  )
}
