#' Data loading, cleaning, and group splitting
#'
#' Depends on: 00_config.R (define_domains, build_item_labels)

#' Load and clean the raw survey CSV.
#'
#' @param path  Path to the CSV file.
#' @return A list with:
#'   df_clean, domains, all_items, item_labels, df_pwa, df_fmc
load_and_clean <- function(path) {
  df <- read_csv(path, show_col_types = FALSE) %>%
    clean_names()

  domains     <- define_domains(names(df))
  all_items   <- unlist(domains, use.names = FALSE)
  item_labels <- build_item_labels(domains)

  # ── Stakeholder group ──────────────────────────────────
  df <- df %>%
    mutate(
      respondent = case_when(
        stakeholder_group == 1 ~ "PWA",
        stakeholder_group == 2 ~ "FMC",
        TRUE                   ~ NA_character_
      ),
      respondent = factor(respondent, levels = c("PWA", "FMC"))
    )

  # ── Aphasia severity ───────────────────────────────────
  if ("history_severity" %in% names(df)) {
    df <- df %>%
      mutate(
        history_severity = case_when(
          history_severity == 1 ~ "Mild",
          history_severity == 2 ~ "Moderate",
          history_severity == 3 ~ "Severe",
          TRUE                  ~ as.character(history_severity)
        ),
        history_severity = factor(
          history_severity,
          levels  = c("Mild", "Moderate", "Severe"),
          ordered = TRUE
        ),
        history_severity_num = as.numeric(history_severity)
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

  # ── Months post-onset ──────────────────────────────────
  if ("mpo" %in% names(df)) {
    df <- df %>%
      mutate(mpo = suppressWarnings(as.numeric(mpo)))
  }

  # ── Categorical demographics ───────────────────────────
  if ("region" %in% names(df)) df <- df %>% mutate(region = factor(region))
  if ("urban"  %in% names(df)) df <- df %>% mutate(urban  = factor(urban))
  if ("ses"    %in% names(df)) df <- df %>% mutate(ses    = factor(ses))

  # ── Harmonize race/ethnicity fields ───────────────────
  if (!"race_ethnicity" %in% names(df)) {
    if ("race" %in% names(df)) {
      df <- df %>% mutate(race_ethnicity = race)
    } else if ("ethnicity" %in% names(df)) {
      df <- df %>% mutate(race_ethnicity = ethnicity)
    }
  }

  # Numeric coding:
  # race:      1 = White, 2 = Black/AA, 3 = Asian,
  #            4 = American Indian/Alaska Native,
  #            5 = Native Hawaiian/PI, 6 = More Than One Race
  # ethnicity: 1 = Hispanic, 0 = Non-Hispanic
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
        ethnicity_label = factor(
          ethnicity_label,
          levels = c("Non-Hispanic", "Hispanic")
        )
      )
  }

  # Collapsed race/ethnicity:
  #   "White" = White AND Non-Hispanic only
  #   everyone else = Racially/ethnically minoritized
  if (all(c("race", "ethnicity") %in% names(df))) {
    df <- df %>%
      mutate(
        race_ethnicity_collapsed = case_when(
          is.na(race) | is.na(ethnicity)  ~ NA_character_,
          race == 1 & ethnicity == 0      ~ "White",
          TRUE                            ~ "Racially/ethnically minoritized"
        ),
        race_ethnicity_collapsed = factor(
          race_ethnicity_collapsed,
          levels = c("White", "Racially/ethnically minoritized")
        )
      )
  }

  # ── Survey item cleaning ───────────────────────────────
  # Coerce to numeric; treat 0 and 6 as skip/invalid → NA
  df_clean <- df %>%
    mutate(
      across(all_of(all_items), ~ suppressWarnings(as.numeric(.))),
      across(all_of(all_items), ~ ifelse(. %in% c(0, 6), NA, .))
    )

  list(
    df_clean    = df_clean,
    domains     = domains,
    all_items   = all_items,
    item_labels = item_labels,
    df_pwa      = df_clean %>% filter(stakeholder_group == 1),
    df_fmc      = df_clean %>% filter(stakeholder_group == 2)
  )
}
