#' Data loading, cleaning, and demographic preparation
#'
#' Single stakeholder group (Clinicians / Researchers): no group split.
#'
#' Depends on: 00_config.R (define_domains, build_item_labels)

# ── US state → Census region lookup ────────────────────────
# The template carries `state` rather than a pre-coded `region`. Region
# analysis is derived here from the four US Census regions so that the
# region association analyses can run.
.US_STATE_REGION <- c(
  # Northeast
  CT = "Northeast", ME = "Northeast", MA = "Northeast", NH = "Northeast",
  RI = "Northeast", VT = "Northeast", NJ = "Northeast", NY = "Northeast",
  PA = "Northeast",
  # Midwest
  IL = "Midwest", IN = "Midwest", MI = "Midwest", OH = "Midwest",
  WI = "Midwest", IA = "Midwest", KS = "Midwest", MN = "Midwest",
  MO = "Midwest", NE = "Midwest", ND = "Midwest", SD = "Midwest",
  # South
  DE = "South", FL = "South", GA = "South", MD = "South", NC = "South",
  SC = "South", VA = "South", DC = "South", WV = "South", AL = "South",
  KY = "South", MS = "South", TN = "South", AR = "South", LA = "South",
  OK = "South", TX = "South",
  # West
  AZ = "West", CO = "West", ID = "West", MT = "West", NV = "West",
  NM = "West", UT = "West", WY = "West", AK = "West", CA = "West",
  HI = "West", OR = "West", WA = "West"
)

#' Load and clean the raw survey CSV.
#'
#' @param path  Path to the CSV file.
#' @return A list with:
#'   df_clean, domains, all_items, item_labels
load_and_clean <- function(path) {
  df <- read_csv(path, show_col_types = FALSE) %>%
    clean_names()

  domains     <- define_domains(names(df))
  all_items   <- unlist(domains, use.names = FALSE)
  item_labels <- build_item_labels(domains)

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

  # ── Occupation group ───────────────────────────────────
  # Primary stratifier for this stakeholder group (e.g. clinician vs
  # researcher vs clinician-researcher). Kept as a factor for KW/Dunn and
  # for use as a covariate in the mixed models.
  if ("occupation_group" %in% names(df)) {
    df <- df %>% mutate(occupation_group = factor(occupation_group))
  }

  # ── Region (derived from state) ────────────────────────
  if ("region" %in% names(df)) {
    df <- df %>% mutate(region = factor(region))
  } else if ("state" %in% names(df)) {
    df <- df %>%
      mutate(
        state_abbr = toupper(trimws(as.character(state))),
        region     = unname(.US_STATE_REGION[state_abbr]),
        region     = factor(region, levels = c("Northeast", "Midwest", "South", "West"))
      )
  }

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
    item_labels = item_labels
  )
}
