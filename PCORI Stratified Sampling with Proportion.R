library(tidyverse)
library(splitstackshape)

set.seed(1234)

## LOAD AND PREPARE DATA ##

setwd("/Volumes/Kiran/KiranLab4/Students/Madeline Dunne/PCORI/PCORI R test/data")
intake_data <- read.csv("mock_pcori_intake_data (2).csv", stringsAsFactors = T)

# Calculate age from Date of Birth (format: YYYY-MM-DD)
intake_data <- intake_data %>%
  mutate(
    age = 2025 - as.numeric(substr(Date.of.Birth, 1, 4)),
    # Convert ADI columns to numeric
    adi_state_decile = as.numeric(ADI.State.Decile),
    adi_national_decile = as.numeric(ADI.National.Decile),
    years_education = as.numeric(Years.of.Education),
    ruca_code = as.numeric(RUCA)
  )

# Create eligibility flag (exclude multiple conditions)
intake_data <- intake_data %>%
  mutate(
    eligible = 
      Please.check.if.you.have.any.of.the.following.conditions...choice.Alzheimers.Disease. != "Checked" &
      Please.check.if.you.have.any.of.the.following.conditions...choice.Memory.Problems. != "Checked" &
      Please.check.if.you.have.any.of.the.following.conditions...choice.Learning.Disability. != "Checked" &
      Please.check.if.you.have.any.of.the.following.conditions...choice.Parkinsons.Disease. != "Checked" &
      Please.check.if.you.have.any.of.the.following.conditions...choice.Seizures. != "Checked"
  )

# Create all stratification variables
strata_data <- intake_data %>%
  filter(eligible == TRUE) %>%
  mutate(
    # Race/Ethnicity - Hispanic takes precedence
    race_stratum = case_when(
      Ethnicity == "Hispanic" ~ "Hispanic",
      Race == "Black or African American" ~ "Black",
      Race == "Asian" ~ "Asian",
      Race == "White" ~ "White",
      TRUE ~ "Other"
    ),
    # Age
    age_stratum = ifelse(age >= 60, "60+", "<60"),
    # Region
    region_stratum = case_when(
      State %in% c("FL", "GA", "SC", "NC", "VA", "WV", "KY", "TN", "AL", "MS", "AR", "LA") ~ "Southeast",
      State %in% c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA") ~ "Northeast",
      State %in% c("IL", "IN", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD") ~ "Midwest",
      TRUE ~ "West"
    ),
    # Urban/Rural based on RUCA codes (OFFICIAL CLASSIFICATION)
    # RUCA 1-3 = Urban, 4-6 = Large Rural, 7-9 = Small Rural, 10 = Remote Rural
    urban_stratum = case_when(
      ruca_code >= 1 & ruca_code <= 3 ~ "Urban",
      ruca_code >= 4 & ruca_code <= 6 ~ "Large Rural",
      ruca_code >= 7 & ruca_code <= 9 ~ "Small Rural",
      ruca_code == 10 ~ "Remote Rural",
      TRUE ~ "Unknown"
    ),
    # Simple urban/rural (for 80% urban target)
    urban_rural_simple = ifelse(ruca_code <= 3, "Urban", "Rural"),
    # SES based on ADI National Decile
    ses_stratum = case_when(
      adi_national_decile <= 30 ~ "Higher SES",
      adi_national_decile <= 70 ~ "Middle SES",
      adi_national_decile > 70 ~ "Lower SES",
      TRUE ~ "Unknown"
    ),
    # Gender
    gender_stratum = Gender
  )

print(paste("Total participants loaded:", nrow(intake_data)))
print(paste("Eligible participants:", nrow(strata_data)))
print(paste("Excluded participants:", nrow(intake_data) - nrow(strata_data)))

# Target n=125 (25% oversample for final n=100)
sample_sizes <- c(32, 24, 12, 28, 4)
names(sample_sizes) <- c("Black", "Hispanic", "Asian", "White", "Other")

print("\nAvailable participants by race_stratum:")
available_counts <- table(strata_data$race_stratum)
print(available_counts)

# Check if we have enough participants in each stratum
for (race in names(sample_sizes)) {
  available <- sum(strata_data$race_stratum == race)
  needed <- sample_sizes[race]
  if (available < needed) {
    warning(paste("Not enough", race, "participants. Available:", available, "Needed:", needed))
    # Adjust to available
    sample_sizes[race] <- available
  }
  print(paste(race, "- Available:", available, "Target:", needed, "Will sample:", sample_sizes[race]))
}

print("\nAdjusted sample sizes:")
print(sample_sizes)
print(paste("Total to sample:", sum(sample_sizes)))

# Use stratified() from splitstackshape
selected_sample <- stratified(
  indt = strata_data,
  group = "race_stratum",
  size = sample_sizes
)

print("\nStep 1: Race/Ethnicity Sampling")
print(table(selected_sample$race_stratum))


## VERIFY ALL DEMOGRAPHIC CRITERIA ##

# Age (Target: 90-95% aged 60+)
age_dist <- selected_sample %>%
  count(age_stratum) %>%
  mutate(percentage = round(100 * n / sum(n), 1))
print("Age Distribution (Target: 90-95% aged 60+):")
print(age_dist)

# Region (Target: Most from Southeast, fewest from Northeast)
region_dist <- selected_sample %>%
  count(region_stratum) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
print("\nRegion Distribution (Target: Southeast > West/Midwest > Northeast):")
print(region_dist)

# Urban/Rural (Target: 80% urban) - Using RUCA-based classification
urban_dist <- selected_sample %>%
  count(urban_rural_simple) %>%
  mutate(percentage = round(100 * n / sum(n), 1))
print("\nUrban/Rural Distribution (Target: 80% urban) - RUCA-based:")
print(urban_dist)

# Detailed RUCA classification
urban_detailed <- selected_sample %>%
  count(urban_stratum) %>%
  mutate(percentage = round(100 * n / sum(n), 1))
print("\nDetailed Urban/Rural Classification (RUCA):")
print(urban_detailed)

# Gender (Target: Equal)
gender_dist <- selected_sample %>%
  count(gender_stratum) %>%
  mutate(percentage = round(100 * n / sum(n), 1))
print("\nGender Distribution (Target: 50% male, 50% female):")
print(gender_dist)

# SES based on ADI (Target: Somewhat more lower SES)
ses_dist <- selected_sample %>%
  count(ses_stratum) %>%
  mutate(percentage = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n))
print("\nSES Distribution (ADI-based, Target: More lower SES representation):")
print(ses_dist)

# ADI summary statistics
adi_summary <- selected_sample %>%
  summarise(
    mean_adi_national = round(mean(adi_national_decile, na.rm = TRUE), 1),
    median_adi_national = median(adi_national_decile, na.rm = TRUE),
    mean_adi_state = round(mean(adi_state_decile, na.rm = TRUE), 1),
    median_adi_state = median(adi_state_decile, na.rm = TRUE)
  )
print("\nADI Summary Statistics:")
print(adi_summary)

# Severity Distribution
severity_dist <- selected_sample %>%
  count(What.is.your.perceived.stroke.severity.) %>%
  mutate(percentage = round(100 * n / sum(n), 1))
print("\nStroke Severity Distribution:")
print(severity_dist)

## ITERATIVE ADJUSTMENT (IF NEEDED) ##

# Check if criteria are met (using RUCA-based urban/rural)
pct_60plus <- 100 * sum(selected_sample$age >= 60) / nrow(selected_sample)
pct_urban <- 100 * sum(selected_sample$urban_rural_simple == "Urban", na.rm = TRUE) / nrow(selected_sample)
pct_southeast <- 100 * sum(selected_sample$region_stratum == "Southeast") / nrow(selected_sample)

criteria_met <- tibble(
  Criterion = c("Age 60+", "Urban (RUCA)", "Southeast Region"),
  Target = c("90-95%", "80%", "35-40%"),
  Actual = c(paste0(round(pct_60plus, 1), "%"),
             paste0(round(pct_urban, 1), "%"),
             paste0(round(pct_southeast, 1), "%")),
  Status = c(
    ifelse(pct_60plus >= 90 & pct_60plus <= 95, "✓", "Adjust"),
    ifelse(pct_urban >= 75 & pct_urban <= 85, "✓", "Adjust"),
    ifelse(pct_southeast >= 30 & pct_southeast <= 45, "✓", "Adjust")
  )
)

print("\nCriteria Status:")
print(criteria_met)

# If adjustments needed, use weighted sampling
if (any(criteria_met$Status == "Adjust")) {
  print("\nPerforming weighted adjustment...")
  
  # Calculate sampling weights based on multiple criteria
  strata_data <- strata_data %>%
    mutate(
      weight = case_when(
        # Higher weight for 60+, urban (RUCA 1-3), Southeast, lower SES (ADI > 70)
        age >= 60 & ruca_code <= 3 & 
          region_stratum == "Southeast" & adi_national_decile > 70 ~ 4,
        age >= 60 & ruca_code <= 3 ~ 3,
        age >= 60 | ruca_code <= 3 ~ 2,
        TRUE ~ 1
      )
    )
  
  # Re-sample with weights using loop
  selected_list <- list()
  
  for (race in names(sample_sizes)) {
    stratum_data <- strata_data %>% filter(race_stratum == race)
    n_sample <- sample_sizes[[race]]
    
    if (nrow(stratum_data) >= n_sample) {
      # Weighted sampling
      sampled_indices <- sample(
        x = 1:nrow(stratum_data),
        size = n_sample,
        replace = FALSE,
        prob = stratum_data$weight
      )
      selected_list[[race]] <- stratum_data[sampled_indices, ]
    } else {
      warning(paste("Not enough", race, "participants"))
      selected_list[[race]] <- stratum_data
    }
  }
  
  selected_sample <- bind_rows(selected_list)
  
  # Re-check criteria
  pct_60plus <- 100 * sum(selected_sample$age >= 60) / nrow(selected_sample)
  pct_urban <- 100 * sum(selected_sample$urban_rural_simple == "Urban", na.rm = TRUE) / nrow(selected_sample)
  pct_southeast <- 100 * sum(selected_sample$region_stratum == "Southeast") / nrow(selected_sample)
  
  print("\nAfter weighted adjustment:")
  print(paste("Age 60+:", round(pct_60plus, 1), "%"))
  print(paste("Urban (RUCA):", round(pct_urban, 1), "%"))
  print(paste("Southeast:", round(pct_southeast, 1), "%"))
}


## COMPREHENSIVE SUMMARY ##

summary_stats <- selected_sample %>%
  summarise(
    n = n(),
    mean_age = round(mean(age, na.rm = TRUE), 1),
    sd_age = round(sd(age, na.rm = TRUE), 1),
    median_education = median(years_education, na.rm = TRUE),
    mean_adi_national = round(mean(adi_national_decile, na.rm = TRUE), 1),
    mean_adi_state = round(mean(adi_state_decile, na.rm = TRUE), 1),
    pct_60plus = round(100 * sum(age >= 60) / n(), 1),
    pct_female = round(100 * sum(Gender == "Female") / n(), 1),
    pct_male = round(100 * sum(Gender == "Male") / n(), 1),
    pct_urban_ruca = round(100 * sum(urban_rural_simple == "Urban", na.rm = TRUE) / n(), 1),
    pct_southeast = round(100 * sum(region_stratum == "Southeast") / n(), 1),
    pct_black = round(100 * sum(race_stratum == "Black") / n(), 1),
    pct_hispanic = round(100 * sum(race_stratum == "Hispanic") / n(), 1),
    pct_asian = round(100 * sum(race_stratum == "Asian") / n(), 1)
  )

print("\n=== FINAL SAMPLE SUMMARY ===")
print(summary_stats)


## STATISTICAL TESTS ##

# Chi-square test for race/ethnicity
expected_race_prop <- c(Black = 0.32, Hispanic = 0.24, Asian = 0.12, White = 0.28, Other = 0.04)
observed_race <- table(selected_sample$race_stratum)

# Check if any expected counts are too small (< 5)
expected_counts <- expected_race_prop * nrow(selected_sample)
if (any(expected_counts < 5)) {
  print("\nNote: Some expected counts < 5, chi-square approximation may not be accurate")
  print("Expected counts:")
  print(expected_counts)
}

chi_race <- suppressWarnings(chisq.test(x = observed_race, p = expected_race_prop))

# Chi-square test for region
expected_region_prop <- c(Southeast = 0.38, West = 0.23, Midwest = 0.23, Northeast = 0.16)
observed_region <- table(selected_sample$region_stratum)

expected_region_counts <- expected_region_prop * nrow(selected_sample)
if (any(expected_region_counts < 5)) {
  print("\nNote: Some expected region counts < 5, chi-square approximation may not be accurate")
}

chi_region <- suppressWarnings(chisq.test(x = observed_region, p = expected_region_prop))

print("\nStatistical Tests:")
print(paste("Race/Ethnicity: X² =", round(chi_race$statistic, 3), ", p =", round(chi_race$p.value, 4)))
print(paste("Region: X² =", round(chi_region$statistic, 3), ", p =", round(chi_region$p.value, 4)))


## EXPORT ##

write.csv(selected_sample, "selected_participants.csv", row.names = FALSE)

# Create detailed verification report
verification_report <- tibble(
  Demographic = c("Total Sample", "Age 60+", "Female", "Male", 
                  "Urban (RUCA)", "Rural (RUCA)",
                  "Southeast", "Northeast", "Midwest", "West",
                  "Black", "Hispanic", "Asian", "White", "Other",
                  "Higher SES (ADI ≤30)", "Middle SES (ADI 31-70)", "Lower SES (ADI >70)"),
  Target = c("125", "90-95%", "~50%", "~50%", 
             "80%", "20%",
             "35-40%", "15-20%", "20-25%", "20-25%",
             "32%", "24%", "12%", "28%", "4%",
             "-", "-", "Higher proportion"),
  Actual = c(
    nrow(selected_sample),
    paste0(round(100 * sum(selected_sample$age >= 60) / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$Gender == "Female") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$Gender == "Male") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$urban_rural_simple == "Urban", na.rm = TRUE) / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$urban_rural_simple == "Rural", na.rm = TRUE) / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$region_stratum == "Southeast") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$region_stratum == "Northeast") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$region_stratum == "Midwest") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$region_stratum == "West") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$race_stratum == "Black") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$race_stratum == "Hispanic") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$race_stratum == "Asian") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$race_stratum == "White") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$race_stratum == "Other") / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$adi_national_decile <= 30, na.rm = TRUE) / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$adi_national_decile > 30 & selected_sample$adi_national_decile <= 70, na.rm = TRUE) / nrow(selected_sample), 1), "%"),
    paste0(round(100 * sum(selected_sample$adi_national_decile > 70, na.rm = TRUE) / nrow(selected_sample), 1), "%")
  )
)

write.csv(verification_report, "demographic_verification.csv", row.names = FALSE)
write.csv(summary_stats, "sample_summary.csv", row.names = FALSE)

# Create contact list with PCORI-specific columns
contact_list <- selected_sample %>%
  select(
    Record.ID,
    First.Name,
    Last.Name,
    Email,
    Phone.Number,
    City,
    State,
    Zip.code,
    race_stratum,
    age,
    Gender,
    urban_rural_simple,
    adi_national_decile,
    What.is.your.perceived.stroke.severity.
  )

write.csv(contact_list, "participant_contact_list.csv", row.names = FALSE)

print("\nFiles exported:")
print("  - selected_participants.csv (full data)")
print("  - demographic_verification.csv (target vs actual)")
print("  - sample_summary.csv (summary statistics)")
print("  - participant_contact_list.csv (contact info)")

print("\n=== SAMPLING COMPLETE ===")
print(paste("Selected", nrow(selected_sample), "participants"))
print("Urban/Rural classification based on RUCA codes")
print("SES classification based on ADI National Decile")

