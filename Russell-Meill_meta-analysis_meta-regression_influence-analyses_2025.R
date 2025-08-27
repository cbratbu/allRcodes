library(bannerCommenter)
library(metafor)
library(dplyr)
library(meta)
library(robumeta)
library(dmetar)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(robvis)
library(metasens)
library(flextable)
library(officer)


# LOAD DATA ---------------------------------------------------------------
data <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/outcomes.csv", stringsAsFactors = TRUE)
data_post <- data %>% filter(timepoint == "Post-treatment")
outcomes <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/post_tx_outcomes2.csv", stringsAsFactors = TRUE)
 
# Separate the data into experimental and control groups
experimental <- data_post %>%
  filter(study_group == "Experimental") %>%
  select(study_ID, outcome_measure, outcome_designation_specific, outcome_designation_broad, outcome_notes,
         exp_group_description, ctrl_group_description, control_group_type, acute_chronic, non.language.cointervention,
         X3_plus_arms, plus_UC, unsure_of_outcome, include_exhaustive, include_simplified, simplified_outcome_rationale,
         missing_data, notes, reverse_scoring, primary_outcome_reported, mean, SD, n) %>%
  rename(exp_mean = mean, exp_SD = SD, exp_n = n)

control <- data_post %>% filter(study_group == "Control") %>%
  select(study_ID, outcome_measure, outcome_designation_specific, mean, SD, n) %>%
  rename(ctrl_mean = mean, ctrl_SD = SD, ctrl_n = n)

# Merge the experimental and control data back together
merged_data <- experimental %>%
  left_join(control, by = c("study_ID", "outcome_measure", "outcome_designation_specific"))


# CALCULATE SMD -----------------------------------------------------------


# Ensure numeric columns are correctly formatted
numeric_cols <- c("exp_mean", "exp_SD", "exp_n", "ctrl_mean", "ctrl_SD", "ctrl_n")
outcomes[numeric_cols] <- lapply(outcomes[numeric_cols], function(x) as.numeric(as.character(x)))

# Define the calculate_smd function using escalc from metafor package
calculate_smd <- function(exp_mean, exp_SD, exp_n, ctrl_mean, ctrl_SD, ctrl_n) {
  result <- escalc(measure = "SMD", 
                   m1i = exp_mean, sd1i = exp_SD, n1i = exp_n,
                   m2i = ctrl_mean, sd2i = ctrl_SD, n2i = ctrl_n)
  return(result)
}

# Apply the function to each row
smd_results <- outcomes %>%
  rowwise() %>%
  mutate(result = list(calculate_smd(exp_mean, exp_SD, exp_n, ctrl_mean, ctrl_SD, ctrl_n)),
         SMD = result$yi,
         var_SMD = result$vi,
         SE_SMD = sqrt(result$vi)) %>%
  ungroup() %>%
  select(-result)

# Print the results
print(smd_results)

smd_results <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/outcomes_smd_new_rob_final.csv", stringsAsFactors = TRUE)
smd_results$SMD <- as.numeric(as.character(smd_results$SMD))


# Averaged ----------------------------------------------------------------


avg <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/rve.csv", stringsAsFactors = TRUE)



# Ensure numeric columns are correctly formatted
cols_to_convert <- c("SMD", "var_SMD")
avg[cols_to_convert] <- lapply(avg[cols_to_convert], function(x) as.numeric(as.character(x)))

# Function to calculate averaged SMD and averaged variance for each study
calculate_averaged_SMD <- function(data) {
  data <- data %>%
    summarise(
      # Averaging the SMD across the subtests
      averaged_SMD = mean(SMD),
      
      # Averaging the variances of the subtests
      averaged_var_SMD = mean(var_SMD)
    )
  
  # Calculating standard error based on the averaged variance
  data$averaged_SE_SMD <- sqrt(data$averaged_var_SMD)
  
  return(data)
}

# Group by study_ID and calculate the averaged effect size and variance
pooled_results_avg <- avg %>%
  group_by(study_ID) %>%
  do(calculate_averaged_SMD(.))

# Print the results
print(pooled_results_avg)

# Export the pooled results to an Excel file
write.csv(pooled_results_avg, "pooled_results_avg.csv")

# If you want to group by both study_ID and a broader outcome category
pooled_results_avg_grouped <- avg %>%
  group_by(study_ID, outcome_designation_broad) %>%
  do(calculate_averaged_SMD(.))

# Print the grouped results
print(pooled_results_avg_grouped)

# Export the grouped results
write.csv(pooled_results_avg_grouped, "pooled_results_avg_grouped2.csv")


# Outcome Planning --------------------------------------------------------

# Define the list of outcome measures based on the actual column names
outcome_measures <- c("aphasia severity", "discourse", "expressive language", 
                      "functional communication", "intelligibility", 
                      "psychosocial impact", "reading", "receptive language", "writing")

# Filter data to include only the specified outcome measures
filtered_data <- smd_results %>%
  filter(outcome_designation_broad %in% outcome_measures) %>%
  select(study_ID, control_group_type_collapsed, outcome_designation_broad) %>%
  distinct()

# Create a table with unique study counts
pivot_data <- filtered_data %>%
  mutate(count = 1) %>%
  pivot_wider(names_from = outcome_designation_broad, values_from = count, values_fill = list(count = 0), values_fn = list(count = ~ 1))

# Summarize the data to count unique studies per control group type and outcome measure
control_group_summary <- pivot_data %>%
  group_by(control_group_type_collapsed) %>%
  summarise(across(all_of(outcome_measures), sum, na.rm = TRUE))

# Convert the summary into a long format for easier interpretation
control_group_summary_long <- control_group_summary %>%
  pivot_longer(cols = all_of(outcome_measures), names_to = "outcome_measure", values_to = "study_count") %>%
  arrange(control_group_type_collapsed, outcome_measure)

# Print the summary
View(control_group_summary_long)


# POWER ANALYSIS ----------------------------------------------------------

# Example subgroup results
subgroup_results <- data.frame(
  group = c("active comparator", "usual care"),
  SMD = c(0.0458, 0.3563),
  SE = c(0.1263, 0.2765)
)

# Perform power analysis between two subgroups
TE1 <- subgroup_results$SMD[1]
TE2 <- subgroup_results$SMD[2]
seTE1 <- subgroup_results$SE[1]
seTE2 <- subgroup_results$SE[2]

# Perform power analysis
result <- power.analysis.subgroup(TE1 = TE1, TE2 = TE2, seTE1 = seTE1, seTE2 = seTE2)

# Print results
print(result)


# SEVERITY ----------------------------------------------------------------

outcomes_broad <- smd_results %>% filter(include_broad == "Y")
outcomes_broad$sequence_generation <- relevel(factor(outcomes_broad$sequence_generation), ref = "Low")
outcomes_broad$allocation_concealment <- relevel(factor(outcomes_broad$allocation_concealment), ref = "Low")
outcomes_broad$outcome_blinding <- relevel(factor(outcomes_broad$outcome_blinding), ref = "Low")
outcomes_broad$incomplete_outcome_data <- relevel(factor(outcomes_broad$incomplete_outcome_data), ref = "Low")
outcomes_broad$other_bias <- relevel(factor(outcomes_broad$other_bias), ref = "Low")
severity_broad <- outcomes_broad %>% filter(outcome_designation_broad == "aphasia severity")
severity_broad$SMD <- as.numeric(as.character(severity_broad$SMD))


m.sev.broad <- metagen(TE =SMD,
                            seTE = SE_SMD,
                            studlab = study_ID,
                            data = severity_broad,
                            sm = "SMD",
                            fixed = FALSE,
                            random = TRUE,
                            method.tau = "REML",
                            method.random.ci = "HK",
                            title = "Aphasia Severity - broad")

# Severity forest plots ---------------------------------------------------
# Extract the subgroup levels
subgroups <- unique(severity_broad_clean$control_group_type_collapsed)

# Loop through each subgroup and create a forest plot
for (subgroup in subgroups) {
  # Subset the data for the current subgroup
  subgroup_data <- subset(severity_broad_clean, control_group_type_collapsed == subgroup)
  
  # Check if there is enough data for a meta-analysis in this subgroup
  if (nrow(subgroup_data) > 1) {
    # Create a meta-analysis object for the current subgroup
    m.subgroup <- metagen(TE = subgroup_data$SMD, 
                          seTE = subgroup_data$SE_SMD, 
                          studlab = subgroup_data$study_ID,
                          data = subgroup_data,
                          sm = "SMD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = paste("Forest plot for subgroup:", subgroup))
    
    # Save each forest plot as a PDF
    pdf(paste0("plot_sev_", subgroup, ".pdf"), width = 10, height = 8)
    
    # Create a forest plot for the current subgroup
    forest(m.subgroup, 
           comb.fixed = FALSE, # Do not combine subgroups
           comb.random = TRUE, # Combine using random effects model
           print.subgroup.labels = TRUE, # Print subgroup labels
           print.subgroup.names = TRUE, # Print names of subgroups
           col.subgroup = "blue", # Color for subgroup summary lines
           col.study = "black", # Color for individual study lines
           col.diamond = "red", # Color for overall summary diamond
           col.diamond.lines = "blue", # Color for the lines of the diamond
           layout = "RevMan5", # Layout style
           digits = 2) # Number of digits to display
    
    # Close the PDF device
    dev.off()
  } else {
    cat("Not enough data for subgroup:", subgroup, "\n")
  }
}




# SEVERITY - heterogeneity ------------------------------------------------
# If this doesn't run, make sure ggplot, ggrepel, and gridExtra are loaded
m.sev.broad.outliers <- find.outliers(m.sev.broad)
m.sev.broad.inf <- InfluenceAnalysis(m.sev.broad, random = TRUE)

flextable(m.sev.broad.outliers)

# Visualize
plot(m.sev.broad.inf, "baujat")
plot(m.sev.broad.inf, "influence")
plot(m.sev.broad.inf, "es")

# Re-run with removals
exclude_studies_sev <- c("Akabogu et al. 2019", "Teng 2017")


# Filter the dataset

severity_broad_clean <- severity_broad %>% filter(!(study_ID %in% exclude_studies_sev))
# Re-run the meta-analysis with the cleaned dataset

m.sev.broad.clean <- metagen(TE = SMD,
                               seTE = SE_SMD,
                               studlab = study_ID,
                               data = severity_broad_clean,
                               sm = "SMD",
                               fixed = FALSE,
                               random = TRUE,
                               method.tau = "REML",
                               method.random.ci = "HK",
                               title = "Aphasia Severity - broad (Cleaned)")

summary(m.sev.broad.clean)






# SEVERITY - subgroup analysis --------------------------------------------


m.sev.broad.subgroup.collapsed <- update(m.sev.broad, 
                                 subgroup = control_group_type_collapsed, 
                                 tau.common = FALSE)

m.sev.broad.subgroup <- update(m.sev.broad, 
                                         subgroup = control_group_type, 
                                         tau.common = FALSE)
# Clean
m.sev.broad.clean.subgroup.collapsed <- update(m.sev.broad.clean, 
                                       subgroup = control_group_type_collapsed, 
                                       tau.common = FALSE)
summary(m.sev.broad.clean.subgroup.collapsed)

m.sev.broad.clean.subgroup <- update(m.sev.broad.clean, 
                                               subgroup = control_group_type, 
                                               tau.common = FALSE)
summary(m.sev.broad.clean.subgroup)

# SEVERITY - tables -------------------------------------------------------

# Extract subgroup results from the model
subgroup_results_collapsed <- as.data.frame(summary(m.sev.broad.clean.subgroup.collapsed)$subgroup)
subgroup_results_collapsed$Subgroup <- rownames(subgroup_results_collapsed)
rownames(subgroup_results_collapsed) <- NULL

# Export to CSV
write.csv(subgroup_results_collapsed, "subgroup_results_collapsed.csv", row.names = FALSE)



# Passive Active ----------------------------------------------------------



outcomes_broad$control_category <- ifelse(outcomes_broad$control_group_type %in% c("waitlist", "no treatment"), 
                                "Passive", "Active")



m.sev.broad.clean.subgroup.category <- update(m.sev.broad.clean, 
                                     subgroup = control_category, 
                                     tau.common = FALSE)


# DOI Plots - Severity ----------------------------------------------------

# SEVERITY OVERALL
doiplot(m.sev.broad.clean)
title("DOI Plot for Aphasia Severity")


# Subset the data for the "usual care - protocolized" group
subgroup_data_usual_protocolized <- update(m.sev.broad.clean, subset = (control_group_type == "usual care - protocolized"))

# Generate DOI plot for "usual care - protocolized"
doiplot(subgroup_data_usual_protocolized)
title("DOI Plot for Usual Care - Protocolized")


# Subset the data for the "usual care" group
subgroup_data_usual <- update(m.sev.broad.clean, subset = (control_group_type_collapsed == "usual care"))

# Generate DOI plot for "usual care"
doiplot(subgroup_data_usual)
title("DOI Plot for Usual Care")



lfk_result_usual_protocolized <- lfkindex(subgroup_data_usual_protocolized$TE, 
                                          subgroup_data_usual_protocolized$seTE)

# Print the LFK index results
print(lfk_result_usual_protocolized)

# Subset the data for the "active comparator - delivery model" group
subgroup_data_active_delivery <- update(m.sev.broad.clean, subset = (control_group_type == "active comparator - delivery model"))

# Subset the data for the "active comparator - delivery model" group
subgroup_data_active <- update(m.sev.broad.clean, subset = (control_group_type_collapsed == "active comparator"))

# Generate DOI plot for "active comparator"
doiplot(subgroup_data_active)
title("DOI Plot for Active Comparator")

# Subset the data for the "active comparator - delivery model" group
subgroup_data_active_delivery <- update(m.sev.broad.clean, subset = (control_group_type == "active comparator - delivery model"))

# Generate DOI plot for "active comparator - delivery model"
doiplot(subgroup_data_active_delivery)
title("DOI Plot for Active Comparator - Delivery Model")

# Subset the data for the "active comparator - theoretical" group
subgroup_data_active_theoretical <- update(m.sev.broad.clean, subset = (control_group_type == "active comparator - theoretical"))

# Generate DOI plot for "active comparator - theoretical"
doiplot(subgroup_data_active_theoretical)
title("DOI Plot for Active Comparator - Theoretical")

lfkindex(m.sev.broad.clean, subset = (control_group_type == "usual care - protocolized"))




## Play
# Function to update model with valid subgroups
update_model_with_valid_subgroups <- function(data, outcome_var, se_var, study_var, subgroup_var, min_studies = 5, method.tau = "REML", method.random.ci = "HK", title = "Meta-Analysis") {
  # Calculate the number of studies in each subgroup
  subgroup_counts <- table(data[[subgroup_var]])
  
  # Get the names of subgroups with more than the specified minimum number of studies
  valid_subgroups <- names(subgroup_counts[subgroup_counts >= min_studies])
  
  # Subset the data to include only these valid subgroups
  data_valid <- data %>% filter(data[[subgroup_var]] %in% valid_subgroups)
  
  # Re-run the meta-analysis with the valid subgroups
  model <- metagen(TE = data_valid[[outcome_var]],
                   seTE = data_valid[[se_var]],
                   studlab = data_valid[[study_var]],
                   data = data_valid,
                   sm = "SMD",
                   fixed = FALSE,
                   random = TRUE,
                   method.tau = method.tau,
                   method.random.ci = method.random.ci,
                   title = title)
  
  # Update the model with subgroup analysis
  model_subgroup <- update(model, 
                           subgroup = data_valid[[subgroup_var]], 
                           tau.common = FALSE)
  
  return(model_subgroup)
}

# Apply the function to the severity analysis model
m.sev.primary.valid.subgroup <- update_model_with_valid_subgroups(
  data = severity_primary_clean, 
  outcome_var = "SMD", 
  se_var = "SE_SMD", 
  study_var = "study_ID", 
  subgroup_var = "control_group_type_collapsed", 
  min_studies = 5, 
  method.tau = "REML", 
  method.random.ci = "HK", 
  title = "Aphasia Severity - Primary (Valid Subgroups)"
)

# Print the summary
summary(m.sev.primary.valid.subgroup)


# Severity Bubble Plots ---------------------------------------------------
smd_results <- smd_results %>%
  mutate(total_n = exp_n + ctrl_n) 
## BUBBLE PLOT

# Step 2: Filter the dataframe to exclude outliers and keep only aphasia severity data
filtered_data_severity <- smd_results %>%
  filter(include_broad == "Y" & 
           outcome_designation_broad == "aphasia severity" & 
           !(study_ID %in% exclude_studies_sev))  # Exclude outliers

# Step 3: Aggregate data to count the number of studies per control group type and sum total participants
agg_data_collapsed_severity <- filtered_data_severity %>%
  group_by(control_group_type_collapsed) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

agg_data_specific_severity <- filtered_data_severity %>%
  group_by(control_group_type) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

# COLLAPSED PLOT 
ggplot(agg_data_collapsed_severity, aes(x = num_studies, y = control_group_type_collapsed, color = control_group_type_collapsed)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Collapsed) vs. Study Count (Aphasia Severity)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend

# SPECIFIC PLOT 
ggplot(agg_data_specific_severity, aes(x = num_studies, y = control_group_type, color = control_group_type)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Collapsed) vs. Study Count (Aphasia Severity)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend



# EXPRESSIVE LANGUAGE -----------------------------------------------------
exp_broad <- outcomes_broad %>% filter(outcome_designation_broad == "expressive language")


m.exp.broad <- metagen(TE =SMD,
                     seTE = SE_SMD,
                     studlab = study_ID,
                     data = exp_broad,
                     sm = "SMD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Expressive Language - broad")


# Expressive forest plots -------------------------------------------------
subgroups <- unique(exp_broad_clean$control_group_type_collapsed)

# Loop through each subgroup and create a forest plot
for (subgroup in subgroups) {
  # Subset the data for the current subgroup
  subgroup_data <- subset(exp_broad_clean, control_group_type_collapsed == subgroup)
  
  # Check if there is enough data for a meta-analysis in this subgroup
  if (nrow(subgroup_data) > 1) {
    # Create a meta-analysis object for the current subgroup
    m.subgroup <- metagen(TE = subgroup_data$SMD, 
                          seTE = subgroup_data$SE_SMD, 
                          studlab = subgroup_data$study_ID,
                          data = subgroup_data,
                          sm = "SMD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = paste("Forest plot for subgroup:", subgroup))
    
    # Save each forest plot as a PDF
    pdf(paste0("plot_exp_broad_", subgroup, ".pdf"), width = 10, height = 8)
    
    # Create a forest plot for the current subgroup
    forest(m.subgroup, 
           comb.fixed = FALSE, # Do not combine subgroups
           comb.random = TRUE, # Combine using random effects model
           print.subgroup.labels = TRUE, # Print subgroup labels
           print.subgroup.names = TRUE, # Print names of subgroups
           col.subgroup = "blue", # Color for subgroup summary lines
           col.study = "black", # Color for individual study lines
           col.diamond = "red", # Color for overall summary diamond
           col.diamond.lines = "blue", # Color for the lines of the diamond
           layout = "RevMan5", # Layout style
           digits = 2) # Number of digits to display
    
    # Close the PDF device
    dev.off()
  } else {
    cat("Not enough data for subgroup:", subgroup, "\n")
  }
}




# EXPRESSIVE - heterogeneity -----------------------------------------------


m.exp.broad.outliers <- find.outliers(m.exp.broad)
m.exp.broad.inf <- InfluenceAnalysis(m.exp.broad, random = TRUE)
# Visualize
plot(m.exp.broad.inf, "baujat")
plot(m.exp.broad.inf, "influence")
plot(m.exp.broad.inf, "es")

# Re-run with removals
exclude_studies_exp <- c("van der Meulen et al. 2014", "Teng 2017")

# Filter the dataset
exp_broad_clean <- exp_broad %>% filter(!(study_ID %in% exclude_studies_exp))
# Re-run the meta-analysis with the cleaned dataset

m.exp.broad.clean <- metagen(TE = SMD,
                           seTE = SE_SMD,
                           studlab = study_ID,
                           data = exp_broad_clean,
                           sm = "SMD",
                           fixed = FALSE,
                           random = TRUE,
                           method.tau = "REML",
                           method.random.ci = "HK",
                           title = "Expressive Language - broad (Cleaned)")
summary(m.exp.broad.clean)
# Expressive Bubble Plots ---------------------------------------------------

## BUBBLE PLOT

# Step 2: Filter the dataframe to exclude outliers and keep only aphasia severity data
filtered_data_exp <- smd_results %>%
  filter(include_broad == "Y" & 
           outcome_designation_broad == "expressive language" & 
           !(study_ID %in% exclude_studies_exp))  # Exclude outliers

# Step 3: Aggregate data to count the number of studies per control group type and sum total participants
agg_data_collapsed_exp <- filtered_data_exp %>%
  group_by(control_group_type_collapsed) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

agg_data_specific_exp <- filtered_data_exp %>%
  group_by(control_group_type) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

# COLLAPSED PLOT 
ggplot(agg_data_collapsed_exp, aes(x = num_studies, y = control_group_type_collapsed, color = control_group_type_collapsed)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Collapsed) vs. Study Count (Expressive Language)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend

# SPECIFIC PLOT 
ggplot(agg_data_specific_exp, aes(x = num_studies, y = control_group_type, color = control_group_type)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Collapsed) vs. Study Count (Expressive Language)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend

# EXPRESSIVE - subgroups --------------------------------------------------


m.exp.broad.subgroup.collapsed <- update(m.exp.broad, 
                             subgroup = control_group_type_collapsed, 
                             tau.common = FALSE)

m.exp.broad.subgroup <- update(m.exp.broad, 
                                         subgroup = control_group_type, 
                                         tau.common = FALSE)

m.exp.broad.clean.subgroup <- update(m.exp.broad.clean, 
                                   subgroup = control_group_type, 
                                   tau.common = FALSE)
m.exp.broad.clean.subgroup.collapsed <- update(m.exp.broad.clean, 
                                     subgroup = control_group_type_collapsed, 
                                     tau.common = FALSE)

summary(m.exp.broad.subgroup.collapsed)
summary(m.exp.broad.subgroup)
summary(m.exp.broad.clean.subgroup)
summary(m.exp.broad.clean.subgroup.collapsed)

# DOI Plots - Expressive --------------------------------------------------

# OVERALL 
doiplot(m.exp.broad)
title("DOI Plot for Expressive Language")


# Subset the data for the "active comparator" group
subgroup_data_active_comparator <- update(m.exp.broad.clean.subgroup, subset = (control_group_type_collapsed == "active comparator"))

# Generate DOI plot for "active comparator"
doiplot(subgroup_data_active_comparator)
title("DOI Plot for Active Comparator")

# Subset the data for the "active comparator" group
subgroup_data_active_comparator <- update(m.exp.broad.clean.subgroup, subset = (control_group_type == "active comparator - theoretical"))

# Generate DOI plot for "active comparator"
doiplot(subgroup_data_active_comparator)
title("DOI Plot for Active Comparator - theoret")



# Subset the data for the "usual care" group
subgroup_data_usual_care <- update(m.exp.broad.clean, subset = (control_group_type_collapsed == "usual care"))

# Generate DOI plot for "usual care"
doiplot(subgroup_data_usual_care)
title("DOI Plot for Usual Care")

# Subset the data for the "usual care - protocolized" group
subgroup_data_usual_care_protocolized <- update(m.exp.broad.clean, subset = (control_group_type == "usual care - protocolized"))

# Generate DOI plot for "usual care - protocolized"
doiplot(subgroup_data_usual_care_protocolized)
title("DOI Plot for Usual Care - protocolized")


# Subset the data for the "social support" group
subgroup_data_social_support <- update(m.exp.broad.clean, subset = (control_group_type_collapsed == "social support"))

# Generate DOI plot for "social support"
doiplot(subgroup_data_social_support)
title("DOI Plot for Social Support")

# RECEPTIVE LANGUAGE ------------------------------------------------------

rec_broad <- outcomes_broad %>% filter(outcome_designation_broad == "receptive language")

m.rec.broad <- metagen(TE =SMD,
                     seTE = SE_SMD,
                     studlab = study_ID,
                     data = rec_broad,
                     sm = "SMD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Receptive Language - broad")

# RECEPTIVE - heterogeneity -----------------------------------------------

m.rec.broad.outliers <- find.outliers(m.rec.broad)
m.rec.broad.inf <- InfluenceAnalysis(m.rec.broad, random = TRUE)
# Visualize
plot(m.rec.broad.inf, "baujat")
plot(m.rec.broad.inf, "influence")
plot(m.rec.broad.inf, "es")

# Re-run with removals (none)
exclude_studies_rec <- 

# Filter the dataset
rec_broad_clean <- rec_broad %>% filter(!(study_ID %in% exclude_studies_rec))

# Re-run the meta-analysis with the cleaned dataset
m.rec.broad.clean <- metagen(TE = SMD,
                           seTE = SE_SMD,
                           studlab = study_ID,
                           data = rec_broad_clean,
                           sm = "SMD",
                           fixed = FALSE,
                           random = TRUE,
                           method.tau = "REML",
                           method.random.ci = "HK",
                           title = "Receptive Language - broad (Cleaned)")


# RECEPTIVE - forest plots ------------------------------------------------

# Extract the subgroup levels
subgroups <- unique(rec_broad$control_group_type_collapsed)
# Loop through each subgroup and create a forest plot
for (subgroup in subgroups) {
  # Subset the data for the current subgroup
  subgroup_data <- subset(rec_broad, control_group_type_collapsed == subgroup)
  
  # Check if there is enough data for a meta-analysis in this subgroup
  if (nrow(subgroup_data) > 1) {
    # Create a meta-analysis object for the current subgroup
    m.subgroup <- metagen(TE = subgroup_data$SMD, 
                          seTE = subgroup_data$SE_SMD, 
                          studlab = subgroup_data$study_ID,
                          data = subgroup_data,
                          sm = "SMD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = paste("Forest plot for subgroup:", subgroup))
    
    # Save each forest plot as a PDF
    pdf(paste0("plot_rec_", subgroup, ".pdf"), width = 10, height = 8)
    
    # Create a forest plot for the current subgroup
    forest(m.subgroup, 
           comb.fixed = FALSE, # Do not combine subgroups
           comb.random = TRUE, # Combine using random effects model
           print.subgroup.labels = TRUE, # Print subgroup labels
           print.subgroup.names = TRUE, # Print names of subgroups
           col.subgroup = "blue", # Color for subgroup summary lines
           col.study = "black", # Color for individual study lines
           col.diamond = "red", # Color for overall summary diamond
           col.diamond.lines = "blue", # Color for the lines of the diamond
           layout = "RevMan5", # Layout style
           digits = 2) # Number of digits to display
    
    # Close the PDF device
    dev.off()
  } else {
    cat("Not enough data for subgroup:", subgroup, "\n")
  }
}


# RECEPTIVE - subgroup ----------------------------------------------------
m.rec.broad.subgroup.collapsed <- update(m.rec.broad,
                               subgroup = control_group_type_collapsed,
                               tau.common = FALSE)
m.rec.broad.subgroup <- update(m.rec.broad,
                                         subgroup = control_group_type,
                                         tau.common = FALSE)


m.rec.broad.subgroup.clean <- update(m.rec.broad.clean,
                                     subgroup = control_group_type,
                                     tau.common = FALSE)
m.rec.broad.subgroup.clean.collapsed <- update(m.rec.broad.clean,
                                     subgroup = control_group_type_collapsed,
                                     tau.common = FALSE)

summary(m.rec.broad.subgroup.clean)
summary(m.rec.broad.subgroup.clean.collapsed)
# Receptive bubble plots --------------------------------------------------
## BUBBLE PLOT

# Step 2: Filter the dataframe to exclude outliers and keep only aphasia severity data
filtered_data_rec <- smd_results %>%
  filter(include_broad == "Y" & 
           outcome_designation_broad == "receptive language" & 
           !(study_ID %in% exclude_studies_rec))  # Exclude outliers

# Step 3: Aggregate data to count the number of studies per control group type and sum total participants
agg_data_collapsed_rec <- filtered_data_rec %>%
  group_by(control_group_type_collapsed) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

agg_data_specific_rec <- filtered_data_rec %>%
  group_by(control_group_type) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

# COLLAPSED PLOT 
ggplot(agg_data_collapsed_rec, aes(x = num_studies, y = control_group_type_collapsed, color = control_group_type_collapsed)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Collapsed) vs. Study Count (Receptive Language)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend

# SPECIFIC PLOT 
ggplot(agg_data_specific_rec, aes(x = num_studies, y = control_group_type, color = control_group_type)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type vs. Study Count (Receptive Language)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend


# DOI Plots - Receptive ---------------------------------------------------

# OVERALL 
doiplot(m.rec.broad.clean)
title("DOI Plot for Receptive Language")

# Subset the data for the "active comparator" group in m.rec.broad.clean
subgroup_data_active_comparator <- update(m.rec.broad, subset = (control_group_type == "active comparator"))

# Generate DOI plot for "active comparator"
doiplot(subgroup_data_active_comparator)
title("DOI Plot for Active Comparator")

# Subset the data for the "usual care" group in m.rec.broad.clean
subgroup_data_usual_care <- update(m.rec.broad.clean, subset = (control_group_type_collapsed == "usual care"))

# Generate DOI plot for "usual care"
doiplot(subgroup_data_usual_care)
title("DOI Plot for Usual Care")



# FUNCTIONAL COMMUNICATION ------------------------------------------------

func_broad <- outcomes_broad %>% filter(outcome_designation_broad == "functional communication")


m.func.broad <- metagen(TE =SMD,
                     seTE = SE_SMD,
                     studlab = study_ID,
                     data = func_broad,
                     sm = "SMD",
                     fixed = FALSE,
                     random = TRUE,
                     method.tau = "REML",
                     method.random.ci = "HK",
                     title = "Functional Communication - broad")

# FUNCTIONAL COMMUNICATION - heterogeneity --------------------------------
m.func.broad.outliers <- find.outliers(m.func.broad)
m.func.broad.inf <- InfluenceAnalysis(m.func.broad, random = TRUE)

# Visualize
plot(m.func.broad.inf, "baujat")
plot(m.func.broad.inf, "influence")
plot(m.func.broad.inf, "es")

# Re-run with removals
exclude_studies_func <- c("Wei et al. 2021")

# Filter the dataset
func_broad_clean <- func_broad %>% filter(!(study_ID %in% exclude_studies_func))

# Re-run the meta-analysis with the cleaned dataset
m.func.broad.clean <- metagen(TE = SMD,
                           seTE = SE_SMD,
                           studlab = study_ID,
                           data = func_broad_clean,
                           sm = "SMD",
                           fixed = FALSE,
                           random = TRUE,
                           method.tau = "REML",
                           method.random.ci = "HK",
                           title = "Functional Communication - broad (Cleaned)")


# FUNCTIONAL - forest plots -----------------------------------------------


# Extract the subgroup levels
# Extract the subgroup levels
subgroups <- unique(func_broad_clean$control_group_type_collapsed)

# Loop through each subgroup and create a forest plot
for (subgroup in subgroups) {
  # Subset the data for the current subgroup
  subgroup_data <- subset(func_broad_clean, control_group_type_collapsed == subgroup)
  
  # Check if there is enough data for a meta-analysis in this subgroup
  if (nrow(subgroup_data) > 1) {
    # Create a meta-analysis object for the current subgroup
    m.subgroup <- metagen(TE = subgroup_data$SMD, 
                          seTE = subgroup_data$SE_SMD, 
                          studlab = subgroup_data$study_ID,
                          data = subgroup_data,
                          sm = "SMD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = paste("Forest plot for subgroup:", subgroup))
    
    # Save each forest plot as a PDF
    pdf(paste0("plot_func_", subgroup, ".pdf"), width = 10, height = 8)
    
    # Create a forest plot for the current subgroup
    forest(m.subgroup, 
           comb.fixed = FALSE, # Do not combine subgroups
           comb.random = TRUE, # Combine using random effects model
           print.subgroup.labels = TRUE, # Print subgroup labels
           print.subgroup.names = TRUE, # Print names of subgroups
           col.subgroup = "blue", # Color for subgroup summary lines
           col.study = "black", # Color for individual study lines
           col.diamond = "red", # Color for overall summary diamond
           col.diamond.lines = "blue", # Color for the lines of the diamond
           layout = "RevMan5", # Layout style
           digits = 2) # Number of digits to display
    
    # Close the PDF device
    dev.off()
  } else {
    cat("Not enough data for subgroup:", subgroup, "\n")
  }
}


# FUNCTIONAL - subgroup ---------------------------------------------------

m.func.broad.subgroup.collapsed <- update(m.func.broad, 
                             subgroup = control_group_type_collapsed, 
                             tau.common = FALSE)
m.func.broad.subgroup <- update(m.func.broad, 
                                          subgroup = control_group_type, 
                                          tau.common = FALSE)
m.func.broad.subgroup.clean <- update(m.func.broad.clean, 
                                   subgroup = control_group_type, 
                                   tau.common = FALSE)
m.func.broad.subgroup.clean.collapsed <- update(m.func.broad.clean, 
                                      subgroup = control_group_type_collapsed, 
                                      tau.common = FALSE)


summary(m.func.broad.subgroup)
summary(m.func.broad.subgroup.collapsed)
summary(m.func.broad.subgroup.clean)
summary(m.func.broad.subgroup.clean.collapsed)
# FC bubble plots --------------------------------------------------
## BUBBLE PLOT

# Step 2: Filter the dataframe to exclude outliers and keep only aphasia severity data
filtered_data_func <- smd_results %>%
  filter(include_broad == "Y" & 
           outcome_designation_broad == "functional communication" & 
           !(study_ID %in% exclude_studies_func))  # Exclude outliers

# Step 3: Aggregate data to count the number of studies per control group type and sum total participants
agg_data_collapsed_func <- filtered_data_func %>%
  group_by(control_group_type_collapsed) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

agg_data_specific_func <- filtered_data_func %>%
  group_by(control_group_type) %>%
  summarize(num_studies = n(),            # Count the number of studies
            total_participants = sum(total_n)) %>%  # Sum total participants
  ungroup()

# COLLAPSED PLOT 
ggplot(agg_data_collapsed_func, aes(x = num_studies, y = control_group_type_collapsed, color = control_group_type_collapsed)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Collapsed) vs. Study Count (Functional Communication)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend

# SPECIFIC PLOT 
ggplot(agg_data_specific_func, aes(x = num_studies, y = control_group_type, color = control_group_type)) +
  geom_point(aes(size = total_participants), alpha = 0.7) +  # Directly map size to participants
  geom_text(aes(label = total_participants), vjust = -0.5, hjust = 0.5) +  # Overlay the correct number of participants
  scale_size(range = c(3, 20), guide = FALSE) +  # Use a fixed size range
  labs(title = "Control Group Type (Specific) vs. Study Count (Functional communication)",
       x = "Number of Studies",
       y = "Control Group Type") +  # No need for size label since legend is removed
  theme_minimal() +  # Use a clean theme for the plot
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        legend.position = "none")  # Remove the legend

# DOI Plots - functional communication ------------------------------------

# OVERALL 
doiplot(m.func.broad.clean)
title("DOI Plot for Functional Communication")

# Subset the data for the "active comparator" group in m.func.broad.clean
subgroup_data_active_comparator <- update(m.func.broad.clean, subset = (control_group_type_collapsed == "active comparator"))

# Generate DOI plot for "active comparator"
doiplot(subgroup_data_active_comparator)
title("DOI Plot for Active Comparator")

# Subset the data for the "usual care" group in m.func.broad.clean
subgroup_data_usual_care <- update(m.func.broad.clean, subset = (control_group_type_collapsed == "usual care"))

# Generate DOI plot for "usual care"
doiplot(subgroup_data_usual_care)
title("DOI Plot for Usual Care")









# PSYCHOSOCIAL IMPACT -----------------------------------------------------
psych_broad <- outcomes_broad %>% filter(outcome_designation_broad == "psychosocial impact")


m.psych.broad <- metagen(TE =SMD,
                        seTE = SE_SMD,
                        studlab = study_ID,
                        data = func_broad,
                        sm = "SMD",
                        fixed = FALSE,
                        random = TRUE,
                        method.tau = "REML",
                        method.random.ci = "HK",
                        title = "Psychosocial Impact - broad")

# FUNCTIONAL COMMUNICATION - heterogeneity --------------------------------
m.psych.broad.outliers <- find.outliers(m.psych.broad)
m.psych.broad.inf <- InfluenceAnalysis(m.psych.broad, random = TRUE)

# Visualize
plot(m.psych.broad.inf, "baujat")
plot(m.psych.broad.inf, "influence")
plot(m.psych.broad.inf, "es")

# Re-run with removals
exclude_studies_psych <- c("van der Meulen et al. 2014")

# Filter the dataset
psych_broad_clean <- psych_broad %>% filter(!(study_ID %in% exclude_studies_psych))

# Re-run the meta-analysis with the cleaned dataset
m.psych.broad.clean <- metagen(TE = SMD,
                              seTE = SE_SMD,
                              studlab = study_ID,
                              data = func_broad_clean,
                              sm = "SMD",
                              fixed = FALSE,
                              random = TRUE,
                              method.tau = "REML",
                              method.random.ci = "HK",
                              title = "Psychosocial Impact - broad (Cleaned)")


# PSYCHOSOCIAL IMPACT - forest plots -----------------------------------------------


# Extract the subgroup levels
# Extract the subgroup levels
subgroups <- unique(func_broad_clean$control_group_type_collapsed)

# Loop through each subgroup and create a forest plot
for (subgroup in subgroups) {
  # Subset the data for the current subgroup
  subgroup_data <- subset(func_broad_clean, control_group_type_collapsed == subgroup)
  
  # Check if there is enough data for a meta-analysis in this subgroup
  if (nrow(subgroup_data) > 1) {
    # Create a meta-analysis object for the current subgroup
    m.subgroup <- metagen(TE = subgroup_data$SMD, 
                          seTE = subgroup_data$SE_SMD, 
                          studlab = subgroup_data$study_ID,
                          data = subgroup_data,
                          sm = "SMD",
                          fixed = FALSE,
                          random = TRUE,
                          method.tau = "REML",
                          method.random.ci = "HK",
                          title = paste("Forest plot for subgroup:", subgroup))
    
    # Save each forest plot as a PDF
    pdf(paste0("plot_func_", subgroup, ".pdf"), width = 10, height = 8)
    
    # Create a forest plot for the current subgroup
    forest(m.subgroup, 
           comb.fixed = FALSE, # Do not combine subgroups
           comb.random = TRUE, # Combine using random effects model
           print.subgroup.labels = TRUE, # Print subgroup labels
           print.subgroup.names = TRUE, # Print names of subgroups
           col.subgroup = "blue", # Color for subgroup summary lines
           col.study = "black", # Color for individual study lines
           col.diamond = "red", # Color for overall summary diamond
           col.diamond.lines = "blue", # Color for the lines of the diamond
           layout = "RevMan5", # Layout style
           digits = 2) # Number of digits to display
    
    # Close the PDF device
    dev.off()
  } else {
    cat("Not enough data for subgroup:", subgroup, "\n")
  }
}


# PSYCHOSOCIAL IMPACT - subgroup ---------------------------------------------------

m.psych.broad.subgroup.collapsed <- update(m.psych.broad, 
                                          subgroup = control_group_type_collapsed, 
                                          tau.common = FALSE)
m.psych.broad.subgroup <- update(m.psych.broad, 
                                subgroup = control_group_type, 
                                tau.common = FALSE)
m.psych.broad.subgroup.clean <- update(m.psych.broad.clean, 
                                      subgroup = control_group_type, 
                                      tau.common = FALSE)
m.psych.broad.subgroup.clean.collapsed <- update(m.psych.broad.clean, 
                                                subgroup = control_group_type_collapsed, 
                                                tau.common = FALSE)


summary(m.psych.broad.subgroup)
summary(m.psych.broad.subgroup.collapsed)
summary(m.psych.broad.subgroup.clean)
summary(m.psych.broad.subgroup.clean.collapsed)



# RVE ---------------------------------------------------------------------
# Load necessary libraries
library(metafor)
library(clubSandwich)
library(dplyr)  # For data filtering

# 📌 Step 1: Filter Out Unwanted Outcome Domains
outcomes_filtered <- outcomes_broad %>%
  filter(!(outcome_designation_broad %in% c("psychosocial impact", "intelligibility")))

# 📌 Step 2: Apply Sum Coding to Categorical Variables
outcomes_filtered$outcome_designation_broad <- factor(outcomes_filtered$outcome_designation_broad)
outcomes_filtered$control_group_type_collapsed <- factor(outcomes_filtered$control_group_type_collapsed)

# Sum coding (ensures no reference category)
contrasts(outcomes_filtered$outcome_designation_broad) <- contr.sum(length(levels(outcomes_filtered$outcome_designation_broad)))
contrasts(outcomes_filtered$control_group_type_collapsed) <- contr.sum(length(levels(outcomes_filtered$control_group_type_collapsed)))

# 📌 Step 3: Run RVE Meta-Regression
rve_model <- robu(SMD ~ outcome_designation_broad + control_group_type_collapsed, 
                  data = outcomes_filtered, 
                  studynum = study_ID, 
                  var.eff.size = var_SMD,  # Using existing variance column
                  modelweights = "CORR",
                  small = TRUE)

# 📌 Step 4: View Results
summary(rve_model)

# BAYESIAN
install.packages("remotes")
remotes::install_github("stan-dev/rstan", subdir = "rstan/rstan")

# Ensure outcome variable is a factor with explicit levels
outcomes_filtered$outcome_designation_broad <- factor(
  outcomes_filtered$outcome_designation_broad,
  levels = c("aphasia severity", 
             "expressive language", 
             "functional communication", 
             "receptive language")
)

# Ensure control group variable is a factor with explicit levels
outcomes_filtered$control_group_type_collapsed <- factor(
  outcomes_filtered$control_group_type_collapsed,
  levels = c("active comparator", 
             "activity control", 
             "no treatment", 
             "social support", 
             "usual care", 
             "waitlist")
)

# Save original labels for reference
outcome_labels <- levels(outcomes_filtered$outcome_designation_broad)
control_labels <- levels(outcomes_filtered$control_group_type_collapsed)

# Apply sum coding but RETAIN labels
contrasts(outcomes_filtered$outcome_designation_broad) <- contr.sum(length(outcome_labels))
colnames(contrasts(outcomes_filtered$outcome_designation_broad)) <- outcome_labels[-length(outcome_labels)] # Retains names

contrasts(outcomes_filtered$control_group_type_collapsed) <- contr.sum(length(control_labels))
colnames(contrasts(outcomes_filtered$control_group_type_collapsed)) <- control_labels[-length(control_labels)] # Retains names


# Bayesian Meta-Regression Model
bayesian_model <- brm(
  SMD ~ outcome_designation_broad + control_group_type_collapsed + (1 | study_ID),
  data = outcomes_filtered,
  family = gaussian(),
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(student_t(3, 0, 2), class = "sd")
  ),
  iter = 4000, warmup = 1000, chains = 4, cores = 4,
  backend = "cmdstanr"  # Use cmdstanr instead of rstan
)



# Extra Visuals -----------------------------------------------------------

# Load libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(RColorBrewer)

# Load dataset
control_viz <- read.csv("cleaned_controls_new.csv")

# Total number of unique studies
total_studies <- n_distinct(control_viz$study_ID)

# Calculate total number of trials per parent group
totals <- control_counts %>%
  group_by(parent_group) %>%
  summarise(total_studies = sum(study_count))


# Define parent groups
control_viz <- control_viz %>%
  mutate(parent_group = case_when(
    grepl("Usual care", control_group_type, ignore.case = TRUE) ~ "Usual Care",
    grepl("Active comparator", control_group_type, ignore.case = TRUE) ~ "Active Comparator",
    TRUE ~ control_group_type
  ))

# Count unique studies per control group type within parent group
control_counts <- control_viz %>%
  group_by(parent_group, control_group_type) %>%
  summarise(study_count = n_distinct(study_ID), .groups = "drop") %>%
  mutate(percentage = (study_count / total_studies) * 100)

# Get total size per control_group_type to define stacking order (largest to smallest)
stack_order <- control_counts %>%
  group_by(control_group_type) %>%
  summarise(total_count = sum(study_count)) %>%
  arrange(desc(total_count)) %>%
  pull(control_group_type)

# Reorder control_group_type within each parent group by study_count
# Smallest first (so it appears on the bottom of the stack)
control_counts <- control_counts %>%
  group_by(parent_group) %>%
  arrange(study_count, .by_group = TRUE) %>%
  mutate(control_group_type = forcats::fct_inorder(control_group_type)) %>%
  ungroup()


# Sort parent groups by total percentage
parent_order <- control_counts %>%
  group_by(parent_group) %>%
  summarise(total_percentage = sum(percentage)) %>%
  arrange(desc(total_percentage)) %>%
  pull(parent_group)

# Apply parent group order
control_counts$parent_group <- factor(control_counts$parent_group, levels = parent_order)

# Define subgroup colors
custom_colors <- c(
  "Active comparator - delivery model" = "#5fa95f",  # Darkened/Saturated Deepest Blue
  "Active comparator - regimen" = "#b0d66b",         # Darkened/Saturated Medium Blue
  "Active comparator - theoretical" = "#517251",     # Darkened/Saturated Lightest Blue
  "Activity control" = "#3ca8d1",                    # Darkened/Saturated Muted Coral
  "No treatment" = "#4557a7",                        # Darkened/Saturated Pastel Green
  "Social support" = "#c22b2b",                      # Darkened/Saturated Dusty Rose
  "Usual care - protocolized" = "#c45a2e",           # Darkened/Saturated Sky Blue
  "Usual care - unrestricted" = "#ed6d3e",           # Darkened/Saturated Muted Blue
  "Waitlist" = "#fec841"                             # Darkened/Saturated Sandy Yellow
)
# Define only the subgroups to appear in the legend
subgroup_legend <- c(
  "Active comparator - regimen",
  "Active comparator - delivery model",
  "Active comparator - theoretical",
  "Usual care - unrestricted",
  "Usual care - protocolized"
)


subgroup_labels <- c(
  "Usual care - unrestricted" = "Unrestricted",
  "Usual care - protocolized" = "Protocolized",
  "Active comparator - regimen" = "Regimen",
  "Active comparator - delivery model" = "Delivery Model",
  "Active comparator - theoretical" = "Theoretical"
)


ggplot(control_counts, aes(x = parent_group, y = study_count, fill = control_group_type)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(
    aes(label = paste0(round(percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 6,
    color = "black",
    fontface = "bold"
  ) +

  scale_fill_manual(
    values = custom_colors,
    breaks = subgroup_legend,
    labels = subgroup_labels,
    name = "Subgroups",
    na.translate = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  labs(
    title = "Distribution of Control Types in Included Trials",
    x = "Control Group Type",
    y = "Number of Trials"
  ) +
  guides(fill = guide_legend(override.aes = list(size = 5)))


# EXTRA VISUALS - tree map of measures ------------------------------------

# Load required libraries
library(ggplot2)
library(treemapify)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(colorspace)


# Load the dataset
included_measures <- read.csv("outcome summary.csv")

# Ensure 'domain' and 'subdomain' are factors
included_measures$domain <- factor(included_measures$domain)
included_measures$subdomain <- factor(included_measures$subdomain, exclude = NULL)  # Keep NA as a level

# Sort dataset by domain, subdomain, and k (size)
included_measures <- included_measures %>%
  arrange(domain, subdomain, desc(k))

# Create a combined "domain-subdomain" factor for grouping (this part can stay if needed)
included_measures$subdomain_group <- ifelse(is.na(included_measures$subdomain), 
                                            as.character(included_measures$domain), 
                                            paste(included_measures$domain, included_measures$subdomain, sep = " - "))

# Calculate proportions within each domain
included_measures <- included_measures %>%
  group_by(domain) %>%
  mutate(proportion = k / sum(k) * 100,  # Compute percentage by domain
         label_text = paste0(acronym, "\n", round(proportion, 1), "%"))  # Create label


# Generate a base color per domain
domain_colors <- brewer.pal(length(unique(included_measures$domain)), "Set2")
names(domain_colors) <- unique(included_measures$domain)




# Function to generate distinct subdomain colors
# Manually assigned colors for Expressive Language subdomains
expressive_colors_named <- setNames(
  c("#D67236", "#FF6400","#F98400", "#F15a60", "#FFD454", "#FFF2BD"),
  c("Discourse", "Naming", "Repetition", "Spontaneous Speech", "Writing", "Other") # Ensure correct mapping
)

# Function to generate subdomain colors for all other domains
subdomain_colors <- setNames(
  unlist(lapply(unique(included_measures$domain), function(dom) {
    subdomains <- unique(included_measures$subdomain_group[included_measures$domain == dom])
    
    if (dom == "Expressive Language") {
      return(expressive_colors_named)  # Use manually assigned colors
    } else if (length(subdomains) == 1) {
      return(setNames(domain_colors[dom], subdomains))  # Use domain color if only one subdomain
    } else {
      # Start with a DARKER version of the domain color
      darker_base <- darken(domain_colors[dom], 0.3)  
      
      # Convert to HCL for precise control
      base_hcl <- as(hex2RGB(darker_base), "polarLUV")
      
      # Extract HCL values
      base_hue <- coords(base_hcl)[, "H"]
      base_chroma <- coords(base_hcl)[, "C"]
      base_luminance <- coords(base_hcl)[, "L"]
      
      # Generate variation for non-Expressive Language subdomains
      h_values <- seq(base_hue, base_hue + 50, length.out = length(subdomains))
      c_values <- seq(base_chroma, base_chroma + 10, length.out = length(subdomains))
      l_values <- seq(base_luminance, min(base_luminance + 30, 90), length.out = length(subdomains))
      
      # Convert back to HEX
      shades <- hex(polarLUV(L = l_values, C = c_values, H = h_values))
      
      # Fallback if colors fail
      if (any(is.na(shades))) {
        shades <- lighten(darker_base, seq(0.2, 0.6, length.out = length(subdomains)))
      }
      
      return(setNames(shades, subdomains))
    }
  })),
  unique(included_measures$subdomain_group)
)



# Function to create treemap with clustered subdomains
create_treemap <- function(data) {
  ggplot(data, aes(area = k, fill = subdomain_group, label = label_text)) +
    geom_treemap(color = "white") +  
    geom_treemap_text(min.size = 2, max.size = 6, place = "centre", grow = FALSE, reflow = TRUE) +  
    scale_fill_manual(values = subdomain_colors) +  
    theme_minimal() +
    theme(legend.position = "none",  
          plot.title = element_text(hjust = 0.5, size = 16)) +
    ggtitle(unique(data$domain))  
}

# Split dataset by domain and generate treemaps
plots <- included_measures %>%
  split(.$domain) %>%
  lapply(create_treemap)

# Arrange all treemaps into a grid
final_plot <- ggarrange(plotlist = plots, 
                        ncol = 2, nrow = ceiling(length(plots)/2))  

# Add title
final_plot <- final_plot + 
  plot_annotation(title = "Outcome Measures for Included Trials") & 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 8)))

# Display the final plot
print(final_plot)



# CUSTOM LEGEND
# Define manual legend entries
legend_labels <- c("Discourse", "Naming", "Repetition", "Spontaneous Speech", "Writing", "Other",
                   "Reading", "Auditory Comprehension")

legend_colors <- c("#D67236", "#FF6400", "#F98400", "#Ff8c75", "#FFD454", "#FFF2BD",
                   "#ffaadd", "#da61ae")

# Create a dataframe for plotting the legend
legend_data <- data.frame(
  subdomain = factor(legend_labels, levels = legend_labels),
  color = legend_colors
)

# Generate the manual legend with a thinner border and more padding inside
legend_plot <- ggplot(legend_data, aes(x = 1, y = subdomain, fill = subdomain)) +
  geom_tile() +  # Creates colored boxes
  scale_fill_manual(values = setNames(legend_colors, legend_labels)) +  # Apply colors
  guides(fill = guide_legend(title = "Subdomains")) +  # Legend title
  theme_void() +  # Remove axes
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black", fill = "white", size = 0.3),  # Keeps box border thin
    legend.box.margin = margin(10, 10, 10, 10),  # Keeps internal padding the same
    plot.margin = margin(10, 10, 80, 10)  # Increase third value (bottom margin)
  )






# SEPARATE - combine in photoshop



# Ensure 'domain' and 'subdomain' are factors
included_measures$domain <- factor(included_measures$domain)
included_measures$subdomain <- factor(included_measures$subdomain, exclude = NULL)  # Keep NA as a level

# Sort dataset by domain, subdomain, and k (size)
included_measures <- included_measures %>%
  arrange(domain, subdomain, desc(k))

# Create a combined "domain-subdomain" factor for better grouping
included_measures$subdomain_group <- ifelse(is.na(included_measures$subdomain), 
                                            as.character(included_measures$domain), 
                                            paste(included_measures$domain, included_measures$subdomain, sep = " - "))

# Calculate proportions within each subdomain
included_measures <- included_measures %>%
  group_by(subdomain_group) %>%
  mutate(proportion = k / sum(k) * 100,  
         label_text = paste0(acronym, "\n", round(proportion, 1), "%"))  

# Generate a base color per domain
domain_colors <- brewer.pal(length(unique(included_measures$domain)), "Set2")
names(domain_colors) <- unique(included_measures$domain)

# Generate subdomain color shades based on domain color
subdomain_colors <- setNames(
  unlist(lapply(unique(included_measures$domain), function(dom) {
    subdomains <- unique(included_measures$subdomain_group[included_measures$domain == dom])
    if (length(subdomains) == 1) {
      return(setNames(domain_colors[dom], subdomains))
    } else {
      shades <- colorRampPalette(c(domain_colors[dom], "white"))(length(subdomains) + 1)[-1]  # Avoid white
      return(setNames(shades, subdomains))
    }
  })),
  unique(included_measures$subdomain_group)
)

# Function to create and export a treemap for each subdomain
export_treemap <- function(data, subdomain_name) {
  p <- ggplot(data, aes(area = k, fill = subdomain_group, label = label_text)) +
    geom_treemap(color = "white") +  
    geom_treemap_text(min.size = 2, max.size = 6, place = "centre", grow = FALSE, reflow = TRUE) +  
    scale_fill_manual(values = subdomain_colors) +  
    theme_minimal() +
    theme(legend.position = "none")  # No title, no extra labels
  
  # Save plot as a separate PNG file
  ggsave(filename = paste0("treemap_", gsub(" ", "_", subdomain_name), ".png"), 
         plot = p, width = 8, height = 6, units = "in", dpi = 300)
}

# **Export treemaps per subdomain**
included_measures %>%
  split(.$subdomain_group) %>%
  purrr::imap(~ export_treemap(.x, .y))  # Loops through each subdomain and saves a file




# EXTRA VISUALS - Subgroup analyses ---------------------------------------


# SUBGROUP RESULTS COMPILED


visual_data <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/corrected_data_visual.csv", stringsAsFactors = TRUE)
visual_data_collapsed <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/corrected_data_visual_collapsed.csv", stringsAsFactors =  TRUE)
visual_data$Control.Group <- as.character(visual_data$Control.Group)
visual_data_collapsed$Control.Group <- as.character(visual_data_collapsed$Control.Group)

shapes <- c(16, 17, 18, 15, 3, 7, 8, 4, 5)
# Define a custom color palette with vibrant, darker colors
custom_colors <- c(
  "Delivery model" = "#5fa95f",  # Darkened/Saturated Deepest Blue
  "Regimen" = "#b0d66b",         # Darkened/Saturated Medium Blue
  "Theoretical" = "#517251",     # Darkened/Saturated Lightest Blue
  "Activity control" = "#3ca8d1",                    # Darkened/Saturated Muted Coral
  "No treatment" = "#4557a7",                        # Darkened/Saturated Pastel Green
  "Social support" = "#c22b2b",                      # Darkened/Saturated Dusty Rose
  "Protocolized" = "#c45a2e",           # Darkened/Saturated Sky Blue
  "Unrestricted" = "#ed6d3e",           # Darkened/Saturated Muted Blue
  "Waitlist" = "#fec841"                             # Darkened/Saturated Sandy Yellow
)



# Define shapes for the original control groups
shapes <- c(
  "Delivery model" = 16,  # Circle
  "Regimen" = 17,         # Triangle
  "Theoretical" = 18,     # Diamond
  "Activity control" = 15,                    # Square
  "No treatment" = 3,                         # Plus
  "Social support" = 7,                       # X
  "Protocolized" = 8,            # Star
  "Unrestricted" = 4,            # Cross
  "Waitlist" = 5                              # Pentagon
)

plot2 <- ggplot(visual_data, aes(x = SMD, y = reorder(`Control.Group`, SMD), 
                                 color = `Control.Group`, shape = `Control.Group`)) +
  geom_point(size = 4) +  
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  
  facet_wrap(~Outcome, scales = "free_y") +  
  scale_shape_manual(
    values = shapes,
    breaks = c("Delivery model", "Regimen", "Theoretical",  # Active Comparator group
               "Protocolized", "Unrestricted",  # Usual Care group
               "Activity control", "No treatment", "Waitlist", "Social support"),  # Others
    labels = c("Active Comparator (Delivery Model)", 
               "Active Comparator (Regimen)", 
               "Active Comparator (Theoretical)", 
               "Usual Care (Protocolized)", 
               "Usual Care (Unrestricted)", 
               "Activity Control", 
               "No Treatment", 
               "Waitlist", 
               "Social Support")  # Custom names for clarity
  ) +
  scale_color_manual(
    values = custom_colors,
    breaks = c("Delivery model", "Regimen", "Theoretical",  
               "Protocolized", "Unrestricted",  
               "Activity control", "No treatment", "Waitlist", "Social support"),
    labels = c("Active Comparator (Delivery Model)", 
               "Active Comparator (Regimen)", 
               "Active Comparator (Theoretical)", 
               "Usual Care (Protocolized)", 
               "Usual Care (Unrestricted)", 
               "Activity Control", 
               "No Treatment", 
               "Waitlist", 
               "Social Support")  
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.2, .08))) +  
  scale_y_discrete(expand = expansion(mult = c(0.2, .1))) +  
  coord_cartesian(clip = "off") +
  theme_minimal() +  
  labs(
    title = "Effect Sizes by Outcome (Specific Groups)",
    x = "Standardized Mean Difference (SMD)",
    color = "Control Group Type",
    shape = "Control Group Type"
  ) +
  theme(
    legend.position = "bottom",  
    legend.box.background = element_rect(color = "black", fill = NA),  
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 12),  
    legend.title = element_text(size = 14, face = "bold"),  
    legend.key.size = unit(1, "lines"),  
    axis.title.y = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14),  
    axis.title.x = element_text(size = 16, face = "bold"),  
    strip.text = element_text(size = 16, face = "bold"),  
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    panel.spacing = unit(1.5, "lines"),  
    panel.background = element_rect(fill = "white", color = "black", size = 0.5)  
  ) +
  guides(
    color = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size = 5)),  # Wider legend
    shape = guide_legend(ncol = 3, byrow = TRUE, override.aes = list(size = 5))
  ) +
  geom_text(aes(label = `Control.Group`), vjust = 2, hjust = 0.5, size = 4, show.legend = FALSE) 

print(plot2)




### COLLAPSED PLOT ###

# Adjust the color mapping for collapsed data, ensuring categories match where applicable
color_mapping_collapsed <- c(
  "No treatment" = "#4557a7",        # Darkened/Saturated Pastel Green
  "Waitlist" = "#fec841",            # Darkened/Saturated Sandy Yellow
  "Social support" = "#c22b2b",      # Darkened/Saturated Dusty Rose
  "Activity control" = "#3ca8d1",    # Darkened/Saturated Muted Coral
  "Usual care" = "#ed6d3e",          # Darkened/Saturated Muted Blue
  "Active comparator" = "#517251"    # Darkened/Saturated Theoretical
)


# Map the shapes in the collapsed dataset
shapes_collapsed <- c(
  "Active comparator" = 16,  # Circle (same as "Active comparator - delivery model")
  "Activity control" = 15,   # Square (same as before)
  "No treatment" = 3,        # Plus (same as before)
  "Social support" = 7,      # X (same as before)
  "Usual care" = 4,          # Cross (new category, but similar shape)
  "Waitlist" = 5             # Pentagon (same as before)
)

plot1 <- ggplot(visual_data_collapsed, aes(x = SMD, y = 1,
                                           color = `Control.Group`, shape = `Control.Group`)) +
  geom_point(size = 6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
  facet_wrap(~Outcome, scales = "free_x") +
  scale_shape_manual(values = shapes_collapsed) +
  scale_color_manual(values = color_mapping_collapsed) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.08))) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  labs(
    title = "Effect Sizes by Outcome (Collapsed Groups)",
    x = "Standardized Mean Difference (SMD)",
    color = "Control Group Type",
    shape = "Control Group Type"
  ) +
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(color = "black", fill = NA),
    legend.box.margin = margin(5, 5, 5, 5),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.size = unit(1, "lines"),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5),
    plot.margin = margin(20, 10, 10, 10),
    panel.spacing = unit(1.5, "lines"),
    panel.background = element_rect(fill = "white", color = "black", size = 0.5)
  ) +
  geom_text(aes(label = `Control.Group`),
            vjust = -1.2,  # Adjust to move closer/farther
            hjust = 0.5,
            size = 4,
            show.legend = FALSE)


print(plot1)



# SUBGROUP VIS UPDATED ----------------------------------------------------

n_controls <- 6
visual_data_collapsed <- visual_data_collapsed %>%
  mutate(Outcome_Factor = factor(Outcome)) %>%
  group_by(Outcome_Factor) %>%
  arrange(desc(SMD), .by_group = TRUE) %>%
  mutate(
    x_within_group = row_number(),
    outcome_index = as.numeric(Outcome_Factor),
    x_plot = (outcome_index - 1) * (n_controls + 1) + x_within_group  # evenly spaced
  ) %>%
  ungroup()

n_outcomes <- length(levels(visual_data_collapsed$Outcome_Factor))

# X-axis tick locations centered under each group
label_positions <- tapply(visual_data_collapsed$x_plot, 
                          visual_data_collapsed$Outcome_Factor, 
                          function(x) mean(range(x)))

# Divider lines between groups (midpoint between adjacent group ranges)
group_ranges <- tapply(visual_data_collapsed$x_plot,
                       visual_data_collapsed$Outcome_Factor,
                       range)

upper_bounds <- sapply(group_ranges, function(x) max(x))
lower_bounds <- sapply(group_ranges, function(x) min(x))

divider_positions <- (head(upper_bounds, -1) + tail(lower_bounds, -1)) / 2

plot1 <- ggplot(visual_data_collapsed, aes(x = x_plot, y = SMD,
                                           color = Control.Group,
                                           shape = Control.Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  
  geom_vline(xintercept = divider_positions, linetype = "solid", color = "lightgray") +
  
  geom_point(size = 5) +
  
  geom_text(aes(label = Control.Group),
            vjust = 2,
            size = 3.8,
            show.legend = FALSE) +
  
  scale_color_manual(values = color_mapping_collapsed) +
  scale_shape_manual(values = shapes_collapsed) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  
  scale_x_continuous(
    breaks = label_positions,
    labels = levels(visual_data_collapsed$Outcome_Factor),
    expand = expansion(add = 1)
  ) +
  
  labs(
    title = "Effect Sizes by Outcome (Collapsed Groups)",
    x = NULL,
    y = "Standardized Mean Difference (SMD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black", size = 0.5)
  )

print(plot1)


# Use 9 control groups
n_controls <- 9

# Define shapes and custom colors
shapes <- c(16, 17, 18, 15, 3, 7, 8, 4, 5)
custom_colors <- c(
  "Delivery model" = "#5fa95f",
  "Regimen" = "#b0d66b",
  "Theoretical" = "#517251",
  "Activity control" = "#3ca8d1",
  "No treatment" = "#4557a7",
  "Social support" = "#c22b2b",
  "Protocolized" = "#c45a2e",
  "Unrestricted" = "#ed6d3e",
  "Waitlist" = "#fec841"
)

# Prepare the visual_data
visual_data <- visual_data %>%
  mutate(Outcome_Factor = factor(Outcome)) %>%
  group_by(Outcome_Factor) %>%
  arrange(desc(SMD), .by_group = TRUE) %>%
  mutate(
    x_within_group = row_number(),
    outcome_index = as.numeric(Outcome_Factor),
    x_plot = (outcome_index - 1) * (n_controls + 1) + x_within_group
  ) %>%
  ungroup()

n_outcomes <- length(levels(visual_data$Outcome_Factor))

# X-axis tick locations centered under each group
label_positions <- ((0:(n_outcomes - 1)) * (n_controls + 1)) + (n_controls + 1) / 2

# Divider lines between groups
divider_positions <- ((1:(n_outcomes - 1)) * (n_controls + 1)) + 0.5

# Build the plot
plot2 <- ggplot(visual_data, aes(x = x_plot, y = SMD,
                                 color = Control.Group,
                                 shape = Control.Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  
  geom_vline(xintercept = divider_positions, linetype = "solid", color = "lightgray") +
  
  geom_point(size = 5) +
  
  geom_text_repel(
    aes(label = Control.Group),
    direction = "y",
    nudge_y = -0.05,       # nudge labels downward
    box.padding = 0.2,     # space around labels
    point.padding = 0.3,   # space between label and point
    segment.color = NA,    # <- disables leader lines
    size = 3.8,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = setNames(shapes, names(custom_colors))) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  
  scale_x_continuous(
    breaks = label_positions,
    labels = levels(visual_data$Outcome_Factor),
    expand = expansion(add = 1)
  ) +
  
  labs(
    title = "Effect Sizes by Outcome (Specific Groups)",
    x = NULL,
    y = "Standardized Mean Difference (SMD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black", size = 0.5)
  )

print(plot2)


# Summary plot take 3 -----------------------------------------------------


# Define number of control groups (max possible, for spacing purposes)
n_controls <- 9

# Define shapes and custom colors
shapes <- c(16, 17, 18, 15, 3, 7, 8, 4, 5)
custom_colors <- c(
  "Delivery model" = "#5fa95f",
  "Regimen" = "#b0d66b",
  "Theoretical" = "#517251",
  "Activity control" = "#3ca8d1",
  "No treatment" = "#4557a7",
  "Social support" = "#c22b2b",
  "Protocolized" = "#c45a2e",
  "Unrestricted" = "#ed6d3e",
  "Waitlist" = "#fec841"
)

# Prepare the visual_data with outcome-based positioning
visual_data <- visual_data %>%
  mutate(Outcome_Factor = factor(Outcome)) %>%
  group_by(Outcome_Factor) %>%
  arrange(desc(SMD), .by_group = TRUE) %>%
  mutate(
    x_within_group = row_number(),
    outcome_index = as.numeric(Outcome_Factor),
    x_plot = (outcome_index - 1) * (n_controls + 1) + x_within_group
  ) %>%
  ungroup()

# X-axis tick locations centered under each group
label_positions <- tapply(visual_data$x_plot, 
                          visual_data$Outcome_Factor, 
                          function(x) mean(range(x)))

# Dynamically calculate divider positions between groups
group_ranges <- tapply(visual_data$x_plot, visual_data$Outcome_Factor, range)
lower_bounds <- sapply(group_ranges, `[`, 1)
upper_bounds <- sapply(group_ranges, `[`, 2)
divider_positions <- (head(upper_bounds, -1) + tail(lower_bounds, -1)) / 2

# Build the plot
plot2 <- ggplot(visual_data, aes(x = x_plot, y = SMD,
                                 color = Control.Group,
                                 shape = Control.Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  geom_vline(xintercept = divider_positions, linetype = "solid", color = "gray40", linewidth = 0.7) +
  
  geom_point(size = 6) +
  
  geom_text_repel(
    aes(label = Control.Group),
    direction = "y",
    nudge_y = -0.05,
    box.padding = 0.25,
    point.padding = 0.4,
    segment.color = NA,
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +
  
  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = setNames(shapes, names(custom_colors))) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  
  scale_x_continuous(
    breaks = label_positions,
    labels = levels(visual_data$Outcome_Factor),
    expand = expansion(add = 1)
  ) +
  
  labs(
    title = "Effect Sizes by Outcome (Specific Groups)",
    x = NULL,
    y = "Standardized Mean Difference (SMD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black", size = 0.8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Print the plot
print(plot2)


# Prepare data
n_controls <- 6
visual_data_collapsed <- visual_data_collapsed %>%
  mutate(Outcome_Factor = factor(Outcome)) %>%
  group_by(Outcome_Factor) %>%
  arrange(desc(SMD), .by_group = TRUE) %>%
  mutate(
    x_within_group = row_number(),
    outcome_index = as.numeric(Outcome_Factor),
    x_plot = (outcome_index - 1) * (n_controls + 1) + x_within_group
  ) %>%
  ungroup()

# X-axis tick locations and dividers
label_positions <- tapply(visual_data_collapsed$x_plot, 
                          visual_data_collapsed$Outcome_Factor, 
                          function(x) mean(range(x)))

group_ranges <- tapply(visual_data_collapsed$x_plot,
                       visual_data_collapsed$Outcome_Factor,
                       range)

upper_bounds <- sapply(group_ranges, max)
lower_bounds <- sapply(group_ranges, min)
divider_positions <- (head(upper_bounds, -1) + tail(lower_bounds, -1)) / 2

# Build the plot
plot1 <- ggplot(visual_data_collapsed, aes(x = x_plot, y = SMD,
                                           color = Control.Group,
                                           shape = Control.Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  geom_vline(xintercept = divider_positions, linetype = "solid", color = "gray40", linewidth = 0.7) +
  
  geom_point(size = 6) +
  
  geom_text(
    aes(label = Control.Group),
    vjust = 2.5,         # Moves label below the point (1 = just below; 1.5 = more padding)
    size = 3,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values = color_mapping_collapsed) +
  scale_shape_manual(values = shapes_collapsed) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  
  scale_x_continuous(
    breaks = label_positions,
    labels = levels(visual_data_collapsed$Outcome_Factor),
    expand = expansion(add = 1)
  ) +
  
  labs(
    title = "Effect Sizes by Outcome (Collapsed Groups)",
    x = NULL,
    y = "Standardized Mean Difference (SMD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "gray40", size = 0.8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Print it
print(plot1)


# Defense -----------------------------------------------------------------

# Redefine visual_data_no_func with fixed-width sections
# Define number of outcomes (after excluding Functional communication)
visual_data_no_func <- visual_data %>%
  filter(Outcome != "Functional communication")

n_controls <- 9
gap <- 2

visual_data_no_func <- visual_data %>%
  filter(Outcome != "Functional communication") %>%
  mutate(Outcome_Factor = factor(Outcome)) %>%
  mutate(outcome_index = as.numeric(Outcome_Factor)) %>%
  group_by(Outcome_Factor) %>%
  arrange(desc(SMD), .by_group = TRUE) %>%
  mutate(
    n_points = n(),  # number of points in this group
    offset = floor((n_controls - n_points) / 2),  # center alignment
    slot_in_block = row_number() + offset,
    x_plot = (outcome_index - 1) * (n_controls + gap) + slot_in_block
  ) %>%
  ungroup()




# Updated axis labels and dividers
label_positions <- ((0:(n_outcomes - 1)) * (n_controls + 1)) + (n_controls + 1) / 2


group_ranges <- tapply(visual_data_no_func$x_plot, visual_data_no_func$Outcome_Factor, range)
lower_bounds <- sapply(group_ranges, `[`, 1)
upper_bounds <- sapply(group_ranges, `[`, 2)
n_outcomes <- length(levels(visual_data_no_func$Outcome_Factor))

label_positions <- ((0:(n_outcomes - 1)) * (n_controls + gap)) + (n_controls / 2)
# For exact divider placement between blocks
block_starts <- ((0:(n_outcomes - 1)) * (n_controls + gap)) + 1
block_ends <- block_starts + n_controls - 1

# Divider = midpoint between one block's end and the next block's start
divider_positions <- (head(block_ends, -1) + tail(block_starts, -1)) / 2

# Plot (copying original formatting)
plot2_no_func <- ggplot(visual_data_no_func, aes(x = x_plot, y = SMD,
                                                 color = Control.Group,
                                                 shape = Control.Group)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  geom_vline(xintercept = divider_positions, linetype = "solid", color = "gray40", linewidth = 0.7) +
  
  geom_point(size = 6) +
  
  geom_text_repel(
    aes(label = Control.Group),
    direction = "y",
    nudge_y = -0.07,
    box.padding = 0.25,
    point.padding = 0.75,
    segment.color = NA,
    size = 3,
    show.legend = FALSE,
    max.overlaps = Inf
  ) +

  scale_color_manual(values = custom_colors) +
  scale_shape_manual(values = setNames(shapes, names(custom_colors))) +
  
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.25))) +
  
  scale_x_continuous(
    breaks = label_positions,
    labels = levels(visual_data_no_func$Outcome_Factor),
    expand = expansion(add = 1)
  ) +
  
  labs(
    title = "Effect Sizes by Outcome",
    x = NULL,
    y = "Standardized Mean Difference (SMD)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.y = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "black", size = 0.8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Print the plot
print(plot2_no_func)



# Risk of Bias ------------------------------------------------------------
# Select relevant columns
rob_data <- smd_results %>%
  select(study_ID, sequence_generation, allocation_concealment, outcome_blinding, 
         incomplete_outcome_data, selective_reporting, other_bias)

rob_data <- rob_data %>%
  mutate(across(-study_ID, ~ ifelse(. == "Unsure", "Unclear", .)))

# Map numeric codes to labels
rob_data_clean <- rob_data %>%
  mutate(across(-study_ID, ~ case_when(
    . == 1 ~ "Low",
    . == 2 ~ "High",
    . == 3 ~ "Unclear",
    TRUE ~ "Unclear"  # catch anything unexpected
  )))


# Select the relevant RoB columns
rob_data <- smd_results %>%
  select(study_ID, sequence_generation, allocation_concealment, outcome_blinding, 
         incomplete_outcome_data, selective_reporting, other_bias)

# Define severity ranking
severity_levels <- c("Low" = 1, "Unclear" = 2, "High" = 3)
reverse_severity_levels <- c("Low", "Unclear", "High")

# Step 1: Map judgments to severity numbers
rob_data_ranked <- rob_data_clean %>%
  mutate(across(-study_ID, ~ severity_levels[.]))

# Step 2: Collapse to worst judgment per study
rob_data_collapsed <- rob_data %>%
  group_by(study_ID) %>%
  summarise(across(everything(), max, na.rm = TRUE)) %>%
  ungroup()

# Step 3: Convert severity numbers back to labels
rob_data_collapsed <- rob_data_collapsed %>%
  mutate(across(-study_ID, ~ reverse_severity_levels[.]))

# Optional: Check the result
head(rob_data_collapsed)

# Rename for robvis
colnames(rob_data) <- c("Study", "Randomization", "Allocation concealment", 
                        "Blinding of outcome assessment", 
                        "Incomplete outcome data", 
                        "Selective reporting", "Other bias")

# Replace 'Unsure' with 'Unclear' for compatibility
rob_data <- rob_data %>%
  mutate(across(-study_ID, ~ ifelse(. == "Unsure", "Unclear", .)))


# Stoplight plot
stoplight_plot <- rob_traffic_light(data = rob_data, tool = "Generic")

# Summary plot
summary_plot <- rob_summary(data = rob_data, tool = "Generic")

# Export Stoplight Plot
ggsave("rob_stoplight_plot.png", stoplight_plot, width = 14, height = 10, dpi = 300)
ggsave("rob_stoplight_plot.pdf", stoplight_plot, width = 14, height = 10)

# Export Summary Plot
ggsave("rob_summary_plot.png", summary_plot, width = 10, height = 6, dpi = 300)
ggsave("rob_summary_plot.pdf", summary_plot, width = 10, height = 6)


library(ggplot2)
library(tidyr)
library(dplyr)

# Optional: take a subset if there are too many studies to display nicely
rob_heatmap_data <- read.csv("rob_data.csv")


# Pivot longer for ggplot
rob_heatmap_long <- rob_heatmap_data %>%
  pivot_longer(-study_ID, names_to = "Domain", values_to = "Judgment")

# Set the order of judgments
rob_heatmap_long$Judgment <- factor(rob_heatmap_long$Judgment, 
                                    levels = c("Low", "Unclear", "High"))

# Clean and order domain labels
rob_heatmap_long$Domain <- factor(rob_heatmap_long$Domain,
                                  levels = c(
                                    "sequence_generation", 
                                    "allocation_concealment", 
                                    "outcome_blinding", 
                                    "incomplete_outcome_data", 
                                    "selective_reporting",
                                    "other_bias"
                                  ),
                                  labels = c(
                                    "Sequence Generation", 
                                    "Allocation Concealment", 
                                    "Outcome Blinding", 
                                    "Incomplete Outcome Data", 
                                    "Selective Reporting",
                                    "Other Bias"
                                  )
)


# Plot heatmap
heatmap_plot <- ggplot(rob_heatmap_long, aes(x = Domain, y = study_ID, fill = Judgment)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Low" = "#98D688", 
                               "Unclear" = "#F4D166", 
                               "High" = "tomato")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase size
        axis.text.y = element_text(size = 8),  # Increase size
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5),  # Center the title
        axis.title.x = element_text(face = "bold")) +  # Make x-axis title bold
  labs(title = "Risk of Bias Heatmap",
       x = "Risk of Bias Domain",
       y = "Study")


ggsave("heatmap.png", heatmap_plot, width = 8, height = 10)  # Adjust height
ggsave("rob_heatmap.pdf", heatmap_plot, width = 12, height = 10)


# RoB Meta-regression -----------------------------------------------------


m.sev.broad.clean.reg <- metareg(
  m.sev.broad.clean, 
  ~ sequence_generation*control_group_type_collapsed + 
    allocation_concealment + 
    outcome_blinding + 
    incomplete_outcome_data + 
    other_bias
)


m.sev.broad.clean.reg

m.exp.broad.clean.reg <- metareg(
  m.exp.broad.clean, 
  ~ sequence_generation + 
    allocation_concealment + 
    outcome_blinding + 
    incomplete_outcome_data + 
    other_bias*control_group_type_collapsed
)

m.exp.broad.clean.reg

m.rec.broad.clean.reg <- metareg(
  m.rec.broad.clean, 
  ~ sequence_generation + 
    allocation_concealment + 
    outcome_blinding + 
    incomplete_outcome_data + 
    other_bias
)

m.rec.broad.clean.reg


m.func.broad.clean.reg <- metareg(
  m.func.broad.clean, 
  ~ sequence_generation + 
    allocation_concealment + 
    outcome_blinding + 
    incomplete_outcome_data + 
    other_bias
)

m.func.broad.clean.reg


# see all
m.sev.broad.clean.reg
m.exp.broad.clean.reg
m.rec.broad.clean.reg
m.func.broad.clean.reg

#TABLES

# Load your predictors CSV
meta_reg <- read.csv("meta_regression_predictors_no_interactions_complete.csv")


# Format p-values
format_p <- function(p) {
  ifelse(p < .001, "< .001", sprintf("%.3f", p))
}

# Build the table
meta_reg_table <- meta_reg %>%
  mutate(
    Estimate = sprintf("%.2f", Estimate),
    SE = sprintf("%.2f", SE),
    t = sprintf("%.2f", t),
    p_value = format_p(p_value),
    CI = paste0("[", sprintf("%.2f", CI_lower), "; ", sprintf("%.2f", CI_upper), "]")
  ) %>%
  select(Outcome, Predictor, Estimate, SE, t, df, p_value, CI) %>%
  flextable() %>%
  set_header_labels(
    Outcome = "Outcome",
    Predictor = "Predictor",
    Estimate = "Estimate",
    SE = "SE",
    t = "t",
    df = "df",
    p_value = "p-value",
    CI = "95% CI"
  ) %>%
  fontsize(size = 9, part = "all") %>%
  autofit() %>%
  align(align = "center", part = "all") %>%
  theme_booktabs() %>%
  border_outer(border = fp_border(color = "black", width = 1))

# Export the table to Word
doc <- read_docx()
doc <- body_add_par(doc, "Meta-Regression Results", style = "heading 1")
doc <- body_add_flextable(doc, meta_reg_table)
doc <- body_add_par(doc, "Note. SE = Standard Error; CI = Confidence Interval; p-values are reported to three decimal places, with p < .001 where applicable.", style = "Normal")

print(doc, target = "Meta_Regression_Table2.docx")

# Descriptive summary for paper -----------------------------------------------------

unique_study_ids <- outcomes_broad %>%
  summarize(unique_study_IDs = n_distinct(study_ID))

total_n_sum <- outcomes_broad %>%
  group_by(study_ID) %>%
  summarize(total_n = first(total_n), .groups = "drop") %>% 
  summarize(total_n_sum = sum(total_n, na.rm = TRUE)) %>%
  pull(total_n_sum)

control_group_counts <- outcomes_broad %>%
  group_by(control_group_type) %>%
  summarize(study_count = n_distinct(study_ID))

control_group_collapsed_counts <- outcomes_broad %>%
  group_by(control_group_type_collapsed) %>%
  summarize(study_count = n_distinct(study_ID))

plus_UC_count <- outcomes_broad %>%
  filter(plus_UC == "Y") %>%
  summarize(study_count = n_distinct(study_ID))

plus_arms_count <- outcomes_broad %>%
  filter(`X3_plus_arms` == "Y") %>%
  summarize(study_count = n_distinct(study_ID))

non_language_cointervention_count <- outcomes_broad %>%
  filter(non.language.cointervention == "Y") %>%
  summarize(study_count = n_distinct(study_ID))

outcome_designation_counts <- outcomes_broad %>%
  group_by(outcome_designation_broad) %>%
  summarize(study_count = n_distinct(study_ID))

summary_results <- list(
  total_n_sum = total_n_sum,
  control_group_counts = control_group_counts,
  control_group_collapsed_counts = control_group_collapsed_counts,
  plus_UC_count = plus_UC_count,
  plus_arms_count = plus_arms_count,
  non_language_cointervention_count = non_language_cointervention_count,
  outcome_designation_counts = outcome_designation_counts
)



# -------------------------------------------------------------------------

# DOI Plots
# Open high-resolution PNG device
png("doi_plots.png", width = 3000, height = 3000, res = 300)

# Set up 2x2 layout
par(mfrow = c(2, 2), mar = c(5, 4, 3, 2))

# Plot 1: Aphasia Severity
doiplot(m.sev.broad.clean)
title(main = "Aphasia Severity", line = 1)

# Plot 2: Expressive Language
doiplot(m.exp.broad.clean)
title(main = "Expressive Language", line = 1)

# Plot 3: Receptive Language
doiplot(m.rec.broad.clean)
title(main = "Receptive Language", line = 1)

# Plot 4: Functional Communication
doiplot(m.func.broad.clean)
title(main = "Functional Communication", line = 1)

# Close the device to save the file
dev.off()




# VISUALS - model tables --------------------------------------------------


# Models
m.sev.broad.clean.subgroup
m.exp.broad.clean.subgroup
m.rec.broad.subgroup.clean 
m.func.broad.subgroup.clean 

m.sev.broad.clean.subgroup.collapsed
m.exp.broad.clean.subgroup.collapsed
m.rec.broad.subgroup.clean.collapsed 
m.func.broad.subgroup.clean.collapsed 


subgroup_models <- read.csv("full_subgroup_results.csv")
overall_models <- read.csv("overall results.csv")



library(readxl)
library(dplyr)
library(flextable)
library(officer)

# List of outcome domains
domains <- unique(overall_models$Outcome)

# Start a new Word document
doc <- read_docx()

for (domain in domains) {
  
  # Filter overall model
  overall <- overall_models %>% filter(Outcome == domain)
  
  # Filter subgroups
  subgroups <- subgroup_models %>% filter(Outcome == domain)
  
  # Simplified overall model table (without the Outcome column)
  overall_section <- data.frame(
    SMD = round(overall$SMD, 3),
    CI = paste0("[", round(overall$CI_lower, 3), "; ", round(overall$CI_upper, 3), "]"),
    p_value = overall$p_value,
    tau2 = round(overall$tau2, 3),
    I2 = paste0(round(overall$I2, 1), "%"),
    Subgroup_Differences = paste0("Q = ", round(overall$Q_between, 2), ", p = ", overall$Q_between_p_value),
    stringsAsFactors = FALSE
  )
  
  overall_ft <- flextable(overall_section) %>%
    set_header_labels(
      SMD = "SMD",
      CI = "95% CI",
      p_value = "p-value",
      tau2 = "Tau²",
      I2 = "I²",
      Subgroup_Differences = "Subgroup Differences"
    ) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    theme_booktabs() %>%
    border_outer(part = "all", border = fp_border(color = "black", width = 1))
  
  # Subgroup results table
  subgroups_table <- subgroups %>%
    transmute(
      Subgroup,
      k,
      SMD = round(SMD, 3),
      CI = paste0("[", round(CI_lower, 3), "; ", round(CI_upper, 3), "]")
    )
  
  subgroups_ft <- flextable(subgroups_table) %>%
    set_header_labels(
      Subgroup = "Subgroup",
      k = "Studies (k)",
      SMD = "SMD",
      CI = "95% CI"
    ) %>%
    autofit() %>%
    fontsize(size = 10, part = "all") %>%
    bold(part = "header") %>%
    align(align = "center", part = "all") %>%
    theme_booktabs() %>%
    border_outer(part = "all", border = fp_border(color = "black", width = 1))
  
  # Add domain section to the document
  doc <- body_add_par(doc, paste("Results for", domain), style = "heading 1")
  doc <- body_add_par(doc, "Overall Model Results", style = "heading 2")
  doc <- body_add_flextable(doc, overall_ft)
  doc <- body_add_par(doc, "Subgroup Results", style = "heading 2")
  doc <- body_add_flextable(doc, subgroups_ft)
  doc <- body_add_par(doc, "", style = "Normal")  # Add spacing between sections
}

# Save the combined Word document
print(doc, target = "All_Outcome_Tables.docx")





# Build combined table
combined_table <- list()

for (domain in unique(overall_models$Outcome)) {
  
  # Overall model row (all as character)
  overall <- overall_models %>% filter(Outcome == domain)
  
  overall_row <- data.frame(
    Outcome = domain,
    Model = "Overall Model",
    k = as.character(overall$Q_df),
    SMD = sprintf("%.3f", overall$SMD),
    CI = paste0("[", sprintf("%.3f", overall$CI_lower), "; ", sprintf("%.3f", overall$CI_upper), "]"),
    p_value = ifelse(overall$p_value < .001, "< .001", sprintf("%.3f", overall$p_value)),
    I2 = paste0(sprintf("%.1f", overall$I2), "%"),
    Subgroup_Differences = paste0(
      "Q = ", sprintf("%.2f", overall$Q_between),
      ", p = ", ifelse(overall$Q_between_p_value < .001, "< .001", sprintf("%.3f", overall$Q_between_p_value))
    ),
    stringsAsFactors = FALSE
  )
  
subgroup_rows <- subgroups %>%
  transmute(
    Outcome = "",
    Model = as.character(Subgroup),
    k = as.character(k),
    SMD = sprintf("%.2f", SMD),
    CI = paste0("[", sprintf("%.2f", CI_lower), "; ", sprintf("%.2f", CI_upper), "]"),
    p_value = "–",
    I2 = ifelse(I2 == "--", "–", paste0(sprintf("%.1f", as.numeric(I2)), "%")),
    Subgroup_Differences = "–"
  )
  
  # Combine overall + subgroups
  combined_table[[domain]] <- bind_rows(overall_row, subgroup_rows)
}

# Combine all outcomes into one table
final_table <- bind_rows(combined_table)

library(flextable)
library(officer)

# Create the flextable
ft <- flextable(final_table) %>%
  set_header_labels(
    Outcome = "Outcome Domain",
    Model = "Model Type / Subgroup",
    k = "Studies (k)",
    SMD = "SMD",
    CI = "95% CI",
    p_value = "p-value",
    I2 = "I²",
    Subgroup_Differences = "Subgroup Differences (Q, p)"
  ) %>%
  fontsize(size = 10, part = "all") %>%
  autofit() %>%
  bold(i = which(final_table$Model == "Overall Model"), bold = TRUE) %>%
  align(align = "center", part = "all") %>%
  theme_booktabs() %>%
  border_outer(part = "all", border = fp_border(color = "black", width = 1))

# Find last row of each domain (last subgroup before next overall model or end)
domain_starts <- which(final_table$Model == "Overall Model")
last_rows <- c(domain_starts[-1] - 1, nrow(final_table))

# Apply horizontal borders ONLY at those rows
ft <- hline(ft, i = last_rows, border = fp_border(color = "black", width = 1))


# Create Word document with landscape orientation
doc <- read_docx() %>%
  body_add_par("Table 1. Meta-analytic model results for aphasia rehabilitation outcomes, including overall effect sizes and subgroup analyses by control group type.", style = "heading 1") %>%
  body_add_flextable(ft) %>%
  body_add_par(
    "Note. SMD = Standardized Mean Difference; CI = Confidence Interval. I² is reported for the overall model and subgroups. p-values are presented for the overall model and subgroup differences; subgroup-specific p-values are not estimated. Q refers to the test statistic for subgroup differences. k = number of studies within each subgroup.",
    style = "Normal"
  )

# Apply landscape layout
doc <- body_end_section_landscape(doc)

# Save the Word document
print(doc, target = "Publication_Ready_Landscape_Table2.docx")




# Combined Forest Plots ---------------------------------------------------

# Load libraries
library(meta)
library(ggplot2)
library(dplyr)
library(stringr)

# Proper control group names
control_group_lookup <- c(
  "active comparator - delivery model" = "Active Comparator - Delivery Model",
  "active comparator - regimen" = "Active Comparator - Regimen",
  "active comparator - theoretical" = "Active Comparator - Theoretical",
  "activity control" = "Activity Control",
  "no treatment" = "No Treatment",
  "social support" = "Social Support",
  "usual care - protocolized" = "Usual Care - Protocolized",
  "usual care - unrestricted" = "Usual Care - Unrestricted",
  "waitlist" = "Waitlist"
)

# Outcome models with capitalized names
models <- list(
  "Aphasia Severity" = m.sev.broad.clean.subgroup,
  "Expressive Language" = m.exp.broad.clean.subgroup,
  "Receptive Language" = m.rec.broad.subgroup.clean,
  "Functional Communication" = m.func.broad.subgroup.clean
)

# Extract individual studies
extract_individual_studies <- function(model, model_name, control_group) {
  idx <- which(tolower(model$byvar) == control_group)
  if (length(idx) == 0) return(NULL)
  
  data.frame(
    Study = model$studlab[idx],
    ControlGroup = control_group_lookup[control_group],
    Outcome = model_name,
    Estimate = model$TE[idx],
    CI_Lower = model$lower[idx],
    CI_Upper = model$upper[idx],
    Type = "Study"
  )
}

# Extract pooled effect
extract_pooled_effect <- function(model, model_name, control_group, effect_type = "random") {
  subgroup_labels <- names(model$TE.random.w)
  idx <- which(tolower(subgroup_labels) == control_group)
  if (length(idx) == 0) return(NULL)
  
  data.frame(
    Study = "Pooled Effect",
    ControlGroup = control_group_lookup[control_group],
    Outcome = model_name,
    Estimate = if (effect_type == "random") model$TE.random.w[idx] else model$TE.fixed.w[idx],
    CI_Lower = if (effect_type == "random") model$lower.random.w[idx] else model$lower.fixed.w[idx],
    CI_Upper = if (effect_type == "random") model$upper.random.w[idx] else model$upper.fixed.w[idx],
    Type = "Pooled"
  )
}

# Loop over each control group
for (group in names(control_group_lookup)) {
  
  group_data <- bind_rows(
    lapply(names(models), function(outcome) {
      model <- models[[outcome]]
      studies <- extract_individual_studies(model, outcome, group)
      pooled <- extract_pooled_effect(model, outcome, group)
      bind_rows(studies, pooled)
    })
  )
  
  if (nrow(group_data) == 0) {
    message(paste("No data found for control group:", control_group_lookup[group]))
    next
  }
  
  # Force pooled effect to always be at the bottom
  study_order <- group_data %>%
    filter(Type == "Study") %>%
    pull(Study) %>%
    unique()
  
  study_levels <- c(sort(study_order), "Pooled Effect")
  
  group_data$Study <- factor(group_data$Study, levels = study_levels)
  
  # Create forest plot with pooled effect always at bottom
  p <- ggplot(group_data, aes(y = Study, x = Estimate, xmin = CI_Lower, xmax = CI_Upper)) +
    geom_point(aes(shape = Type, size = Type, color = Type)) +
    geom_errorbarh(height = 0.2) +
    facet_wrap(~ Outcome, scales = "free_y") +
    scale_shape_manual(values = c("Study" = 16, "Pooled" = 18)) +
    scale_size_manual(values = c("Study" = 2.5, "Pooled" = 4)) +
    scale_color_manual(values = c("Study" = "black", "Pooled" = "red")) +
    labs(
      title = paste("Forest Plot for", control_group_lookup[group]),
      x = "Effect Size (95% CI)",
      y = "Study"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 12, face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 9),
      axis.title = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
  
  
  # Show plot
  print(p)
  
  # Save plot
  filename <- paste0(gsub(" ", "_", control_group_lookup[group]), "_forest_plot_standard.png")
  ggsave(filename, plot = p, width = 12, height = 8, dpi = 300)
}






# Characteristics table ---------------------------------------------------

single_study <- smd_results %>%
  filter(include_broad == "Y",
         outcome_designation_broad %in% c("aphasia severity", "expressive language", "receptive language", "functional communication")) %>%
  group_by(study_ID) %>%
  summarise(
    covidence_id = first(covidence_number),
    X3_plus_arms = first(X3_plus_arms),
    plus_UC = first(plus_UC),
    exp_group_description = first(exp_group_description),
    ctrl_group_description = first(ctrl_group_description),
    control_group_type_collapsed = first(control_group_type),
    acute_chronic = first(acute_chronic),
    total_n = first(total_n),
    outcomes = paste(outcome_measure, collapse = "; "),
    .groups = "drop"
  )

study_table <- read.csv("single study.csv")

study_table[] <- lapply(study_table, function(x) {
  if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8", sub = "")
  else x
})


study_table_for_paper <- flextable(study_table) 


# EXTRA VISUALS - influence analyses --------------------------------------

export_influence_pngs <- function(model_inf, prefix) {
  
  # Baujat plot
  png(paste0(prefix, "_Baujat.png"), width = 3000, height = 2000, res = 300)
  plot(model_inf, "baujat")
  title(main = paste(prefix, "- Baujat"), cex.main = 3, font.main = 2)
  dev.off()
  
  # Influence plot
  png(paste0(prefix, "_Influence.png"), width = 3000, height = 2000, res = 300)
  plot(model_inf, "influence")
  title(main = paste(prefix, "- Influence"), cex.main = 3, font.main = 2)
  dev.off()
  
  # Effect Sizes plot
  png(paste0(prefix, "_ES.png"), width = 3000, height = 2000, res = 300)
  plot(model_inf, "es")
  title(main = paste(prefix, "- Effect Sizes"), cex.main = 3, font.main = 2)
  dev.off()
}


# Export high-res PNGs with better proportions
export_influence_pngs(m.sev.broad.inf, "Sev")
export_influence_pngs(m.exp.broad.inf, "Exp")
export_influence_pngs(m.rec.broad.inf, "Rec")
export_influence_pngs(m.func.broad.inf, "Func")

# TABLES

library(knitr)

# Severity table
sev_table <- data.frame(
  Model = "Aphasia Severity - broad",
  Outliers = "Akabogu et al. 2019, Teng 2017, Wei et al. 2021",
  k = 43,
  SMD = "0.2069",
  CI = "[0.0885; 0.3252]",
  p_value = "0.0010",
  I2 = "27.5%"
)

# Expressive table
exp_table <- data.frame(
  Model = "Expressive Language - broad",
  Outliers = "Simic et al. 2021, Teng 2017, van der Meulen et al. 2014, Wei et al. 2021, Wenke et al. 2018",
  k = 57,
  SMD = "0.1091",
  CI = "[0.0281; 0.1900]",
  p_value = "0.0092",
  I2 = "0.0%"
)

# Receptive table
rec_table <- data.frame(
  Model = "Receptive Language - broad",
  Outliers = "Fleming et al. 2021, Wei et al. 2021, Zhang et al. 2021",
  k = 40,
  SMD = "0.2251",
  CI = "[0.1130; 0.3372]",
  p_value = "0.0002",
  I2 = "4.3%"
)

# Functional table
func_table <- data.frame(
  Model = "Functional Communication - broad",
  Outliers = "van der Meulen et al. 2014, Wei et al. 2021, Wilssens et al. 2015",
  k = 23,
  SMD = "0.0909",
  CI = "[-0.0365; 0.2183]",
  p_value = "0.1532",
  I2 = "8.8%"
)

# View the separate tables
kable(sev_table, caption = "Outliers and Results - Aphasia Severity (broad)")
kable(exp_table, caption = "Outliers and Results - Expressive Language (broad)")
kable(rec_table, caption = "Outliers and Results - Receptive Language (broad)")
kable(func_table, caption = "Outliers and Results - Functional Communication (broad)")


library(officer)
library(flextable)

# Helper to format tables
format_table <- function(ft, caption_text) {
  ft %>%
    set_caption(caption_text) %>%
    autofit() %>%
    width(j = "Outliers", width = 4) %>%  # Limit "Outliers" column width
    fontsize(size = 9, part = "all") %>%  # Reduce font size to fit better
    align(align = "left", part = "all") %>%
    padding(padding = 2, part = "all")
}

# Apply formatting
sev_ft <- format_table(flextable(sev_table), "Table 1. Outliers and Results - Aphasia Severity (broad)")
exp_ft <- format_table(flextable(exp_table), "Table 2. Outliers and Results - Expressive Language (broad)")
rec_ft <- format_table(flextable(rec_table), "Table 3. Outliers and Results - Receptive Language (broad)")
func_ft <- format_table(flextable(func_table), "Table 4. Outliers and Results - Functional Communication (broad)")

# Create Word document
doc <- read_docx()

doc <- doc %>%
  body_add_par("Supplementary Tables - Outlier Results", style = "heading 1") %>%
  
  body_add_flextable(sev_ft) %>%
  body_add_par("") %>%
  
  body_add_flextable(exp_ft) %>%
  body_add_par("") %>%
  
  body_add_flextable(rec_ft) %>%
  body_add_par("") %>%
  
  body_add_flextable(func_ft)

# Save the document
print(doc, target = "Outlier_Results_Tables.docx")



# IRR control group labels ------------------------------------------------

# Load required packages
library(dplyr)
library(irr)
library(stringr)

# Step 1: Load the dataset
IRR_cg <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/Aphasia RCT Meta-Analysis/Analyses/Control group MA/cg_irr.csv", 
                   stringsAsFactors = FALSE)

# Step 2: Select relevant columns, including Study ID
IRR_cg_full <- IRR_cg %>% 
  select(Study, Control.Group.Type_alex, Control.Group.Type_marissa)

# Step 3: Data Cleaning - Standardize text formatting (lowercase, remove extra spaces)
IRR_cg_full <- IRR_cg_full %>%
  mutate(
    Control.Group.Type_alex = str_to_lower(str_trim(Control.Group.Type_alex)),
    Control.Group.Type_marissa = str_to_lower(str_trim(Control.Group.Type_marissa))
  )

# Step 4: Create a pared-down dataset for Kappa calculation
IRR_cg <- IRR_cg_full %>% 
  select(Control.Group.Type_alex, Control.Group.Type_marissa)

# Step 5: Compute Cohen’s Kappa for Interrater Reliability
kappa_result <- kappa2(IRR_cg, weight = "unweighted")  # Unweighted Cohen’s Kappa
print(kappa_result)

# Step 6: Identify disagreements
IRR_cg_full <- IRR_cg_full %>%
  mutate(Agreement = Control.Group.Type_alex == Control.Group.Type_marissa)

# Step 7: Extract and display disagreements
disagreements <- IRR_cg_full %>% 
  filter(Agreement == FALSE)

# Print disagreement details (Study ID + mismatches)
print(disagreements)

# Step 8: Generate a confusion matrix to see where disagreements happen most
conf_matrix <- table(IRR_cg$Control.Group.Type_alex, IRR_cg$Control.Group.Type_marissa)
print(conf_matrix)

# Step 9: Classify and summarize disagreement types
disagreement_types <- disagreements %>%
  mutate(Type = case_when(
    grepl("no treament|actice", Control.Group.Type_alex) ~ "Typo",
    Control.Group.Type_alex %in% c("usual care - protocolized", "usual care - unrestricted") &
      Control.Group.Type_marissa %in% c("usual care - protocolized", "usual care - unrestricted") ~ "Usual Care Discrepancy",
    TRUE ~ "Conceptual Difference"
  ))

# Print summary of disagreement categories
print(table(disagreement_types$Type))

# Optional: Save disagreements for review
write.csv(disagreements, "disagreements.csv", row.names = FALSE)

# Done! 🎉
