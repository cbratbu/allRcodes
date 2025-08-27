###########################################################################
###                              1. SET-UP                              ###
###########################################################################


# Load packages
library(tidyverse)
library(dplyr)
library(lme4)
library(ggplot2)
library(broom.mixed)
library(buildmer)
library(broom)
library(mice)
library(bannerCommenter)
library(mitools)
library(lmerTest)
library(ggsignif)


# Read assessment data
assessment_data <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/assessments_raw.csv", stringsAsFactors = T)

# Factorize variables - assessment data
assessment_data$id <- factor(assessment_data$id)
assessment_data$L1 <- factor(assessment_data$L1)
assessment_data$sex <- factor(assessment_data$sex)
assessment_data$timepoint <- factor(assessment_data$timepoint)
assessment_data$edu <- factor(assessment_data$edu)

# Separate datasets for imputation
assessment_pre_L1eng <- subset(assessment_data, L1 == "English" & timepoint == "Pre-Tx")
assessment_pre_L1span <- subset(assessment_data, L1 == "Spanish" & timepoint == "Pre-Tx")
assessment_post_L1eng <- subset(assessment_data, L1 == "English" & timepoint == "Post-Tx")
assessment_post_L1span <- subset(assessment_data, L1 == "Spanish" & timepoint == "Post-Tx")

############################################################################
###                       ASSESSMENTS - IMPUTATION                       ###
############################################################################


# Participant IDs to exclude from imputation
no_impute_ids <- c("BUBA099", "BUBA123", "BUBA066", "BUBA137", "BUBA020", "BUBA117")


# Create a custom 'where' matrix for the Spanish L1 post-treatment subset
where_matrix_span <- matrix(FALSE, nrow = nrow(assessment_post_L1span), ncol = ncol(assessment_post_L1span))
where_matrix_span[, 10:101] <- TRUE
no_impute_condition_span <- assessment_post_L1span$id %in% no_impute_ids
where_matrix_span[no_impute_condition_span, ] <- FALSE  # Apply exclusion to all columns for these rows

# Imputations
assessment_pre_L1eng_imputed <- mice(assessment_pre_L1eng, m = 5, maxit = 30, method = 'pmm', seed = 123)
assessment_pre_L1span_imputed <- mice(assessment_pre_L1span, m = 5, maxit = 30, method = 'pmm', seed = 123)
assessment_post_L1eng_imputed <- mice(assessment_post_L1eng, m = 5, maxit = 30, method = 'pmm', seed = 123)

# Imputations minus special IDs
assessment_post_L1span_imputed <- mice(assessment_post_L1span, m = 5, maxit = 30, method = 'pmm', seed = 123, where = where_matrix_span)


##################################################################
##                         SANITY CHECK                        ##
##################################################################
imputed_data_sample <- complete(combined_mids_all, action = 1)
common_columns <- intersect(
  colnames(assessment_data)[10:101], 
  colnames(imputed_data_sample)
)

# Modified function to produce more readable output
calc_summary_stats <- function(data, columns, tp) {
  # Create a list to store all the different statistics
  result_list <- list()
  
  # Calculate each statistic type separately
  # Mean values
  means <- data %>%
    filter(timepoint == tp) %>%
    summarise(across(all_of(columns), ~mean(.x, na.rm = TRUE)))
  means$statistic <- "mean"
  
  # Median values
  medians <- data %>%
    filter(timepoint == tp) %>%
    summarise(across(all_of(columns), ~median(.x, na.rm = TRUE)))
  medians$statistic <- "median"
  
  # Standard deviation
  sds <- data %>%
    filter(timepoint == tp) %>%
    summarise(across(all_of(columns), ~sd(.x, na.rm = TRUE)))
  sds$statistic <- "sd"
  
  # Min values
  mins <- data %>%
    filter(timepoint == tp) %>%
    summarise(across(all_of(columns), ~min(.x, na.rm = TRUE)))
  mins$statistic <- "min"
  
  # Max values
  maxs <- data %>%
    filter(timepoint == tp) %>%
    summarise(across(all_of(columns), ~max(.x, na.rm = TRUE)))
  maxs$statistic <- "max"
  
  # Combine all statistics
  result <- rbind(means, medians, sds, mins, maxs)
  
  # Add timepoint information
  result$timepoint <- tp
  
  return(result)
}

# Calculate for original data
original_stats_pre <- calc_summary_stats(assessment_data, common_columns, "Pre-Tx")
original_stats_post <- calc_summary_stats(assessment_data, common_columns, "Post-Tx")

# Add dataset information
original_stats_pre$dataset <- "Original"
original_stats_post$dataset <- "Original"

# Initialize results dataframe
all_stats <- rbind(original_stats_pre, original_stats_post)

# Process imputed datasets
for (i in 1:5) {
  imputed_data <- complete(combined_mids_all, action = i)
  
  imputed_stats_pre <- calc_summary_stats(imputed_data, common_columns, "Pre-Tx")
  imputed_stats_post <- calc_summary_stats(imputed_data, common_columns, "Post-Tx")
  
  # Add dataset information
  imputed_stats_pre$dataset <- paste("Imputed", i)
  imputed_stats_post$dataset <- paste("Imputed", i)
  
  # Combine with existing stats
  all_stats <- rbind(all_stats, imputed_stats_pre, imputed_stats_post)
}

# Reorder columns for better readability
all_stats <- all_stats %>%
  select(dataset, timepoint, statistic, everything())

write.csv(all_stats, "/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/FInal Take Feb 2024/Transfer Analysis Final Take 2024/sanitycheck2.csv")
##################################################################
##                         COMBINE MIDS                         ##
##################################################################
combined_mids_pre <- rbind(assessment_pre_L1eng_imputed, assessment_pre_L1span_imputed)
combined_mids_post <- rbind(assessment_post_L1eng_imputed, assessment_post_L1span_imputed)
combined_mids_all<- rbind(combined_mids_pre, combined_mids_post)

#################################################################
##                   MIDS REORGANIZATION                      ##
#################################################################
calculate_composite <- function(...){
  components <- c(...)
  if (any(is.na(components))) {
    return(NA_real_)
  } else {
    return(sum(components, na.rm = TRUE))
  }
}

create_composite_scores <- function(dataset) {
  dataset <- dataset %>%
    mutate(
      semantic_composite_eng = (
        bat_semantic_cat_eng * 5 / 40 +
          bat_synonyms_eng * 5 / 40 +
          bat_antonyms_eng * 5 / 40 +
          bat_antonyms2_eng * 5 / 40 +
          bat_sem_acceptability_eng * 10 / 40 +
          bat_sem_opp_eng * 10 / 40
      ),
      
      semantic_composite_span = (
        bat_semantic_cat_span * 5 / 40 +
          bat_synonyms_span * 5 / 40 +
          bat_antonyms_span * 5 / 40 +
          bat_antonyms2_span * 5 / 40 +
          bat_sem_acceptability_span * 10 / 40 +
          bat_sem_opp_span * 10 / 40
      ),
      
      comprehension_composite_eng = (
        bat_pointing_eng * 10 / 150 +
          bat_semicomplex_commands_eng * 10 / 150 +
          bat_complex_commands_eng * 20 / 150 +
          bat_verbal_aud_discrimination_eng * 18 / 150 +
          bat_syntactic_comp_eng * 87 / 150 +
          bat_listening_comp_eng * 5 / 150
      ),
      
      comprehension_composite_span = (
        bat_pointing_span * 10 / 150 +
          bat_semicomplex_commands_span * 10 / 150 +
          bat_complex_commands_span * 20 / 150 +
          bat_verbal_aud_discrimination_span * 18 / 150 +
          bat_syntactic_comp_span * 87 / 150 +
          bat_listening_comp_span * 5 / 150
      ),
      
      judgment_composite_eng = (
        bat_grammaticality_judg_eng * 10 / 30 +
          bat_sem_acceptability_eng * 10 / 30 +  
          bat_word_judg_eng * 30 / 30
      ),
      
      judgment_composite_span = (
        bat_grammaticality_judg_span * 10 / 30 +
          bat_sem_acceptability_span * 10 / 30 +  
          bat_word_judg_span * 30 / 30
      ),
      
      repetition_composite_eng = (
        bat_word_rep_eng * 30 / 37 +
          bat_sentence_rep_eng * 7 / 37
      ),
      
      repetition_composite_span = (
        bat_word_rep_span * 30 / 37 +
          bat_sentence_rep_span * 7 / 37
      ),
      
      reading_comp_composite_eng = (
        bat_reading_comp_words_eng * 10 / 21 +
          bat_reading_comp_sent_eng * 10 / 21 +
          bat_reading_comp_txt_eng * 6 / 21
      ),
      
      reading_comp_composite_span = (
        bat_reading_comp_words_span * 10 / 21 +
          bat_reading_comp_sent_span * 10 / 21 +
          bat_reading_comp_txt_span * 6 / 21
      ),
      
      writing_composite_eng = (
        bat_copying_eng * 5 / 30 +
          bat_dictation_eng * 5 / 30 +
          bat_dictation_sent_eng * 5 / 30
      ),
      
      writing_composite_span = (
        bat_copying_span * 5 / 30 +
          bat_dictation_span * 5 / 30 +
          bat_dictation_sent_span * 5 / 30
      ),
      
      morphology_composite_eng = (
        bat_deriv_morph_eng * 10 / 35 +
          bat_morph_opp_eng * 10 / 35 +
          bat_antonyms_eng * 5 / 35
      ),
      
      morphology_composite_span = (
        bat_deriv_morph_span * 10 / 35 +
          bat_morph_opp_span * 10 / 35 +
          bat_antonyms_span * 5 / 35
      ),
      
      translation_composite_eng = (
        bat_c_word_translation_eng * 10 / 28 +
          bat_c_sent_translation_eng * 18 / 28
      ),
      
      translation_composite_span = (
        bat_c_word_translation_span * 10 / 28 +
          bat_c_sent_translation_span * 18 / 28
      ),
      
      production_composite_eng = (
        bat_descrip_eng * 3 / 8 +
          bat_sentence_const_eng * 5 / 8
      ),
      
      production_composite_span = (
        bat_descrip_span * 3 / 8 +
          bat_sentence_const_span * 5 / 8
      )
    )
  
  return(dataset)
}

adjust_and_populate <- function(dataset) {
  # Process specific assessments for trained and untrained columns
  specific_assessments <- list(
    letter_length_reading = list(palpa = "palpa29_eng", epla = "epla28_span"),
    spoken_word_picture_matching = list(palpa = "palpa47_eng", epla = "epla45_span"),
    written_word_picture_matching = list(palpa = "palpa48_eng", epla = "epla46_span"),
    auditory_synonym_judgments = list(palpa = "palpa49_eng", epla = "epla47_span"),
    written_synonym_judgments = list(palpa = "palpa50_eng", epla = "epla48__span"),
    word_semantic_association = list(palpa = "palpa51_eng", epla = "epla49__span")
  )
  
  for(assessment_name in names(specific_assessments)) {
    palpa_col <- specific_assessments[[assessment_name]]$palpa
    epla_col <- specific_assessments[[assessment_name]]$epla
    
    dataset <- dataset %>%
      mutate(
        "{assessment_name}_trained" := case_when(
          tx_lang == "English" ~ !!sym(palpa_col),
          tx_lang == "Spanish" ~ !!sym(epla_col),
          TRUE ~ NA_real_
        ),
        "{assessment_name}_untrained" := case_when(
          tx_lang == "English" ~ !!sym(epla_col),
          tx_lang == "Spanish" ~ !!sym(palpa_col),
          TRUE ~ NA_real_
        )
      )
  }
  
  # Dynamically process other assessments for trained and untrained columns
  eng_columns <- names(dataset)[grepl("_eng$", names(dataset))]
  span_columns <- names(dataset)[grepl("_span$", names(dataset))]
  
  specific_assessments_flat <- unlist(specific_assessments)
  
  for(col in setdiff(eng_columns, specific_assessments_flat)) {
    col_base <- sub("_eng$", "", col)
    col_span <- paste0(col_base, "_span")
    
    if (col_span %in% span_columns) {
      dataset <- dataset %>%
        mutate(
          "{col_base}_trained" := case_when(
            tx_lang == "English" ~ .data[[col]],
            tx_lang == "Spanish" ~ .data[[col_span]],
            TRUE ~ NA_real_
          ),
          "{col_base}_untrained" := case_when(
            tx_lang == "English" ~ .data[[col_span]],
            tx_lang == "Spanish" ~ .data[[col]],
            TRUE ~ NA_real_
          )
        )
    }
  }
  
  cols_to_remove <- grep("^bat_|_eng$|_span$", names(dataset), value = TRUE)
  dataset <- select(dataset, -all_of(cols_to_remove))
  
  
  return(dataset)
}
# Revised function to convert, manipulate, and revert the mids object
convert_manipulate_and_revert_mids <- function(midsobj) {
  # Convert mids object to long format data frame
  long_format <- mice::complete(midsobj, action = 'long', include = TRUE)
  
  # Apply creation of composite scores
  long_format <- long_format %>% 
    group_by(.imp) %>%
    do(create_composite_scores(.)) %>%
    ungroup()
  
  # Adjust for specific/general assessments and append _trained or _untrained
  long_format <- adjust_and_populate(long_format)
  
  # Convert back to mids object, ensuring correct structure
  midsobj_modified <- mice::as.mids(long_format)
  
  return(midsobj_modified)
}

# Applying the function to your mids object
combined_mids_all_modified <- convert_manipulate_and_revert_mids(combined_mids_all)

# Assuming 'timepoint' is a factor. If it's not, first convert it to a factor.
combined_mids_all_modified <- mice::complete(combined_mids_all_modified, "long", include = TRUE)

# Change the reference level for 'timepoint' to 'Pre-Tx'
combined_mids_all_modified$timepoint <- factor(combined_mids_all_modified$timepoint, levels = c("Pre-Tx", "Post-Tx"))

# Convert back to mids object if needed
combined_mids_all_modified <- mice::as.mids(combined_mids_all_modified)

#################################################################
##                            % ACC                            ##
#################################################################
# Define a helper function to calculate percent accuracy
calculate_percent_accuracy <- function(score, max_score) {
  return((score / max_score))
}

# Update function to modify dataset with percent accuracy
add_percent_accuracy <- function(dataset) {
  dataset <- dataset %>%
    mutate(
      spoken_word_picture_matching_trained_percent = calculate_percent_accuracy(spoken_word_picture_matching_trained, 40) * 100,
      spoken_word_picture_matching_untrained_percent = calculate_percent_accuracy(spoken_word_picture_matching_untrained, 40) * 100,
      written_word_picture_matching_trained_percent = calculate_percent_accuracy(written_word_picture_matching_trained, 40) * 100,
      written_word_picture_matching_untrained_percent = calculate_percent_accuracy(written_word_picture_matching_untrained, 40) * 100,
      auditory_synonym_judgments_trained_percent = calculate_percent_accuracy(auditory_synonym_judgments_trained, 60) * 100,
      auditory_synonym_judgments_untrained_percent = calculate_percent_accuracy(auditory_synonym_judgments_untrained, 60) * 100,
      written_synonym_judgments_trained_percent = calculate_percent_accuracy(written_synonym_judgments_trained, 60) * 100,
      written_synonym_judgments_untrained_percent = calculate_percent_accuracy(written_synonym_judgments_untrained, 60) * 100,
      word_semantic_association_trained_percent = calculate_percent_accuracy(word_semantic_association_trained, 30) * 100,
      word_semantic_association_untrained_percent = calculate_percent_accuracy(word_semantic_association_untrained, 30) * 100,
      bnt_trained_percent = calculate_percent_accuracy(bnt_trained, 60) * 100,
      bnt_untrained_percent = calculate_percent_accuracy(bnt_untrained, 60) * 100,
      wab_aq_trained_percent = calculate_percent_accuracy(wab_aq_trained, 100) * 100,
      wab_aq_untrained_percent = calculate_percent_accuracy(wab_aq_untrained, 100) * 100,
      ravens_percent = calculate_percent_accuracy(ravens, 37) * 100,
      papt_total_percent = calculate_percent_accuracy(papt_total, 52) * 100,
      translation_composite_trained_percent = calculate_percent_accuracy(translation_composite_trained, 28) * 100,
      translation_composite_untrained_percent = calculate_percent_accuracy(translation_composite_untrained, 28) * 100
    )
  
  return(dataset)
}

# Revised function to integrate percent accuracy calculation into the mids manipulation
convert_manipulate_and_revert_mids_with_accuracy <- function(midsobj) {
  # Convert mids object to long format data frame
  long_format <- mice::complete(midsobj, action = 'long', include = TRUE)
  
  # Apply creation of composite scores
  long_format <- long_format %>% 
    group_by(.imp) %>%
    do(create_composite_scores(.)) %>%
    ungroup()
  
  # Adjust for specific/general assessments and append _trained or _untrained
  long_format <- adjust_and_populate(long_format)
  
  # Add percent accuracy
  long_format <- add_percent_accuracy(long_format)
  
  # Convert back to mids object, ensuring correct structure
  midsobj_modified_with_accuracy <- mice::as.mids(long_format)
  
  return(midsobj_modified_with_accuracy)
}

# Now apply the revised function to your 'combined_mids_all' mids object
combined_mids_all_modified_with_accuracy <- convert_manipulate_and_revert_mids_with_accuracy(combined_mids_all)

# Adjustments for 'timepoint' factor and converting back to mids object
combined_mids_all_modified_with_accuracy <- mice::complete(combined_mids_all_modified_with_accuracy, "long", include = TRUE)
combined_mids_all_modified_with_accuracy$timepoint <- factor(combined_mids_all_modified_with_accuracy$timepoint, levels = c("Pre-Tx", "Post-Tx"))
combined_mids_all_modified_with_accuracy <- mice::as.mids(combined_mids_all_modified_with_accuracy)

#################################################################
##                         CHANGE IN %                         ##
#################################################################

long_format_dataset <- mice::complete(combined_mids_all_modified_with_accuracy, action = 'long', include = TRUE)

# Function to calculate change in percentage accuracy for each _percent column
calculate_percent_changes <- function(dataset) {
  # Identify columns ending with '_percent'
  percent_columns <- grep("_percent$", names(dataset), value = TRUE)
  
  # For each percent column, calculate the change and update the dataset
  for(col_name in percent_columns) {
    # Create a new column name for the change
    change_col_name <- paste0(col_name, "_change")
    
    # Calculate the change for each ID
    dataset <- dataset %>%
      group_by(id) %>%
      mutate(
        !!change_col_name := if_else(timepoint == "Post-Tx", get(col_name) - lag(get(col_name)), NA_real_),
        !!change_col_name := if_else(timepoint == "Pre-Tx", lead(get(!!change_col_name)), get(!!change_col_name))
      ) %>%
      ungroup()
  }
  
  return(dataset)
}

# Apply the function to calculate changes
long_format_dataset <- calculate_percent_changes(long_format_dataset)

# Assuming you want to convert it back to a mids object after the calculation

combined_mids_all_modified_with_accuracy_changes <- mice::complete(combined_mids_all_modified_with_accuracy_changes, "long", include = TRUE)
combined_mids_all_modified_with_accuracy$timepoint <- factor(combined_mids_all_modified_with_accuracy_changes$timepoint, levels = c("Pre-Tx", "Post-Tx"))
mids_perc_change <- mice::as.mids(combined_mids_all_modified_with_accuracy_changes)

############################################################################
############################################################################
###                                                                      ###
###                  PREPARE DATA FOR ASSESSMENT MODELS                  ###
###                                                                      ###
############################################################################
############################################################################

# Convert the mids object to a long-format data.frame
long1 <- complete(combined_mids_all_modified, action = "long", include = TRUE)

# Use pivot_longer to merge specified columns under "assessment"
long2 <- long1 %>%
  pivot_longer(
    cols = c(
      "papt_total",
      "ravens",
      "naming_screener_trained",
      "naming_screener_untrained",
      "wab_aq_trained",
      "wab_aq_untrained",
      "bnt_trained",
      "bnt_untrained",
      "semantic_composite_trained",
      "semantic_composite_untrained",
      "comprehension_composite_trained",
      "comprehension_composite_untrained",
      "judgment_composite_trained",
      "judgment_composite_untrained",
      "repetition_composite_trained",
      "repetition_composite_untrained",
      "reading_comp_composite_trained",
      "reading_comp_composite_untrained",
      "writing_composite_trained",
      "writing_composite_untrained",
      "morphology_composite_trained",
      "morphology_composite_untrained",
      "translation_composite_trained",
      "translation_composite_untrained",
      "letter_length_reading_trained",
      "letter_length_reading_untrained",
      "spoken_word_picture_matching_trained",
      "spoken_word_picture_matching_untrained",
      "written_word_picture_matching_trained",
      "written_word_picture_matching_untrained",
      "auditory_synonym_judgments_trained",
      "auditory_synonym_judgments_untrained",
      "written_synonym_judgments_trained",
      "written_synonym_judgments_untrained",
      "word_semantic_association_trained",
      "word_semantic_association_untrained",
      "production_composite_trained",
      "production_composite_untrained"
    ),
    names_to = "assessment",
    values_to = "score"
  ) %>%
  mutate(
    trained_untrained = case_when(
      str_detect(assessment, "_trained") ~ "Trained",
      str_detect(assessment, "_untrained") ~ "Untrained",
      TRUE ~ "NA" # For assessments that do not specify training status
    )
  )


# Note: This script assumes all your assessment columns are formatted with "_trained" or "_untrained" 
# suffixes to automatically assign the "trained_untrained" status. Adjust as necessary for your data.
long2 <- long2 %>%
  mutate(assessment = str_remove(assessment, "_trained|_untrained"))
# Adding a unique identifier for each row
long2$new_id <- 1:nrow(long2)

# Convert back to a mids object
mids_long <- as.mids(long2, .id = "new_id")



# Complete
complete_data_long <- complete(mids_long, action = "long", include = TRUE)

# Subset data for papt_total
papt_total_mids <- subset(complete_data_long, assessment == "papt_total")
papt_total_mids <- as.mids(papt_total_mids)

# Subset data for ravens
ravens_mids <- subset(complete_data_long, assessment == "ravens")
ravens_mids <- as.mids(ravens_mids)

# Subset data for naming_screener
naming_screener_mids <- subset(complete_data_long, assessment == "naming_screener")
naming_screener_mids <- as.mids(naming_screener_mids)

# Subset data for wab_aq
wab_aq_mids <- subset(complete_data_long, assessment == "wab_aq")
wab_aq_mids <- as.mids(wab_aq_mids)

# Subset data for bnt
bnt_mids <- subset(complete_data_long, assessment == "bnt")
bnt_mids <- as.mids(bnt_mids)

# Subset data for semantic_composite
semantic_composite_mids <- subset(complete_data_long, assessment == "semantic_composite")
semantic_composite_mids <- as.mids(semantic_composite_mids)

# Subset data for comprehension_composite
comprehension_composite_mids <- subset(complete_data_long, assessment == "comprehension_composite")
comprehension_composite_mids <- as.mids(comprehension_composite_mids)

# Subset data for judgment_composite
judgment_composite_mids <- subset(complete_data_long, assessment == "judgment_composite")
judgment_composite_mids <- as.mids(judgment_composite_mids)

# Subset data for repetition_composite
repetition_composite_mids <- subset(complete_data_long, assessment == "repetition_composite")
repetition_composite_mids <- as.mids(repetition_composite_mids)

# Subset data for reading_comp_composite
reading_comp_composite_mids <- subset(complete_data_long, assessment == "reading_comp_composite")
reading_comp_composite_mids <- as.mids(reading_comp_composite_mids)

# Subset data for writing_composite
writing_composite_mids <- subset(complete_data_long, assessment == "writing_composite")
writing_composite_mids <- as.mids(writing_composite_mids)

# Subset data for morphology_composite
morphology_composite_mids <- subset(complete_data_long, assessment == "morphology_composite")
morphology_composite_mids <- as.mids(morphology_composite_mids)

# Subset data for translation_composite
translation_composite_mids <- subset(complete_data_long, assessment == "translation_composite")
translation_composite_mids <- as.mids(translation_composite_mids)

# Subset data for letter_length_reading
letter_length_reading_mids <- subset(complete_data_long, assessment == "letter_length_reading")
letter_length_reading_mids <- as.mids(letter_length_reading_mids)

# Subset data for spoken_word_picture_matching
spoken_word_picture_matching_mids <- subset(complete_data_long, assessment == "spoken_word_picture_matching")
spoken_word_picture_matching_mids <- as.mids(spoken_word_picture_matching_mids)

# Subset data for written_word_picture_matching
written_word_picture_matching_mids <- subset(complete_data_long, assessment == "written_word_picture_matching")
written_word_picture_matching_mids <- as.mids(written_word_picture_matching_mids)

# Subset data for auditory_synonym_judgments
auditory_synonym_judgments_mids <- subset(complete_data_long, assessment == "auditory_synonym_judgments")
auditory_synonym_judgments_mids <- as.mids(auditory_synonym_judgments_mids)

# Subset data for written_synonym_judgments
written_synonym_judgments_mids <- subset(complete_data_long, assessment == "written_synonym_judgments")
written_synonym_judgments_mids <- as.mids(written_synonym_judgments_mids)

# Subset data for word_semantic_association
word_semantic_association_mids <- subset(complete_data_long, assessment == "word_semantic_association")
word_semantic_association_mids <- as.mids(word_semantic_association_mids)

# Subset data for production_composite
production_composite_mids <- subset(complete_data_long, assessment == "production_composite")
production_composite_mids <- as.mids(production_composite_mids)


############################################################################
############################################################################
###                                                                      ###
###                              RUN MODELS                              ###
###                                                                      ###
############################################################################
############################################################################


##################################################################
##                            WAB-AQ                            ##
##################################################################
mipo_list <- list()

## TRAINED


# Fit the model
mod_wab_trained <- with(data = combined_mids_all_modified, exp = lmer(wab_aq_trained ~ timepoint + (1|id)))

# Pool the model results
mod_wab_trained_pooled <- pool(mod_wab_trained)
mipo_list[["mod_wab_trained"]] <- mod_wab_trained_pooled

# Summarize the pooled results
summary(mod_wab_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_wab_trained <- lmer(wab_aq_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_wab_trained <- residuals(model_single_wab_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_wab_trained <- shapiro.test(residuals_single_wab_trained)
print(shapiro_test_wab_trained)

# QQ plot of residuals using qqnorm and qqline
png("wab_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_wab_trained)
qqline(residuals_single_wab_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_wab_trained))
qqline(residuals_single_wab_trained, col = "steelblue")

# Print results of Shapiro-Wilk test to console
print(shapiro_test_wab_trained)


## UNTRAINED


# Fit the model
mod_wab_untrained <- with(data = combined_mids_all_modified, exp = lmer(wab_aq_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_wab_untrained_pooled <- pool(mod_wab_untrained)
mipo_list[["mod_wab_untrained"]] <- mod_wab_untrained_pooled

# Summarize the pooled results
summary(mod_wab_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_wab_untrained <- lmer(wab_aq_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_wab_untrained <- residuals(model_single_wab_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_wab_untrained <- shapiro.test(residuals_single_wab_untrained)
print(shapiro_test_wab_untrained)

# QQ plot of residuals using qqnorm and qqline
png("wab_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_wab_untrained)
qqline(residuals_single_wab_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_wab_untrained))
qqline(residuals_single_wab_untrained, col = "steelblue")

#################################################################
##                             BNT                             ##
#################################################################

## TRAINED

# Fit the model
mod_bnt_trained <- with(data = combined_mids_all_modified, exp = lmer(bnt_trained ~ timepoint + (1|id)))

# Pool the model results
mod_bnt_trained_pooled <- pool(mod_bnt_trained)
mipo_list[["mod_bnt_trained"]] <- mod_bnt_trained_pooled
# Summarize the pooled results
summary(mod_bnt_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_bnt_trained <- lmer(bnt_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_bnt_trained <- residuals(model_single_bnt_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_bnt_trained <- shapiro.test(residuals_single_bnt_trained)
print(shapiro_test_bnt_trained)

# QQ plot of residuals using qqnorm and qqline
png("bnt_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_bnt_trained)
qqline(residuals_single_bnt_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_bnt_trained))
qqline(residuals_single_bnt_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_bnt_untrained <- with(data = combined_mids_all_modified, exp = lmer(bnt_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_bnt_untrained_pooled <- pool(mod_bnt_untrained)
mipo_list[["mod_bnt_untrained"]] <- mod_bnt_untrained_pooled
# Summarize the pooled results
summary(mod_bnt_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_bnt_untrained <- lmer(bnt_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_bnt_untrained <- residuals(model_single_bnt_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_bnt_untrained <- shapiro.test(residuals_single_bnt_untrained)
print(shapiro_test_bnt_untrained)

# QQ plot of residuals using qqnorm and qqline
png("bnt_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_bnt_untrained)
qqline(residuals_single_bnt_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_bnt_untrained))
qqline(residuals_single_bnt_untrained, col = "steelblue")


##################################################################
##                    BAT SEMANTIC COMPOSITE                    ##
##################################################################

## TRAINED

# Fit the model
mod_semantic_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(semantic_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_semantic_composite_trained_pooled <- pool(mod_semantic_composite_trained)
mipo_list[["mod_semantic_composite_trained"]] <- mod_semantic_composite_trained_pooled

# Summarize the pooled results
summary(mod_semantic_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_semantic_composite_trained <- lmer(semantic_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_semantic_composite_trained <- residuals(model_single_semantic_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_semantic_composite_trained <- shapiro.test(residuals_single_semantic_composite_trained)
print(shapiro_test_semantic_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("semantic_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_semantic_composite_trained)
qqline(residuals_single_semantic_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_semantic_composite_trained))
qqline(residuals_single_semantic_composite_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_semantic_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(semantic_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_semantic_composite_untrained_pooled <- pool(mod_semantic_composite_untrained)
mipo_list[["mod_semantic_composite_untrained"]] <- mod_semantic_composite_untrained_pooled

# Summarize the pooled results
summary(mod_semantic_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_semantic_composite_untrained <- lmer(semantic_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_semantic_composite_untrained <- residuals(model_single_semantic_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_semantic_composite_untrained <- shapiro.test(residuals_single_semantic_composite_untrained)
print(shapiro_test_semantic_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("semantic_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_semantic_composite_untrained)
qqline(residuals_single_semantic_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_semantic_composite_untrained))
qqline(residuals_single_semantic_composite_untrained, col = "steelblue")



#################################################################
##                 BAT COMPREHENSION COMPOSITE                 ##
#################################################################


## TRAINED

# Fit the model
mod_comprehension_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(comprehension_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_comprehension_composite_trained_pooled <- pool(mod_comprehension_composite_trained)
mipo_list[["mod_comprehension_composite_trained"]] <- mod_comprehension_composite_trained_pooled

# Summarize the pooled results
summary(mod_comprehension_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_comprehension_composite_trained <- lmer(comprehension_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_comprehension_composite_trained <- residuals(model_single_comprehension_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_comprehension_composite_trained <- shapiro.test(residuals_single_comprehension_composite_trained)
print(shapiro_test_comprehension_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("comprehension_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_comprehension_composite_trained)
qqline(residuals_single_comprehension_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_comprehension_composite_trained))
qqline(residuals_single_comprehension_composite_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_comprehension_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(comprehension_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_comprehension_composite_untrained_pooled <- pool(mod_comprehension_composite_untrained)
mipo_list[["mod_comprehension_composite_untrained"]] <- mod_comprehension_composite_untrained_pooled

# Summarize the pooled results
summary(mod_comprehension_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_comprehension_composite_untrained <- lmer(comprehension_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_comprehension_composite_untrained <- residuals(model_single_comprehension_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_comprehension_composite_untrained <- shapiro.test(residuals_single_comprehension_composite_untrained)
print(shapiro_test_comprehension_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("comprehension_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_comprehension_composite_untrained)
qqline(residuals_single_comprehension_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_comprehension_composite_untrained))
qqline(residuals_single_comprehension_composite_untrained, col = "steelblue")


##################################################################
##                    BAT JUDGMENT COMPOSITE                    ##
##################################################################

## TRAINED

# Fit the model
mod_judgment_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(judgment_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_judgment_composite_trained_pooled <- pool(mod_judgment_composite_trained)
mipo_list[["mod_judgment_composite_trained"]] <- mod_judgment_composite_trained_pooled

# Summarize the pooled results
summary(mod_judgment_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_judgment_composite_trained <- lmer(judgment_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_judgment_composite_trained <- residuals(model_single_judgment_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_judgment_composite_trained <- shapiro.test(residuals_single_judgment_composite_trained)
print(shapiro_test_judgment_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("judgment_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_judgment_composite_trained)
qqline(residuals_single_judgment_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_judgment_composite_trained))
qqline(residuals_single_judgment_composite_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_judgment_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(judgment_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_judgment_composite_untrained_pooled <- pool(mod_judgment_composite_untrained)
mipo_list[["mod_judgment_composite_untrained"]] <- mod_judgment_composite_untrained_pooled

# Summarize the pooled results
summary(mod_judgment_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_judgment_composite_untrained <- lmer(judgment_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_judgment_composite_untrained <- residuals(model_single_judgment_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_judgment_composite_untrained <- shapiro.test(residuals_single_judgment_composite_untrained)
print(shapiro_test_judgment_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("judgment_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_judgment_composite_untrained)
qqline(residuals_single_judgment_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_judgment_composite_untrained))
qqline(residuals_single_judgment_composite_untrained, col = "steelblue")
##################################################################
##                   BAT PRODUCTION COMPOSITE                   ##
##################################################################
## TRAINED

# Fit the model
mod_production_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(production_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_production_composite_trained_pooled <- pool(mod_production_composite_trained)
mipo_list[["mod_production_composite_trained"]] <- mod_production_composite_trained_pooled

# Summarize the pooled results
summary(mod_production_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_production_composite_trained <- lmer(production_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_production_composite_trained <- residuals(model_single_production_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_production_composite_trained <- shapiro.test(residuals_single_production_composite_trained)
print(shapiro_test_production_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("production_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_production_composite_trained)
qqline(residuals_single_production_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_production_composite_trained))
qqline(residuals_single_production_composite_trained, col = "steelblue")

## UNTRAINED
# Fit the model
mod_production_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(production_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_production_composite_untrained_pooled <- pool(mod_production_composite_untrained)
mipo_list[["mod_production_composite_untrained"]] <- mod_production_composite_untrained_pooled

# Summarize the pooled results
summary(mod_production_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_production_composite_untrained <- lmer(production_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_production_composite_untrained <- residuals(model_single_production_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_production_composite_untrained <- shapiro.test(residuals_single_production_composite_untrained)
print(shapiro_test_production_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("production_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_production_composite_untrained)
qqline(residuals_single_production_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_production_composite_untrained))
qqline(residuals_single_production_composite_untrained, col = "steelblue")

##################################################################
##                   BAT REPETITION COMPOSITE                   ##
##################################################################

## TRAINED

# Fit the model
mod_repetition_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(repetition_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_repetition_composite_trained_pooled <- pool(mod_repetition_composite_trained)
mipo_list[["mod_repetition_composite_trained"]] <- mod_repetition_composite_trained_pooled

# Summarize the pooled results
summary(mod_repetition_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_repetition_composite_trained <- lmer(repetition_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_repetition_composite_trained <- residuals(model_single_repetition_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_repetition_composite_trained <- shapiro.test(residuals_single_repetition_composite_trained)
print(shapiro_test_repetition_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("repetition_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_repetition_composite_trained)
qqline(residuals_single_repetition_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_repetition_composite_trained))
qqline(residuals_single_repetition_composite_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_repetition_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(repetition_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_repetition_composite_untrained_pooled <- pool(mod_repetition_composite_untrained)
mipo_list[["mod_repetition_composite_untrained"]] <- mod_repetition_composite_untrained_pooled

# Summarize the pooled results
summary(mod_repetition_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_repetition_composite_untrained <- lmer(repetition_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_repetition_composite_untrained <- residuals(model_single_repetition_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_repetition_composite_untrained <- shapiro.test(residuals_single_repetition_composite_untrained)
print(shapiro_test_repetition_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("repetition_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_repetition_composite_untrained)
qqline(residuals_single_repetition_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_repetition_composite_untrained))
qqline(residuals_single_repetition_composite_untrained, col = "steelblue")



#################################################################
##                    BAT READING COMPOSITE                    ##
#################################################################
## TRAINED

# Fit the model
mod_reading_comp_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(reading_comp_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_reading_comp_composite_trained_pooled <- pool(mod_reading_comp_composite_trained)
mipo_list[["mod_reading_comp_composite_trained"]] <- mod_reading_comp_composite_trained_pooled

# Summarize the pooled results
summary(mod_reading_comp_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_reading_comp_composite_trained <- lmer(reading_comp_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_reading_comp_composite_trained <- residuals(model_single_reading_comp_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_reading_comp_composite_trained <- shapiro.test(residuals_single_reading_comp_composite_trained)
print(shapiro_test_reading_comp_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("reading_comp_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_reading_comp_composite_trained)
qqline(residuals_single_reading_comp_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_reading_comp_composite_trained))
qqline(residuals_single_reading_comp_composite_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_reading_comp_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(reading_comp_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_reading_comp_composite_untrained_pooled <- pool(mod_reading_comp_composite_untrained)
mipo_list[["mod_reading_comp_composite_untrained"]] <- mod_reading_comp_composite_untrained_pooled

# Summarize the pooled results
summary(mod_reading_comp_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_reading_comp_composite_untrained <- lmer(reading_comp_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_reading_comp_composite_untrained <- residuals(model_single_reading_comp_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_reading_comp_composite_untrained <- shapiro.test(residuals_single_reading_comp_composite_untrained)
print(shapiro_test_reading_comp_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("reading_comp_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_reading_comp_composite_untrained)
qqline(residuals_single_reading_comp_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_reading_comp_composite_untrained))
qqline(residuals_single_reading_comp_composite_untrained, col = "steelblue")


#################################################################
##                    BAT WRITING COMPOSITE                    ##
#################################################################

## TRAINED

# Fit the model
mod_writing_composite_trained <- with(data = combined_mids_all_modified, exp = lmer(writing_composite_trained ~ timepoint + (1|id)))

# Pool the model results
mod_writing_composite_trained_pooled <- pool(mod_writing_composite_trained)
mipo_list[["mod_writing_composite_trained"]] <- mod_writing_composite_trained_pooled

# Summarize the pooled results
summary(mod_writing_composite_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_writing_composite_trained <- lmer(writing_composite_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_writing_composite_trained <- residuals(model_single_writing_composite_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_writing_composite_trained <- shapiro.test(residuals_single_writing_composite_trained)
print(shapiro_test_writing_composite_trained)

# QQ plot of residuals using qqnorm and qqline
png("writing_composite_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_writing_composite_trained)
qqline(residuals_single_writing_composite_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_writing_composite_trained))
qqline(residuals_single_writing_composite_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_writing_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(writing_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_writing_composite_untrained_pooled <- pool(mod_writing_composite_untrained)
mipo_list[["mod_writing_composite_untrained"]] <- mod_writing_composite_untrained_pooled

# Summarize the pooled results
summary(mod_writing_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_writing_composite_untrained <- lmer(writing_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_writing_composite_untrained <- residuals(model_single_writing_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_writing_composite_untrained <- shapiro.test(residuals_single_writing_composite_untrained)
print(shapiro_test_writing_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("writing_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_writing_composite_untrained)
qqline(residuals_single_writing_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_writing_composite_untrained))
qqline(residuals_single_writing_composite_untrained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_morphology_composite_untrained <- with(data = combined_mids_all_modified, exp = lmer(morphology_composite_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_morphology_composite_untrained_pooled <- pool(mod_morphology_composite_untrained)
mipo_list[["mod_morphology_composite_untrained"]] <- mod_morphology_composite_untrained_pooled

# Summarize the pooled results
summary(mod_morphology_composite_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_morphology_composite_untrained <- lmer(morphology_composite_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_morphology_composite_untrained <- residuals(model_single_morphology_composite_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_morphology_composite_untrained <- shapiro.test(residuals_single_morphology_composite_untrained)
print(shapiro_test_morphology_composite_untrained)

# QQ plot of residuals using qqnorm and qqline
png("morphology_composite_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_morphology_composite_untrained)
qqline(residuals_single_morphology_composite_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_morphology_composite_untrained))
qqline(residuals_single_morphology_composite_untrained, col = "steelblue")



#################################################################
##           PALPA 29/EPLA 28: letter-length reading           ##
#################################################################
## TRAINED

# Fit the model
mod_letter_length_reading_trained <- with(data = combined_mids_all_modified, exp = lmer(letter_length_reading_trained ~ timepoint + (1|id)))

# Pool the model results
mod_letter_length_reading_trained_pooled <- pool(mod_letter_length_reading_trained)
mipo_list[["mod_letter_length_reading_trained"]] <- mod_letter_length_reading_trained_pooled

# Summarize the pooled results
summary(mod_letter_length_reading_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_letter_length_reading_trained <- lmer(letter_length_reading_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_letter_length_reading_trained <- residuals(model_single_letter_length_reading_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_letter_length_reading_trained <- shapiro.test(residuals_single_letter_length_reading_trained)
print(shapiro_test_letter_length_reading_trained)

# QQ plot of residuals using qqnorm and qqline
png("letter_length_reading_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_letter_length_reading_trained)
qqline(residuals_single_letter_length_reading_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_letter_length_reading_trained))
qqline(residuals_single_letter_length_reading_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_letter_length_reading_untrained <- with(data = combined_mids_all_modified, exp = lmer(letter_length_reading_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_letter_length_reading_untrained_pooled <- pool(mod_letter_length_reading_untrained)
mipo_list[["mod_letter_length_reading_untrained"]] <- mod_letter_length_reading_untrained_pooled

# Summarize the pooled results
summary(mod_letter_length_reading_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_letter_length_reading_untrained <- lmer(letter_length_reading_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_letter_length_reading_untrained <- residuals(model_single_letter_length_reading_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_letter_length_reading_untrained <- shapiro.test(residuals_single_letter_length_reading_untrained)
print(shapiro_test_letter_length_reading_untrained)

# QQ plot of residuals using qqnorm and qqline
png("letter_length_reading_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_letter_length_reading_untrained)
qqline(residuals_single_letter_length_reading_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_letter_length_reading_untrained))
qqline(residuals_single_letter_length_reading_untrained, col = "steelblue")

##################################################################
##        PALPA 47/EPLA 45: spoken word-picture matching        ##
##################################################################

## TRAINED

# Fit the model
mod_spoken_word_picture_matching_trained <- with(data = combined_mids_all_modified, exp = lmer(spoken_word_picture_matching_trained ~ timepoint + (1|id)))

# Pool the model results
mod_spoken_word_picture_matching_trained_pooled <- pool(mod_spoken_word_picture_matching_trained)
mipo_list[["mod_spoken_word_picture_matching_trained"]] <- mod_spoken_word_picture_matching_trained_pooled

# Summarize the pooled results
summary(mod_spoken_word_picture_matching_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_spoken_word_picture_matching_trained <- lmer(spoken_word_picture_matching_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_spoken_word_picture_matching_trained <- residuals(model_single_spoken_word_picture_matching_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_spoken_word_picture_matching_trained <- shapiro.test(residuals_single_spoken_word_picture_matching_trained)
print(shapiro_test_spoken_word_picture_matching_trained)

# QQ plot of residuals using qqnorm and qqline
png("spoken_word_picture_matching_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_spoken_word_picture_matching_trained)
qqline(residuals_single_spoken_word_picture_matching_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_spoken_word_picture_matching_trained))
qqline(residuals_single_spoken_word_picture_matching_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_spoken_word_picture_matching_untrained <- with(data = combined_mids_all_modified, exp = lmer(spoken_word_picture_matching_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_spoken_word_picture_matching_untrained_pooled <- pool(mod_spoken_word_picture_matching_untrained)
mipo_list[["mod_spoken_word_picture_matching_untrained"]] <- mod_spoken_word_picture_matching_untrained_pooled

# Summarize the pooled results
summary(mod_spoken_word_picture_matching_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_spoken_word_picture_matching_untrained <- lmer(spoken_word_picture_matching_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_spoken_word_picture_matching_untrained <- residuals(model_single_spoken_word_picture_matching_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_spoken_word_picture_matching_untrained <- shapiro.test(residuals_single_spoken_word_picture_matching_untrained)
print(shapiro_test_spoken_word_picture_matching_untrained)

# QQ plot of residuals using qqnorm and qqline
png("spoken_word_picture_matching_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_spoken_word_picture_matching_untrained)
qqline(residuals_single_spoken_word_picture_matching_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_spoken_word_picture_matching_untrained))
qqline(residuals_single_spoken_word_picture_matching_untrained, col = "steelblue")


##################################################################
##       PALPA 48/ EPLA 46: written word-picture matching       ##
##################################################################

## TRAINED

# Fit the model
mod_written_word_picture_matching_trained <- with(data = combined_mids_all_modified, exp = lmer(written_word_picture_matching_trained ~ timepoint + (1|id)))

# Pool the model results
mod_written_word_picture_matching_trained_pooled <- pool(mod_written_word_picture_matching_trained)
mipo_list[["mod_written_word_picture_matching_trained"]] <- mod_written_word_picture_matching_trained_pooled

# Summarize the pooled results
summary(mod_written_word_picture_matching_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_written_word_picture_matching_trained <- lmer(written_word_picture_matching_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_written_word_picture_matching_trained <- residuals(model_single_written_word_picture_matching_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_written_word_picture_matching_trained <- shapiro.test(residuals_single_written_word_picture_matching_trained)
print(shapiro_test_written_word_picture_matching_trained)

# QQ plot of residuals using qqnorm and qqline
png("written_word_picture_matching_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_written_word_picture_matching_trained)
qqline(residuals_single_written_word_picture_matching_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_written_word_picture_matching_trained))
qqline(residuals_single_written_word_picture_matching_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_written_word_picture_matching_untrained <- with(data = combined_mids_all_modified, exp = lmer(written_word_picture_matching_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_written_word_picture_matching_untrained_pooled <- pool(mod_written_word_picture_matching_untrained)
mipo_list[["mod_written_word_picture_matching_untrained"]] <- mod_written_word_picture_matching_untrained_pooled

# Summarize the pooled results
summary(mod_written_word_picture_matching_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_written_word_picture_matching_untrained <- lmer(written_word_picture_matching_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_written_word_picture_matching_untrained <- residuals(model_single_written_word_picture_matching_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_written_word_picture_matching_untrained <- shapiro.test(residuals_single_written_word_picture_matching_untrained)
print(shapiro_test_written_word_picture_matching_untrained)

# QQ plot of residuals using qqnorm and qqline
png("written_word_picture_matching_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_written_word_picture_matching_untrained)
qqline(residuals_single_written_word_picture_matching_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_written_word_picture_matching_untrained))
qqline(residuals_single_written_word_picture_matching_untrained, col = "steelblue")


##################################################################
##         PALPA 49/EPLA 47: auditory synonym judgments         ##
##################################################################

## TRAINED

# Fit the model
mod_auditory_synonym_judgments_trained <- with(data = combined_mids_all_modified, exp = lmer(auditory_synonym_judgments_trained ~ timepoint + (1|id)))

# Pool the model results
mod_auditory_synonym_judgments_trained_pooled <- pool(mod_auditory_synonym_judgments_trained)
mipo_list[["mod_auditory_synonym_judgments_trained"]] <- mod_auditory_synonym_judgments_trained_pooled

# Summarize the pooled results
summary(mod_auditory_synonym_judgments_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_auditory_synonym_judgments_trained <- lmer(auditory_synonym_judgments_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_auditory_synonym_judgments_trained <- residuals(model_single_auditory_synonym_judgments_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_auditory_synonym_judgments_trained <- shapiro.test(residuals_single_auditory_synonym_judgments_trained)
print(shapiro_test_auditory_synonym_judgments_trained)

# QQ plot of residuals using qqnorm and qqline
png("auditory_synonym_judgments_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_auditory_synonym_judgments_trained)
qqline(residuals_single_auditory_synonym_judgments_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_auditory_synonym_judgments_trained))
qqline(residuals_single_auditory_synonym_judgments_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_auditory_synonym_judgments_untrained <- with(data = combined_mids_all_modified, exp = lmer(auditory_synonym_judgments_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_auditory_synonym_judgments_untrained_pooled <- pool(mod_auditory_synonym_judgments_untrained)
mipo_list[["mod_auditory_synonym_judgments_untrained"]] <- mod_auditory_synonym_judgments_untrained_pooled

# Summarize the pooled results
summary(mod_auditory_synonym_judgments_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_auditory_synonym_judgments_untrained <- lmer(auditory_synonym_judgments_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_auditory_synonym_judgments_untrained <- residuals(model_single_auditory_synonym_judgments_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_auditory_synonym_judgments_untrained <- shapiro.test(residuals_single_auditory_synonym_judgments_untrained)
print(shapiro_test_auditory_synonym_judgments_untrained)

# QQ plot of residuals using qqnorm and qqline
png("auditory_synonym_judgments_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_auditory_synonym_judgments_untrained)
qqline(residuals_single_auditory_synonym_judgments_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_auditory_synonym_judgments_untrained))
qqline(residuals_single_auditory_synonym_judgments_untrained, col = "steelblue")

##################################################################
##         PALPA 50/ EPLA 48: written synonym judgments         ##
##################################################################
## TRAINED

# Fit the model
mod_written_synonym_judgments_trained <- with(data = combined_mids_all_modified, exp = lmer(written_synonym_judgments_trained ~ timepoint + (1|id)))

# Pool the model results
mod_written_synonym_judgments_trained_pooled <- pool(mod_written_synonym_judgments_trained)
mipo_list[["mod_written_synonym_judgments_trained"]] <- mod_written_synonym_judgments_trained_pooled

# Summarize the pooled results
summary(mod_written_synonym_judgments_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_written_synonym_judgments_trained <- lmer(written_synonym_judgments_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_written_synonym_judgments_trained <- residuals(model_single_written_synonym_judgments_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_written_synonym_judgments_trained <- shapiro.test(residuals_single_written_synonym_judgments_trained)
print(shapiro_test_written_synonym_judgments_trained)

# QQ plot of residuals using qqnorm and qqline
png("written_synonym_judgments_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_written_synonym_judgments_trained)
qqline(residuals_single_written_synonym_judgments_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_written_synonym_judgments_trained))
qqline(residuals_single_written_synonym_judgments_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_written_synonym_judgments_untrained <- with(data = combined_mids_all_modified, exp = lmer(written_synonym_judgments_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_written_synonym_judgments_untrained_pooled <- pool(mod_written_synonym_judgments_untrained)
mipo_list[["mod_written_synonym_judgments_untrained"]] <- mod_written_synonym_judgments_untrained_pooled

# Summarize the pooled results
summary(mod_written_synonym_judgments_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_written_synonym_judgments_untrained <- lmer(written_synonym_judgments_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_written_synonym_judgments_untrained <- residuals(model_single_written_synonym_judgments_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_written_synonym_judgments_untrained <- shapiro.test(residuals_single_written_synonym_judgments_untrained)
print(shapiro_test_written_synonym_judgments_untrained)

# QQ plot of residuals using qqnorm and qqline
png("written_synonym_judgments_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_written_synonym_judgments_untrained)
qqline(residuals_single_written_synonym_judgments_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_written_synonym_judgments_untrained))
qqline(residuals_single_written_synonym_judgments_untrained, col = "steelblue")

#################################################################
##         PALPA 51/EPLA 49: word semantic association         ##
#################################################################

## TRAINED

# Fit the model
mod_word_semantic_association_trained <- with(data = combined_mids_all_modified, exp = lmer(word_semantic_association_trained ~ timepoint + (1|id)))

# Pool the model results
mod_word_semantic_association_trained_pooled <- pool(mod_word_semantic_association_trained)
mipo_list[["mod_word_semantic_association_trained"]] <- mod_word_semantic_association_trained_pooled

# Summarize the pooled results
summary(mod_word_semantic_association_trained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_word_semantic_association_trained <- lmer(word_semantic_association_trained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_word_semantic_association_trained <- residuals(model_single_word_semantic_association_trained)

# Perform Shapiro-Wilk test for normality
shapiro_test_word_semantic_association_trained <- shapiro.test(residuals_single_word_semantic_association_trained)
print(shapiro_test_word_semantic_association_trained)

# QQ plot of residuals using qqnorm and qqline
png("word_semantic_association_trained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_word_semantic_association_trained)
qqline(residuals_single_word_semantic_association_trained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_word_semantic_association_trained))
qqline(residuals_single_word_semantic_association_trained, col = "steelblue")


## UNTRAINED

# Fit the model
mod_word_semantic_association_untrained <- with(data = combined_mids_all_modified, exp = lmer(word_semantic_association_untrained ~ timepoint + (1|id)))

# Pool the model results
mod_word_semantic_association_untrained_pooled <- pool(mod_word_semantic_association_untrained)
mipo_list[["mod_word_semantic_association_untrained"]] <- mod_word_semantic_association_untrained_pooled

# Summarize the pooled results
summary(mod_word_semantic_association_untrained_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_word_semantic_association_untrained <- lmer(word_semantic_association_untrained ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_word_semantic_association_untrained <- residuals(model_single_word_semantic_association_untrained)

# Perform Shapiro-Wilk test for normality
shapiro_test_word_semantic_association_untrained <- shapiro.test(residuals_single_word_semantic_association_untrained)
print(shapiro_test_word_semantic_association_untrained)

# QQ plot of residuals using qqnorm and qqline
png("word_semantic_association_untrained_qqnorm.png") # Start PNG device
qqnorm(residuals_single_word_semantic_association_untrained)
qqline(residuals_single_word_semantic_association_untrained)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_word_semantic_association_untrained))
qqline(residuals_single_word_semantic_association_untrained, col = "steelblue")


##################################################################
##                             PAPT                             ##
##################################################################

# Fit the model
mod_papt_total <- with(data = combined_mids_all_modified, exp = lmer(papt_total ~ timepoint + (1|id)))

# Pool the model results
mod_papt_total_pooled <- pool(mod_papt_total)
mipo_list[["mod_papt_total"]] <- mod_papt_total_pooled

# Summarize the pooled results
summary(mod_papt_total_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_papt_total <- lmer(papt_total ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_papt_total <- residuals(model_single_papt_total)

# Perform Shapiro-Wilk test for normality
shapiro_test_papt_total <- shapiro.test(residuals_single_papt_total)
print(shapiro_test_papt_total)

# QQ plot of residuals using qqnorm and qqline
png("papt_total_qqnorm.png") # Start PNG device
qqnorm(residuals_single_papt_total)
qqline(residuals_single_papt_total)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_papt_total))
qqline(residuals_single_papt_total, col = "steelblue")


##################################################################
##                            RAVENS                            ##
##################################################################

# Fit the model
mod_ravens <- with(data = combined_mids_all_modified, exp = lmer(ravens ~ timepoint + (1|id)))

# Pool the model results
mod_ravens_pooled <- pool(mod_ravens)
mipo_list[["mod_ravens"]] <- mod_ravens_pooled

# Summarize the pooled results
summary(mod_ravens_pooled)

# Confirm normality of residuals
# Fit the model to this single imputed dataset
model_single_ravens <- lmer(ravens ~ timepoint + (1|id), data = data_single)

# Extract residuals
residuals_single_ravens <- residuals(model_single_ravens)

# Perform Shapiro-Wilk test for normality
shapiro_test_ravens <- shapiro.test(residuals_single_ravens)
print(shapiro_test_ravens)

# QQ plot of residuals using qqnorm and qqline
png("ravens_qqnorm.png") # Start PNG device
qqnorm(residuals_single_ravens)
qqline(residuals_single_ravens)
dev.off() # Close PNG device

plot(qqnorm(residuals_single_ravens))
qqline(residuals_single_ravens, col = "steelblue")




## COMBINE RESULTS INTO CSV
# Combine the tidy data frames from each mipo object into one, with an identifier
model_results <- lapply(names(mipo_list), function(name) {
  model_df <- tidy(mipo_list[[name]])
  model_df <- model_df %>%
    mutate(model = name) # Add a column with the model name
})

combined_results <- bind_rows(model_results)





outcome_vars <- c(
  "papt_total",
  "ravens",
  "naming_screener_trained",
  "naming_screener_untrained",
  "wab_aq_trained",
  "wab_aq_untrained",
  "bnt_trained",
  "bnt_untrained",
  "semantic_composite_trained",
  "semantic_composite_untrained",
  "comprehension_composite_trained",
  "comprehension_composite_untrained",
  "judgment_composite_trained",
  "judgment_composite_untrained",
  "repetition_composite_trained",
  "repetition_composite_untrained",
  "reading_comp_composite_trained",
  "reading_comp_composite_untrained",
  "writing_composite_trained",
  "writing_composite_untrained",
  "morphology_composite_trained",
  "morphology_composite_untrained",
  "translation_composite_trained",
  "translation_composite_untrained",
  "letter_length_reading_trained", 
  "letter_length_reading_untrained", 
  "spoken_word_picture_matching_trained", 
  "spoken_word_picture_matching_untrained", 
  "written_word_picture_matching_trained", 
  "written_word_picture_matching_untrained", 
  "auditory_synonym_judgments_trained", 
  "auditory_synonym_judgments_untrained", 
  "written_synonym_judgments_trained", 
  "written_synonym_judgments_untrained", 
  "word_semantic_association_trained", 
  "word_semantic_association_untrained"
)



# Look at correlations
# Filter for Pre-Tx timepoint
pre_tx_data <- subset(hi, timepoint == 'Pre-Tx')

# Identify columns that end with "_trained"
full_trained_columns <- grep("_trained$", names(pre_tx_data), value = TRUE)

# Calculate the correlation matrix for FULL _trained assessments
correlation_matrix <- cor(pre_tx_data[,full_trained_columns], use = "complete.obs")

# Find highly correlated pairs
high_cor <- which(abs(correlation_matrix) > 0.7 & abs(correlation_matrix) < 1, arr.ind = TRUE)

# Extract the names of the assessments and the correlation values
high_cor_pairs <- data.frame(
  Assessment1 = rownames(correlation_matrix)[high_cor[,1]],
  Assessment2 = colnames(correlation_matrix)[high_cor[,2]],
  Correlation = correlation_matrix[high_cor]
)

# Ensure unique pairs
high_cor_pairs <- high_cor_pairs[!duplicated(t(apply(high_cor_pairs, 1, sort))),]

print(high_cor_pairs)

write.csv(high_cor_pairs, "/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/FInal Take Feb 2024/Transfer Analysis Final Take 2024/correlatedpairs_pre-tx.csv" )



###########################################################################
###########################################################################
###                                                                     ###
###                            NAMING PROBES                            ###
###                                                                     ###
###########################################################################
###########################################################################
tx_probes <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/FInal Take Feb 2024/Transfer Analysis Final Take 2024/tx_probes_prepost.csv")
tx_probes$timepoint <- as.factor(tx_probes$timepoint)
tx_probes$timepoint <- relevel(tx_probes$timepoint, ref = "Pre-Tx")
tx_probes_set1_trained <- tx_probes %>% filter(set == "1", trained_untrained == "trained" )
tx_probes_set2_trained <- tx_probes %>% filter(set == "2", trained_untrained == "trained" )
tx_probes_set3_trained <- tx_probes %>% filter(set == "3", trained_untrained == "trained" )
tx_probes_set1_untrained <- tx_probes %>% filter(set == "1", trained_untrained == "untrained")
tx_probes_set2_untrained <- tx_probes %>% filter(set == "2", trained_untrained == "untrained")
tx_probes_set3_untrained <- tx_probes %>% filter(set == "3", trained_untrained == "untrained")

#################################################################
##                        NAMING MODELS                        ##
#################################################################
# SET 1 TRAINED
mod_set1_trained <- lmer(perc_acc ~ timepoint + (1|id), data = tx_probes_set1_trained)
summary(mod_set1_trained)

# SET 2 TRAINED
mod_set2_trained <- lmer(perc_acc ~ timepoint + (1|id), data = tx_probes_set2_trained)
summary(mod_set2_trained)

# SET 3 TRAINED
mod_set3_trained <- lmer(perc_acc ~ timepoint + (1|id), data = tx_probes_set3_trained)
summary(mod_set3_trained)

# SET 1 UNTRAINED
mod_set1_untrained <- lmer(perc_acc ~ timepoint + (1|id), data = tx_probes_set1_untrained)
summary(mod_set1_untrained)

# SET 2 UNTRAINED
mod_set2_untrained <- lmer(perc_acc ~ timepoint + (1|id), data = tx_probes_set2_untrained)
summary(mod_set2_untrained)

# SET 3 UNTRAINED
mod_set3_untrained <- lmer(perc_acc ~ timepoint + (1|id), data = tx_probes_set3_untrained)
summary(mod_set3_untrained)

##################################################################
##                    WAB SPONTANEOUS SPEECH                    ##
##################################################################

wab_ss <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/FInal Take Feb 2024/Transfer Analysis Final Take 2024/wab_ss.csv", stringsAsFactors =  TRUE)
wab_ss$timepoint <- relevel(wab_ss$timepoint, ref = "Pre-Tx")

mod_wab_ss_trained <- lmer(wab_ss_total_trained ~ timepoint + (1|id), data = wab_ss)
summary(mod_wab_ss_trained)

mod_wab_ss_untrained <- lmer(wab_ss_total_untrained ~ timepoint + (1|id), data = wab_ss)
summary(mod_wab_ss_untrained)


##################################################################
##               CORRECT FOR MULTIPLE COMPARISONS (OLD, don't use)##
##################################################################
model_p_values <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/FInal Take Feb 2024/Transfer Analysis Final Take 2024/model_p_values.csv", stringsAsFactors =  TRUE)
model_p_values$bonf <- p.adjust(model_p_values$p.value, method = "bonferroni")
model_p_values$bh <- p.adjust(model_p_values$p.value, method = "BH")
model_p_values$holms <- p.adjust(model_p_values$p.value, method = "holm")



###########################################################################
###########################################################################
###                                                                     ###
###                            VISUALIZATION                            ###
###                                                                     ###
###########################################################################
###########################################################################
library(ggmice)

## Prepare observed (unimputed) dataset

adjust_and_populate <- function(dataset) {
  specific_assessments <- list(
    letter_length_reading = list(palpa = "palpa29_eng", epla = "epla28_span"),
    spoken_word_picture_matching = list(palpa = "palpa47_eng", epla = "epla45_span"),
    written_word_picture_matching = list(palpa = "palpa48_eng", epla = "epla46_span"),
    auditory_synonym_judgments = list(palpa = "palpa49_eng", epla = "epla47_span"),
    written_synonym_judgments = list(palpa = "palpa50_eng", epla = "epla48__span"),
    word_semantic_association = list(palpa = "palpa51_eng", epla = "epla49__span")
  )
  
  # Process specific assessments for trained and untrained columns
  for(assessment_name in names(specific_assessments)) {
    palpa_col <- specific_assessments[[assessment_name]]$palpa
    epla_col <- specific_assessments[[assessment_name]]$epla
    
    dataset <- dataset %>%
      mutate(!!sym(str_c(assessment_name, "_trained")) := case_when(
        tx_lang == "English" ~ !!sym(palpa_col),
        tx_lang == "Spanish" ~ !!sym(epla_col),
        TRUE ~ NA_real_
      ),
      !!sym(str_c(assessment_name, "_untrained")) := case_when(
        tx_lang == "English" ~ !!sym(epla_col),
        tx_lang == "Spanish" ~ !!sym(palpa_col),
        TRUE ~ NA_real_
      ))
  }
  
  # Dynamically process other assessments for trained and untrained columns
  eng_columns <- names(dataset)[grepl("_eng$", names(dataset))]
  span_columns <- names(dataset)[grepl("_span$", names(dataset))]
  
  specific_assessments_flat <- unlist(specific_assessments)
  
  for(col in setdiff(eng_columns, specific_assessments_flat)) {
    col_base <- sub("_eng$", "", col)
    col_span <- paste0(col_base, "_span")
    
    if (col_span %in% span_columns) {
      dataset <- dataset %>%
        mutate(!!sym(str_c(col_base, "_trained")) := case_when(
          tx_lang == "English" ~ !!sym(col),
          tx_lang == "Spanish" ~ !!sym(col_span),
          TRUE ~ NA_real_
        ),
        !!sym(str_c(col_base, "_untrained")) := case_when(
          tx_lang == "English" ~ !!sym(col_span),
          tx_lang == "Spanish" ~ !!sym(col),
          TRUE ~ NA_real_
        ))
    }
  }
  
  cols_to_remove <- grep("_eng$|_span$|bat_", names(dataset), value = TRUE)
  dataset <- select(dataset, -all_of(cols_to_remove))  
  return(dataset)
}

# Use the modified function in your workflow
assessment_data_transformed <- assessment_data %>%
  create_composite_scores() %>%
  adjust_and_populate() %>%
  add_percent_accuracy()

# Ensure 'timepoint' is factored correctly
assessment_data_transformed$timepoint <- factor(assessment_data_transformed$timepoint, levels = c("Pre-Tx", "Post-Tx"))

assessment_data_long <- assessment_data_transformed %>%
  pivot_longer(
    cols = -c(id, timepoint, age, edu, mpo, L1, tx_lang, clqt_ef, sex, trainedset1_ES, L1_LUQ_RC1, L1_LUQ_RC2, L2_LUQ_RC1, L2_LUQ_RC2), # Exclude specified columns
    names_to = "assessment",
    values_to = "score"
  ) %>%
  mutate(
    # Adjust 'condition' to have "Trained" and "Untrained"
    condition = case_when(
      grepl("_trained_percent", assessment) ~ "Trained",
      grepl("_untrained_percent", assessment) ~ "Untrained",
      TRUE ~ NA_character_
    ),
    # Standardize 'assessment' names by removing specified patterns
    assessment = str_replace(assessment, "_trained_percent", ""),
    assessment = str_replace(assessment, "_untrained_percent", ""),
    # Capitalize 'timepoint' levels
    timepoint = factor(timepoint, levels = c("Pre-Tx", "Post-Tx"))
  )


##################################################################
##                         PLOT: WAB-AQ                         ##
##################################################################

# Filter and prepare WAB-AQ observed data
wab_aq_observed <- assessment_data_long %>%
  filter(assessment == "wab_aq") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Ensure the interaction levels are correctly formed
wab_aq_observed <- wab_aq_observed %>%
  mutate(condition = factor(condition, labels = c("Treated", "Untreated")),
         interaction_group = interaction(condition, timepoint))

# Calculate plot_ymax for consistent annotations
plot_ymax <- max(wab_aq_observed$score, na.rm = TRUE) + 5

# Generate WAB-AQ plot with correct color mapping
plot_wab_aq <- ggplot(wab_aq_observed, aes(x = condition, y = score, fill = interaction_group)) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "WAB-R AQ",
       x = "Assessed Language",
       y = "% Accuracy") +
  # Use the same custom color palette as the BNT plot
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    
      "Treated.Post-Tx" = "steelblue4",    
      "Untreated.Pre-Tx" = "sandybrown",      
      "Untreated.Post-Tx" = "sienna3"        
    ),
    name = "Timepoint",  # Legend title
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")  # Simplified labels
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # X-axis text size
    axis.text.y = element_text(size = 14),  # Y-axis text size
    axis.title.x = element_text(size = 16),  # X-axis title size
    axis.title.y = element_text(size = 16),  # Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Title size and styling
    legend.title = element_text(size = 16),  # Legend title size
    legend.text = element_text(size = 14),  # Legend text size
    plot.background = element_rect(color = "black", fill = NA, size = 0.5, linetype = "solid")  # Plot border
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Add annotations for pre- and post-treatment
plot_wab_aq <- plot_wab_aq +
  # Annotations for "Pre-Tx"
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 1, y = plot_ymax + 2, label = "**", size = 6) +  # Larger asterisk
  # Annotations for "Post-Tx"
  annotate("segment", x = 1.8, xend = 2.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.8, xend = 1.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 2.2, xend = 2.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 2, y = plot_ymax + 2, label = "*", size = 6)  # Larger asterisk

# Display the plot
plot_wab_aq

# Save the plot as PNG with specified dimensions and resolution
ggsave("plot_wab_aq.png", plot = plot_wab_aq, width = 8, height = 6, dpi = 300)



#################################################################
##                           PLOT -  BNT                       ##
#################################################################
# Filter and prepare BNT observed data
bnt_observed <- assessment_data_long %>% 
  filter(assessment == "bnt") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

bnt_observed_complete <- bnt_observed %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Adjust the condition labels in the data
bnt_observed <- bnt_observed %>%
  mutate(condition = factor(condition, 
                            levels = unique(condition),  # Ensure correct order if necessary
                            labels = c("Treated", "Untreated")))

# Calculate plot_ymax for consistent annotations
plot_ymax <- max(bnt_observed$score, na.rm = TRUE) + 5

# Generate the BNT plot with correct color mapping and larger font sizes
plot_bnt <- ggplot(bnt_observed, aes(x = condition, y = score, fill = interaction(condition, timepoint))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "Boston Naming Test",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    # Navy blue
      "Treated.Post-Tx" = "steelblue4",       # Medium blue
      "Untreated.Pre-Tx" = "sandybrown",      # Light yellowy-orange
      "Untreated.Post-Tx" = "sienna3"         # Dark yellowy-orange
    ),
    name = "Timepoint",  # Capitalized legend title
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")  # Simplified labels
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Adjust X-axis text size
    axis.text.y = element_text(size = 14),  # Adjust Y-axis text size
    axis.title.x = element_text(size = 16),  # Adjust X-axis title size
    axis.title.y = element_text(size = 16),  # Adjust Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Adjust plot title size
    legend.title = element_text(size = 16),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    plot.background = element_rect(color = "black", fill = NA, size = 0.5, linetype = "solid")  # Add border
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Add annotations for pre- and post-treatment
plot_bnt <- plot_bnt +
  # Annotations for "Pre-Tx"
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 1, y = plot_ymax + 2, label = "**", size = 6) +  # Adjust asterisk size
  # Annotations for "Post-Tx"
  annotate("segment", x = 1.8, xend = 2.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.8, xend = 1.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 2.2, xend = 2.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 2, y = plot_ymax + 2, label = "*", size = 6)  # Adjust asterisk size

# Display the plot
plot_bnt

# Save the plot as PNG with specified dimensions and resolution
ggsave("plot_bnt.png", plot = plot_bnt, width = 8, height = 6, dpi = 300)


#################################################################
##                        PLOT - RAVENS                        ##
#################################################################

# Filter and prepare Ravens observed data
ravens_observed <- assessment_data_long %>% 
  filter(assessment == "ravens_percent") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

ravens_observed_complete <- ravens_observed %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Calculate plot_ymax for consistent annotations
plot_ymax <- max(ravens_observed$score, na.rm = TRUE) + 5

# Generate the Ravens plot with correct color mapping and larger font sizes
plot_ravens <- ggplot(ravens_observed, aes(x = condition, y = score, fill = timepoint)) +
  stat_boxplot(geom = 'errorbar', width = 0.1, position = position_dodge(width = 0.5)) + 
  geom_boxplot(width = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Raven's Progressive Matrices",
       x = "",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Pre-Tx" = "lightblue3",    # Navy blue
      "Post-Tx" = "steelblue4"        # Medium blue
    ),
    name = "Timepoint",  # Capitalized legend title
    labels = c("Pre-treatment", "Post-treatment")  # Simplified labels
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.text.x = element_blank(),  # Suppress x-axis labels
    axis.text.y = element_text(size = 14),  # Adjust Y-axis text size
    axis.title.y = element_text(size = 16),  # Adjust Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Adjust title size and styling
    legend.title = element_text(size = 16),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    plot.background = element_rect(color = "black", fill = NA, size = 0.5, linetype = "solid")  # Add border
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Display the plot
plot_ravens

# Save the plot as PNG with specified dimensions and resolution
ggsave("plot_ravens.png", plot = plot_ravens, width = 8, height = 6, dpi = 300)



#################################################################
##                         PLOT - PAPT                         ##
#################################################################

# Filter and prepare PAPT total observed data
papt_total_observed <- assessment_data_long %>% 
  filter(assessment == "papt_total_percent") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

papt_total_observed_complete <- papt_total_observed %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Calculate plot_ymax for consistent annotations
plot_ymax <- max(papt_total_observed$score, na.rm = TRUE) + 5

# Generate the PAPT total plot with correct color mapping and larger font sizes
plot_papt_total <- ggplot(papt_total_observed, aes(x = condition, y = score, fill = timepoint)) +
  stat_boxplot(geom = 'errorbar', width = 0.1, position = position_dodge(width = 0.5)) + 
  geom_boxplot(width = 0.5, outlier.size = 0.5, position = position_dodge(width = 0.5)) +
  labs(title = "Pyramids and Palm Trees",
       x = "",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Pre-Tx" = "lightblue3",    # Light blue
      "Post-Tx" = "steelblue4"    # Medium blue
    ),
    name = "Timepoint",  # Capitalized legend title
    labels = c("Pre-treatment", "Post-treatment")  # Simplified labels
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.text.x = element_blank(),  # Suppress x-axis labels
    axis.text.y = element_text(size = 14),  # Adjust Y-axis text size
    axis.title.y = element_text(size = 16),  # Adjust Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Adjust title size and styling
    legend.title = element_text(size = 16),  # Adjust legend title size
    legend.text = element_text(size = 14),  # Adjust legend text size
    plot.background = element_rect(color = "black", fill = NA, size = 0.5, linetype = "solid")  # Add border
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Display the plot
plot_papt_total

# Save the plot as PNG with specified dimensions and resolution
ggsave("plot_papt_total.png", plot = plot_papt_total, width = 8, height = 6, dpi = 300)

#################################################################
##             PLOT - SPOKEN WORD PICTURE MATCHING             ##
#################################################################

# Filter and prepare Spoken Word Picture Matching observed data
spoken_word_picture_matching_observed <- assessment_data_long %>% 
  filter(assessment == "spoken_word_picture_matching") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Adjust the condition labels and ensure factors are correctly defined
spoken_word_picture_matching_observed <- spoken_word_picture_matching_observed %>%
  mutate(
    condition = factor(condition, levels = unique(condition), labels = c("Treated", "Untreated")),
    interaction_group = interaction(condition, timepoint)  # Create interaction for fill aesthetic
  )

# Calculate plot_ymax for annotations
plot_ymax <- max(spoken_word_picture_matching_observed$score, na.rm = TRUE) + 5

# Generate the plot with correct fill mapping
plot_spoken_word_picture_matching <- ggplot(spoken_word_picture_matching_observed, 
                                            aes(x = condition, y = score, fill = interaction_group)) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) + 
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Spoken Word Picture Matching",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  # Apply the correct color palette using lightblue3 and others
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",   # Light blue for Treated Pre-Tx
      "Treated.Post-Tx" = "steelblue4",  # Medium blue for Treated Post-Tx
      "Untreated.Pre-Tx" = "sandybrown", # Light yellowy-orange for Untreated Pre-Tx
      "Untreated.Post-Tx" = "sienna3"    # Dark yellowy-orange for Untreated Post-Tx
    ),
    name = "Timepoint",  # Legend title
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")  # Simplified labels
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # X-axis text size
    axis.text.y = element_text(size = 14),  # Y-axis text size
    axis.title.x = element_text(size = 16),  # X-axis title size
    axis.title.y = element_text(size = 16),  # Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Title styling
    legend.title = element_text(size = 16),  # Legend title size
    legend.text = element_text(size = 14),  # Legend text size
    plot.background = element_rect(color = "black", fill = NA, size = 0.5, linetype = "solid")  # Plot border
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")


# Add annotations for pre- and post-treatment comparisons
plot_spoken_word_picture_matching <- plot_spoken_word_picture_matching +
  # Annotations for "Pre-Tx"
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 1, y = plot_ymax + 2, label = "**", size = 6) +  # Asterisk size
  # Annotations for "Post-Tx"
  annotate("segment", x = 1.8, xend = 2.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.8, xend = 1.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 2.2, xend = 2.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 2, y = plot_ymax + 2, label = "*", size = 6)  # Asterisk size

# Display the plot
plot_spoken_word_picture_matching

# Save the plot as PNG
ggsave("plot_spoken_word_picture_matching.png", 
       plot = plot_spoken_word_picture_matching, width = 8, height = 6, dpi = 300)


##################################################################
##             PLOT - WRITTEN WORD PICTURE MATCHING             ##
##################################################################

# Prepare the data
written_word_picture_matching_observed <- assessment_data_long %>% 
  filter(assessment == "written_word_picture_matching") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Ensure the condition is properly labeled
written_word_picture_matching_observed <- written_word_picture_matching_observed %>%
  mutate(condition = factor(condition, levels = unique(condition), labels = c("Treated", "Untreated")),
         interaction_group = interaction(condition, timepoint))

# Calculate plot_ymax
plot_ymax <- max(written_word_picture_matching_observed$score, na.rm = TRUE) + 5

# Generate the plot
plot_written_word_picture_matching <- ggplot(written_word_picture_matching_observed, 
                                             aes(x = condition, y = score, fill = interaction_group)) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) + 
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Written Word Picture Matching",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    
      "Treated.Post-Tx" = "steelblue4",   
      "Untreated.Pre-Tx" = "sandybrown",  
      "Untreated.Post-Tx" = "sienna3"
    ),
    name = "Timepoint",
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.background = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")


# Save the plot
ggsave("plot_written_word_picture_matching.png", plot = plot_written_word_picture_matching, width = 8, height = 6, dpi = 300)

##################################################################
##              PLOT - AUDITORY SYNONYM JUDGEMENTS              ##
##################################################################
# Prepare the data
auditory_synonym_judgments_observed <- assessment_data_long %>% 
  filter(assessment == "auditory_synonym_judgments") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Ensure the condition is properly labeled
auditory_synonym_judgments_observed <- auditory_synonym_judgments_observed %>%
  mutate(condition = factor(condition, levels = unique(condition), labels = c("Treated", "Untreated")),
         interaction_group = interaction(condition, timepoint))

# Calculate plot_ymax
plot_ymax <- max(auditory_synonym_judgments_observed$score, na.rm = TRUE) + 5

# Generate the plot
plot_auditory_synonym_judgments <- ggplot(auditory_synonym_judgments_observed, 
                                          aes(x = condition, y = score, fill = interaction_group)) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) + 
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "Auditory Synonym Judgments",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    
      "Treated.Post-Tx" = "steelblue4",   
      "Untreated.Pre-Tx" = "sandybrown",  
      "Untreated.Post-Tx" = "sienna3"
    ),
    name = "Timepoint",
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.background = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Save the plot
ggsave("plot_auditory_synonym_judgments.png", plot = plot_auditory_synonym_judgments, width = 8, height = 6, dpi = 300)


#################################################################
##              PLOT - WRITTEN SYNONYM JUDGEMENTS              ##
#################################################################
# Filter and prepare Written Synonym Judgments data
written_synonym_judgments_observed <- assessment_data_long %>% 
  filter(assessment == "written_synonym_judgments") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Ensure factors are correctly defined
written_synonym_judgments_observed <- written_synonym_judgments_observed %>%
  mutate(
    condition = factor(condition, levels = unique(condition), labels = c("Treated", "Untreated")),
    interaction_group = interaction(condition, timepoint)
  )

# Calculate ymax
plot_ymax <- max(written_synonym_judgments_observed$score, na.rm = TRUE) + 5

# Generate the plot
plot_written_synonym_judgments <- ggplot(written_synonym_judgments_observed, 
                                         aes(x = condition, y = score, fill = interaction_group)) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(position = position_dodge(width = 0.75)) +
  labs(title = "Written Synonym Judgments",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",
      "Treated.Post-Tx" = "steelblue4",
      "Untreated.Pre-Tx" = "sandybrown",
      "Untreated.Post-Tx" = "sienna3"
    ),
    name = "Timepoint",
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.background = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Save the plot
ggsave("plot_written_synonym_judgments.png", 
       plot = plot_written_synonym_judgments, width = 8, height = 6, dpi = 300)


##################################################################
##               PLOT - WORD SEMANTIC ASSOCIATION               ##
##################################################################
# Filter and prepare Word Semantic Association data
word_semantic_association_observed <- assessment_data_long %>% 
  filter(assessment == "word_semantic_association") %>%
  group_by(id) %>%
  filter(all(!is.na(score))) %>%
  ungroup()

# Ensure factors are correctly defined
word_semantic_association_observed <- word_semantic_association_observed %>%
  mutate(
    condition = factor(condition, levels = unique(condition), labels = c("Treated", "Untreated")),
    interaction_group = interaction(condition, timepoint)
  )

# Calculate ymax
plot_ymax <- max(word_semantic_association_observed$score, na.rm = TRUE) + 5

# Generate the plot
plot_word_semantic_association <- ggplot(word_semantic_association_observed, 
                                         aes(x = condition, y = score, fill = interaction_group)) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) +
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Word Semantic Association",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",
      "Treated.Post-Tx" = "steelblue4",
      "Untreated.Pre-Tx" = "sandybrown",
      "Untreated.Post-Tx" = "sienna3"
    ),
    name = "Timepoint",
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    plot.background = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100)) +
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("text", x = 1, y = plot_ymax + 2, label = "**", size = 6)+
  guides(fill = "none")

# Save the plot
ggsave("plot_word_semantic_association.png", 
       plot = plot_word_semantic_association, width = 8, height = 6, dpi = 300)

#################################################################
##                 PLOT - TRANSLATION COMPOSITE                 ##
##################################################################
translation_composite_observed <- assessment_data_long %>% 
  filter(assessment == "translation_composite") %>% group_by(id) %>%  # Group by the unique identifier for each subject or pair
  filter(all(!is.na(score))) %>%  # Keep groups where all scores are available
  ungroup()

translation_composite_observed_complete <- translation_composite_observed %>%
  group_by(id) %>%  # Group by the unique identifier for each subject or pair
  filter(all(!is.na(score))) %>%  # Keep groups where all scores are available
  ungroup()


# Generate the box plot for translation_composite
plot_translation_composite <- ggplot(translation_composite_observed, aes(x = condition, y = score, fill = timepoint)) +
  stat_boxplot(geom= 'errorbar' , width = 0.3, position = position_dodge(width = 0.75) ) + 
  geom_boxplot() +
  labs(title = "Translation Composite",
       x = "Assessed Language",
       y = "Percent Accuracy") +
  scale_fill_brewer(palette = "Paired", name = "Timepoint") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(color = "black", fill = NA, size = 0.5, linetype = "solid")) +  # Add border around the full plot
  scale_y_continuous(limits = c(0, plot_ymax + 10), breaks = c(0, 25, 50, 75, 100))  # Extend y-axis slightly




plot_ymax <- max(translation_composite_observed$score, na.rm = TRUE) + 5  # Adjust the margin as needed

# Plot with manual annotations for each set of two boxes for translation_composite
plot_translation_composite <- plot_translation_composite +
  # First set of annotations for "Pre-Tx"
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax - 2, yend = plot_ymax, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax - 2, yend = plot_ymax, colour = "black") 
star_y_position <- plot_ymax + 3  # Adjust the value to position the star above the brackets

# Add the star annotation to your plot
plot_translation_composite <- plot_translation_composite +
  annotate("text", x = 1, y = star_y_position, label = "*", size = 5, colour = "black") 

ggsave("plot_translation_composite.png", plot = plot_translation_composite, width = 8, height = 6, dpi = 300)

# Combine plots
combined_plot <- plot_wab_aq / plot_bnt / plot_ravens / plot_papt_total / plot_spoken_word_picture_matching / plot_written_word_picture_matching / plot_auditory_synonym_judgments / plot_written_synonym_judgments / plot_word_semantic_association / plot_translation_composite

# You can adjust the layout to your liking using `plot_layout()`
# For example, to arrange them in a 2x5 grid:
combined_plot <- combined_plot + plot_layout(ncol = 2)

# Save the combined plot to a PNG file
ggsave("combined_plots.png", combined_plot, width = 16, height = 30, dpi = 300)


##################################################################
##                         PLOT - SET 1                         ##
##################################################################
# Convert percentages to correct scale (If plot gets wonky, reload tx probes df and try again. Not sure why it's doing this)
tx_probes$perc_acc <- tx_probes$perc_acc * 100

# Replace "trained" and "untrained" with "Treated" and "Untreated" in tx_probes
tx_probes <- tx_probes %>%
  mutate(trained_untrained = ifelse(trained_untrained == "trained", "Treated", "Untreated"))

# Filter data for Set 1
tx_probes_set1 <- tx_probes %>%
  filter(set == 1) %>%
  group_by(id) %>%
  filter(all(!is.na(perc_acc))) %>%
  ungroup()

# Calculate the ymax value for Set 1
plot_ymax_set1 <- max(tx_probes_set1$perc_acc, na.rm = TRUE) + 5

# Generate the box plot for Set 1
plot_set1 <- ggplot(tx_probes_set1, 
                    aes(x = trained_untrained, y = perc_acc, fill = interaction(trained_untrained, timepoint))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) + 
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Set 1: Trained Items and Untrained Translations",
       x = "Assessed Language",
       y = "% Accuracy") +
  # Apply consistent custom colors for each condition and timepoint
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    # Light blue for Treated Pre-Tx
      "Treated.Post-Tx" = "steelblue4",   # Medium blue for Treated Post-Tx
      "Untreated.Pre-Tx" = "sandybrown",  # Light yellowy-orange for Untreated Pre-Tx
      "Untreated.Post-Tx" = "sienna3"     # Dark yellowy-orange for Untreated Post-Tx
    ),
    name = "Timepoint",
    labels = c("Pre-treatment", "Post-treatment", "Pre-treatment", "Post-treatment")
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # X-axis text size
    axis.text.y = element_text(size = 14),  # Y-axis text size
    axis.title.x = element_text(size = 16),  # X-axis title size
    axis.title.y = element_text(size = 16),  # Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Title styling
    legend.title = element_text(size = 16),  # Legend title size
    legend.text = element_text(size = 14),  # Legend text size
    plot.background = element_rect(color = "black", fill = NA, size = 0.5)  # Plot border
  ) +
  scale_y_continuous(limits = c(0, plot_ymax_set1 + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Add annotations for pre- and post-treatment comparisons
plot_set1 <- plot_set1 +
  # Annotations for Pre-Tx
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax_set1, yend = plot_ymax_set1, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax_set1 - 2, yend = plot_ymax_set1, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax_set1 - 2, yend = plot_ymax_set1, colour = "black") +
  annotate("text", x = 1, y = plot_ymax_set1 + 2, label = "***", size = 6) +  # Asterisk size 6
  # Annotations for Post-Tx
  annotate("segment", x = 1.8, xend = 2.2, y = plot_ymax_set1, yend = plot_ymax_set1, colour = "black") +
  annotate("segment", x = 1.8, xend = 1.8, y = plot_ymax_set1 - 2, yend = plot_ymax_set1, colour = "black") +
  annotate("segment", x = 2.2, xend = 2.2, y = plot_ymax_set1 - 2, yend = plot_ymax_set1, colour = "black") +
  annotate("text", x = 2, y = plot_ymax_set1 + 2, label = "***", size = 6)

# Display the plot
plot_set1

# Save the plot as PNG
ggsave("plot_set1.png", plot = plot_set1, width = 8, height = 6, dpi = 300)

##################################################################
##                         PLOT - SET 2                         ##
##################################################################
# Step 1: Filter data for Set 2
tx_probes_set2 <- tx_probes %>%
  filter(set == 2) %>%
  group_by(id, trained_untrained) %>%
  filter(all(!is.na(perc_acc))) %>%
  ungroup()

# Step 2: Re-factor to ensure both "Treated" and "Untreated" levels are retained
tx_probes_set2$trained_untrained <- factor(
  tx_probes_set2$trained_untrained, levels = c("Treated", "Untreated")
)

# Step 3: Calculate ymax for annotations
plot_ymax_set2 <- max(tx_probes_set2$perc_acc, na.rm = TRUE) + 5

# Step 4: Generate the box plot for Set 2
plot_set2 <- ggplot(tx_probes_set2, 
                    aes(x = trained_untrained, y = perc_acc, fill = interaction(trained_untrained, timepoint))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) + 
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Set 2: Related Items and Translations (Untrained)",
       x = "Assessed Language",
       y = "% Accuracy") +
  # Step 5: Apply custom colors for consistency
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    # Light blue for Treated Pre-Tx
      "Treated.Post-Tx" = "steelblue4",   # Medium blue for Treated Post-Tx
      "Untreated.Pre-Tx" = "sandybrown",  # Light yellowy-orange for Untreated Pre-Tx
      "Untreated.Post-Tx" = "sienna3"     # Dark yellowy-orange for Untreated Post-Tx
    )
  ) +
  theme_minimal(base_size = 14) +  # Adjust base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Adjust X-axis text size
    axis.text.y = element_text(size = 14),  # Adjust Y-axis text size
    axis.title.x = element_text(size = 16),  # Adjust X-axis title size
    axis.title.y = element_text(size = 16),  # Adjust Y-axis title size
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Plot title styling
    plot.background = element_rect(color = "black", fill = NA, size = 0.5),  # Plot border
    legend.position = "none"  # Remove the legend
  ) +
  scale_y_continuous(limits = c(0, plot_ymax_set2 + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Step 6: Add annotations for pre- and post-treatment comparisons
plot_set2 <- plot_set2 +
  # Pre-Tx Annotations
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax_set2, yend = plot_ymax_set2, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax_set2 - 2, yend = plot_ymax_set2, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax_set2 - 2, yend = plot_ymax_set2, colour = "black") +
  annotate("text", x = 1, y = plot_ymax_set2 + 2, label = "***", size = 6) +
  # Post-Tx Annotations
  annotate("segment", x = 1.8, xend = 2.2, y = plot_ymax_set2, yend = plot_ymax_set2, colour = "black") +
  annotate("segment", x = 1.8, xend = 1.8, y = plot_ymax_set2 - 2, yend = plot_ymax_set2, colour = "black") +
  annotate("segment", x = 2.2, xend = 2.2, y = plot_ymax_set2 - 2, yend = plot_ymax_set2, colour = "black") +
  annotate("text", x = 2, y = plot_ymax_set2 + 2, label = "**", size = 6)

# Step 7: Display the plot
plot_set2

# Step 8: Save the plot as PNG
ggsave("plot_set2.png", plot = plot_set2, width = 8, height = 6, dpi = 300)



##################################################################
##                         PLOT - SET 3                         ##
##################################################################
# Step 1: Filter data for Set 3
tx_probes_set3 <- tx_probes %>%
  filter(set == 3) %>%
  group_by(id, trained_untrained) %>%
  filter(all(!is.na(perc_acc))) %>%
  ungroup()

# Step 2: Re-factor trained_untrained to ensure both levels are retained
tx_probes_set3$trained_untrained <- factor(
  tx_probes_set3$trained_untrained, levels = c("Treated", "Untreated")
)

# Step 3: Calculate ymax value for Set 3
plot_ymax_set3 <- max(tx_probes_set3$perc_acc, na.rm = TRUE) + 5

# Step 4: Generate the box plot for Set 3
plot_set3 <- ggplot(tx_probes_set3, 
                    aes(x = trained_untrained, y = perc_acc, fill = interaction(trained_untrained, timepoint))) +
  stat_boxplot(geom = 'errorbar', width = 0.3, position = position_dodge(width = 0.75)) + 
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.75)) +
  labs(title = "Set 3: Unrelated Items and Translations (Untrained)",
       x = "Assessed Language",
       y = "% Accuracy") +
  # Step 5: Apply custom colors
  scale_fill_manual(
    values = c(
      "Treated.Pre-Tx" = "lightblue3",    # Light blue for Treated Pre-Tx
      "Treated.Post-Tx" = "steelblue4",   # Medium blue for Treated Post-Tx
      "Untreated.Pre-Tx" = "sandybrown",  # Light yellowy-orange for Untreated Pre-Tx
      "Untreated.Post-Tx" = "sienna3"     # Dark yellowy-orange for Untreated Post-Tx
    )
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size for clarity
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    plot.background = element_rect(color = "black", fill = NA, size = 0.5),
    legend.position = "none"  # Remove the legend
  ) +
  scale_y_continuous(limits = c(0, plot_ymax_set3 + 10), breaks = c(0, 25, 50, 75, 100))+
  guides(fill = "none")

# Step 6: Add annotations for pre- and post-treatment comparisons
plot_set3 <- plot_set3 +
  # Pre-Tx Annotations
  annotate("segment", x = 0.8, xend = 1.2, y = plot_ymax_set3, yend = plot_ymax_set3, colour = "black") +
  annotate("segment", x = 0.8, xend = 0.8, y = plot_ymax_set3 - 2, yend = plot_ymax_set3, colour = "black") +
  annotate("segment", x = 1.2, xend = 1.2, y = plot_ymax_set3 - 2, yend = plot_ymax_set3, colour = "black") +
  annotate("text", x = 1, y = plot_ymax_set3 + 2, label = "***", size = 6) +
  # Post-Tx Annotations
  annotate("segment", x = 1.8, xend = 2.2, y = plot_ymax_set3, yend = plot_ymax_set3, colour = "black") +
  annotate("segment", x = 1.8, xend = 1.8, y = plot_ymax_set3 - 2, yend = plot_ymax_set3, colour = "black") +
  annotate("segment", x = 2.2, xend = 2.2, y = plot_ymax_set3 - 2, yend = plot_ymax_set3, colour = "black") +
  annotate("text", x = 2, y = plot_ymax_set3 + 2, label = "**", size = 6)

# Step 7: Display the plot
plot_set3

# Step 8: Save the plot as PNG
ggsave("plot_set3.png", plot = plot_set3, width = 8, height = 6, dpi = 300)



#################################################################
##                  COMBINED ASSESSMENTS PLOTS                  ##
#################################################################
combined_assessments_plot <- grid.arrange(plot_bnt, plot_spoken_word_picture_matching, 
                                          plot_written_word_picture_matching, 
                                          plot_auditory_synonym_judgments, 
                                          plot_written_synonym_judgments, 
                                          plot_word_semantic_association, 
                                          plot_papt_total, plot_wab_aq, 
                                          plot_ravens, ncol=3)


# Remove legends and borders from the three plots
plot_set1 <- plot_set1 + theme(legend.position = "none", plot.background = element_blank())
plot_set2 <- plot_set2 + theme(legend.position = "none", plot.background = element_blank())
plot_set3 <- plot_set3 + theme(legend.position = "none", plot.background = element_blank())

# Combine the three plots into a single row
combined_plot <- plot_set1 + plot_set2 + plot_set3 + 
  plot_layout(nrow = 1)  # Arrange in one row

# Save the combined plot with adjusted dimensions
ggsave("combined_plot.svg", plot = combined_plot, width = 24, height = 8, dpi = 300)



# WAB Sanity Check --------------------------------------------------------

# Read the data
wab_data <- read.csv("/Users/marissarussell/Library/CloudStorage/OneDrive-Personal/Documents/Academics/BU/CBR/PROCoM/Generalization Analysis/Data/Transfer Analysis/Final Take Feb 2024/Transfer Analysis Final Take 2024/wab_aq_sanitycheck.csv", stringsAsFactors = TRUE)

# Step 2: Identify all English and Spanish columns (for raw and percentage scores)
eng_columns_wab <- names(wab_data)[grepl("_eng", names(wab_data))]
span_columns_wab <- names(wab_data)[grepl("_span", names(wab_data))]

# Step 3: Dynamically create "treated" and "untreated" columns for both raw and percentage scores
for (col in setdiff(eng_columns_wab, span_columns_wab)) {
  # Generate base column name by removing "_eng" suffix
  col_base <- sub("_eng$", "", col)
  col_span <- paste0(col_base, "_span")  # Corresponding Spanish column
  
  # Check if the corresponding Spanish column exists
  if (col_span %in% span_columns_wab) {
    # Create new treated/untreated columns for both raw and percentage scores
    wab_data <- wab_data %>%
      mutate(
        # Treated column
        "{col_base}_treated" := case_when(
          tx_lang == "English" ~ .data[[col]],
          tx_lang == "Spanish" ~ .data[[col_span]],
          TRUE ~ NA_real_
        ),
        # Untreated column
        "{col_base}_untreated" := case_when(
          tx_lang == "English" ~ .data[[col_span]],
          tx_lang == "Spanish" ~ .data[[col]],
          TRUE ~ NA_real_
        ),
        # Treated percentage column
        "{col_base}_treated_pct" := case_when(
          tx_lang == "English" ~ .data[[paste0(col, ".1")]],
          tx_lang == "Spanish" ~ .data[[paste0(col_span, ".1")]],
          TRUE ~ NA_real_
        ),
        # Untreated percentage column
        "{col_base}_untreated_pct" := case_when(
          tx_lang == "English" ~ .data[[paste0(col_span, ".1")]],
          tx_lang == "Spanish" ~ .data[[paste0(col, ".1")]],
          TRUE ~ NA_real_
        )
      )
  }
}


# Step 2: Reshape the data into long format
pre_post_data <- wab_data %>%
  filter(Event.Name %in% c("Pre-Tx", "Post-Tx")) %>%  # Keep only Pre-Tx and Post-Tx
  pivot_longer(
    cols = ends_with("_treated") | ends_with("_untreated"),
    names_to = c("subtest", "status"),
    names_pattern = "(.*)_(treated|untreated)$",
    values_to = "score"
  )


#AVC
avc_treated <- pre_post_data %>%
  filter(subtest == "Auditory.Verbal.Comprehension...Total") %>%
  filter(status == "treated")

avc_untreated <- pre_post_data %>%
  filter(subtest == "Auditory.Verbal.Comprehension...Total") %>%
  filter(status == "untreated")

# Ensure Event.Name is a factor and set "Pre-Tx" as the reference level
avc_treated <- avc_treated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

avc_untreated <- avc_untreated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

# Fit the models again with re-leveled Event.Name
mod_avc_treated <- lmer(score ~ Event.Name + (1 | Record.ID), data = avc_treated)
summary(mod_avc_treated)

mod_avc_untreated <- lmer(score ~ Event.Name + (1 | Record.ID), data = avc_untreated)
summary(mod_avc_untreated)

#NWF
nwf_treated <- pre_post_data %>%
  filter(subtest == "Naming.and.Word.Finding...Total") %>%
  filter(status == "treated")
nwf_untreated <- pre_post_data %>%
  filter(subtest == "Naming.and.Word.Finding...Total") %>%
  filter(status == "untreated")

nwf_treated <- nwf_treated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))
nwf_untreated <- nwf_untreated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

mod_nwf_treated <- lmer(score ~ Event.Name + (1 | Record.ID), data = nwf_treated)
mod_nwf_untreated <- lmer(score ~ Event.Name + (1 | Record.ID), data = nwf_untreated)
summary(mod_nwf_treated)
summary(mod_nwf_untreated)

#SS
ss_treated <- pre_post_data %>%
  filter(subtest == "Spontaneous.Speech...Total") %>%
  filter(status == "treated")
ss_untreated <- pre_post_data %>%
  filter(subtest == "Spontaneous.Speech...Total") %>%
  filter(status == "untreated")
ss_treated <- ss_treated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))
ss_untreated <- ss_untreated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

mod_ss_treated <- lmer(score ~ Event.Name + (1 | Record.ID), data = ss_treated)
mod_ss_untreated <- lmer(score ~ Event.Name + (1 | Record.ID), data = ss_untreated)
summary(mod_ss_treated)
summary(mod_ss_untreated)

# Rep
# Filter data for Repetition - Total_span
rep_span_treated <- pre_post_data %>%
  filter(subtest == "Repetition - Total_span") %>%
  filter(status == "treated")

rep_span_untreated <- pre_post_data %>%
  filter(subtest == "Repetition - Total_span") %>%
  filter(status == "untreated")

# Filter data for Repetition - Total_eng
rep_eng_treated <- pre_post_data %>%
  filter(subtest == "Repetition - Total_eng") %>%
  filter(status == "treated")

rep_eng_untreated <- pre_post_data %>%
  filter(subtest == "Repetition - Total_eng") %>%
  filter(status == "untreated")

# Set the factor levels for Event.Name in treated and untreated for both span and eng
rep_span_treated <- rep_span_treated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

rep_span_untreated <- rep_span_untreated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

rep_eng_treated <- rep_eng_treated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

rep_eng_untreated <- rep_eng_untreated %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))

# Fit linear mixed-effects models for treated and untreated groups for both span and eng
mod_rep_span_treated <- lmer(score ~ Event.Name + (1 | Record.ID), data = rep_span_treated)
mod_rep_span_untreated <- lmer(score ~ Event.Name + (1 | Record.ID), data = rep_span_untreated)

mod_rep_eng_treated <- lmer(score ~ Event.Name + (1 | Record.ID), data = rep_eng_treated)
mod_rep_eng_untreated <- lmer(score ~ Event.Name + (1 | Record.ID), data = rep_eng_untreated)

# Display summaries of the models
summary(mod_rep_span_treated)
summary(mod_rep_span_untreated)
summary(mod_rep_eng_treated)
summary(mod_rep_eng_untreated)


# Visualize
# Combine treated and untreated data for visualization
avc_combined <- bind_rows(
  avc_treated %>% mutate(status = "Treated"),
  avc_untreated %>% mutate(status = "Untreated")
)

# Create a line plot with separate panels for treated and untreated
ggplot(avc_combined, aes(x = Event.Name, y = score, group = Record.ID, color = status)) +
  geom_line(alpha = 0.5) +  # Individual trajectories
  geom_point() +            # Points for individual scores
  stat_summary(
    fun = mean, geom = "line", aes(group = status), 
    linetype = "solid", size = 1.2
  ) +  # Group mean lines
  stat_summary(
    fun = mean, geom = "point", aes(group = status), 
    size = 3
  ) +  # Group mean points
  facet_wrap(~ status) +  # Separate panels for treated and untreated
  scale_color_manual(values = c("Treated" = "cornflowerblue", "Untreated" = "sandybrown")) +
  labs(
    title = "Pre-Post Change in AVC Subtest Scores (Treated vs. Untreated)",
    x = "Timepoint", 
    y = "Score",
    color = "Status"
  ) +
  theme_minimal()

#NWF PLOT
# Combine NWF treated and untreated data
nwf_combined <- bind_rows(
  nwf_treated %>% mutate(status = "Treated"),
  nwf_untreated %>% mutate(status = "Untreated")
)

# Plot NWF with separate panels for treated and untreated
ggplot(nwf_combined, aes(x = Event.Name, y = score, group = Record.ID, color = status)) +
  geom_line(alpha = 0.5) +  # Individual participant trajectories
  geom_point() +            # Points for individual scores
  stat_summary(
    fun = mean, geom = "line", aes(group = status), 
    size = 1.2, linetype = "solid"
  ) +  # Group mean line
  stat_summary(
    fun = mean, geom = "point", size = 3
  ) +  # Group mean points
  facet_wrap(~ status) +  # Separate panels for treated and untreated
  scale_color_manual(values = c("Treated" = "cornflowerblue", "Untreated" = "sandybrown")) +
  labs(
    title = "Pre-Post Change in NWF Scores (Treated vs. Untreated)",
    x = "Timepoint", y = "Score", color = "Status"
  ) +
  theme_minimal()

# SS
# Combine SS treated and untreated data
ss_combined <- bind_rows(
  ss_treated %>% mutate(status = "Treated"),
  ss_untreated %>% mutate(status = "Untreated")
)

# Plot SS with separate panels for treated and untreated
ggplot(ss_combined, aes(x = Event.Name, y = score, group = Record.ID, color = status)) +
  geom_line(alpha = 0.5) +  # Individual participant trajectories
  geom_point() +            # Points for individual scores
  stat_summary(
    fun = mean, geom = "line", aes(group = status), 
    size = 1.2, linetype = "solid"
  ) +  # Group mean line
  stat_summary(
    fun = mean, geom = "point", size = 3
  ) +  # Group mean points
  facet_wrap(~ status) +  # Separate panels for treated and untreated
  scale_color_manual(values = c("Treated" = "cornflowerblue", "Untreated" = "sandybrown")) +
  labs(
    title = "Pre-Post Change in SS Scores (Treated vs. Untreated)",
    x = "Timepoint", y = "Score", color = "Status"
  ) +
  theme_minimal()


# WAB Sanity Check - Percentages ------------------------------------------


# Reshape percentage columns for further analysis
pre_post_data_pct <- wab_data %>%
  filter(Event.Name %in% c("Pre-Tx", "Post-Tx")) %>%  # Keep only Pre-Tx and Post-Tx
  pivot_longer(
    cols = ends_with("_treated_pct") | ends_with("_untreated_pct"),
    names_to = c("subtest", "status"),
    names_pattern = "(.*)_(treated|untreated)_pct$",
    values_to = "percentage"
  )

# Ensure 'Event.Name' is a factor with "Pre-Tx" as the reference level
pre_post_data_pct <- pre_post_data_pct %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))


# Step 2: Ensure 'Event.Name' is a factor with "Pre-Tx" as the reference level
pre_post_data_pct <- pre_post_data_pct %>%
  mutate(Event.Name = factor(Event.Name, levels = c("Pre-Tx", "Post-Tx")))


# AVC
mod_avc_treated_pct <- lmer(percentage ~ Event.Name + (1 | Record.ID),
                            data = filter(pre_post_data_pct, subtest == "Auditory.Verbal.Comprehension...Total", status == "treated"))
mod_avc_untreated_pct <- lmer(percentage ~ Event.Name + (1 | Record.ID),
                              data = filter(pre_post_data_pct, subtest == "Auditory.Verbal.Comprehension...Total", status == "untreated"))

# NWF
mod_nwf_treated_pct <- lmer(percentage ~ Event.Name + (1 | Record.ID),
                            data = filter(pre_post_data_pct, subtest == "Naming.and.Word.Finding...Total", status == "treated"))
mod_nwf_untreated_pct <- lmer(percentage ~ Event.Name + (1 | Record.ID),
                              data = filter(pre_post_data_pct, subtest == "Naming.and.Word.Finding...Total", status == "untreated"))

# SS
mod_ss_treated_pct <- lmer(percentage ~ Event.Name + (1 | Record.ID),
                           data = filter(pre_post_data_pct, subtest == "Spontaneous.Speech...Total", status == "treated"))
mod_ss_untreated_pct <- lmer(percentage ~ Event.Name + (1 | Record.ID),
                             data = filter(pre_post_data_pct, subtest == "Spontaneous.Speech...Total", status == "untreated"))

# Display model summaries
summary(mod_avc_treated_pct)
summary(mod_avc_untreated_pct)
summary(mod_nwf_treated_pct)
summary(mod_nwf_untreated_pct)
summary(mod_ss_treated_pct)
summary(mod_ss_untreated_pct)