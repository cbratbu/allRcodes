## load the data
imputed_data1 <- read.csv("imputed_data1.csv")
imputed_data2 <- read.csv("imputed_data2.csv")
imputed_data3 <- read.csv("imputed_data3.csv")
imputed_data4 <- read.csv("imputed_data4.csv")
imputed_data5 <- read.csv("imputed_data5.csv")

imputed_data1$ID <- NULL
imputed_data2$ID <- NULL
imputed_data3$ID <- NULL
imputed_data4$ID <- NULL
imputed_data5$ID <- NULL

########################################3
# Reduce the imputed datasets into one dataframe
dfs <- list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5)

# Calculate element-wise average
dataframe <- Reduce("+", dfs) / length(dfs)


########################################3
### normalization
Correlationsdataframe <- as.data.frame(scale(dataframe))
write.csv(dataframe, "imputed_pooled.csv", row.names = FALSE)

########################################3
### Correlations
correlation_tasks <- c("colorshape_congruent","colorshape_incongruent","digit_comp",
                       "fish_flanker_congruent","fish_flanker_incongruent",
                       "geometric_inclusion_included","geometric_inclusion_notincluded",
                       "geometric_matching_matching","geometric_matching_notmatching",
                       "lowlevel_complex1","lowlevel_complex2","lowlevel_complex3",
                       "lowlevel_complex4","lowlevel_pure1","lowlevel_pure2","lowlevel_pure3",
                       "lowlevel_pure4","MEC_emot_comprehension_sad",
                       "MEC_emot_comprehension_angry","MEC_emot_comprehension_happy",
                       "MEC_ling_comprehension_Question","MEC_ling_comprehension_Order",
                       "MEC_ling_comprehension_Statement","mental_calculation_multiplication",
                       "metasyntax_grammatical","metasyntax_ungrammatical","music_good",
                       "music_sour","PEPSC_comp_animal","PEPSC_comp_color","reading_numbers",
                       "rules_and_principles_principles","syntax_aud_active",
                       "syntax_aud_filler","syntax_aud_SE_clefts","syntax_aud_SRC",
                       "syntax_aud_passive","syntax_aud_OE_clefts","syntax_aud_ORC",
                       "syntax_vis_active","syntax_vis_filler","syntax_vis_SE_clefts",
                       "syntax_vis_SRC","syntax_vis_passive","syntax_vis_OE_clefts",
                       "syntax_vis_ORC","TOM_long_RUK__FILLER","TOM_long_RUK__MC",
                       "TOM_long_RUK__TB","TOM_long_RUK__FB","TOM_RK__FILLER",
                       "TOM_RK__TB_MC","TOM_RK__FB","writing_numbers",
                       "written_operations_multiplication","written_operations_subtraction",
                       "rcba_total_perc","bnt_total_perc","palpa_8_total_perc",
                       "palpa25_total_perc", "rbans_IMMEDIATEMEMORY_total",
                       "rbans_VISUOSPATIAL_total","rbans_LANGUAGE_total",
                       "rbans_ATTENTION_total","rbans_DELAYEDMEMORY_total",
                       "wms_visualreproduction_total","wms_logicalmemory_total",
                       "wms_verbalpaired_total","wms_symbol_span_total","symbol_cancel_perc","papt_picture_perc")

library(dplyr)

dataframe <- dataframe %>% select(correlation_tasks)

dataframe_corr <- cor(dataframe[, correlation_tasks])


###Threshold
# Function to return variable and row names with correlations >= threshold
tasks_above_threshold <- function(mat, threshold) {
  thresh_value <- threshold / 100
  results <- which(abs(mat) >= thresh_value, arr.ind = TRUE)
  
  # Create a data frame of matching variable and row names
  data.frame(
    Row = rownames(mat)[results[, 1]],
    Column = colnames(mat)[results[, 2]],
    Correlation = mat[results]
  )
}

# Apply function for each threshold and name the results
thresholds <- c(80, 75, 70, 65, 55)

diag(dataframe_corr) <- NA

# Get variable names meeting each threshold
average_df_tasks <- lapply(thresholds, function(t) tasks_above_threshold(dataframe_corr, t))
names(average_df_tasks) <- paste0(thresholds, "%")

# Function to remove duplicate unordered pairs
remove_duplicate_pairs <- function(df) {
  # Create a consistent ordering for the pairs
  df$pair_id <- apply(df[c("Row", "Column")], 1, function(x) paste(sort(x), collapse = "_"))
  
  # Keep only the first occurrence of each pair
  df <- df[!duplicated(df$pair_id), ]
  
  return(df)
}

average_df_tasks <- lapply(average_df_tasks, remove_duplicate_pairs)

########################
# Initialize an empty list to store data frames
task_counts_list <- list()

# Get the names of each threshold (assuming the list is named)
threshold_names <- names(average_df_tasks)

# Loop through each threshold and count task appearances
for (i in seq_along(average_df_tasks)) {
  df <- average_df_tasks[[i]]
  threshold <- threshold_names[i]
  
  # Combine Row and Column into a long format
  tasks <- c(df$Row, df$Column)
  
  # Count frequency of each task
  task_count <- as.data.frame(table(tasks), stringsAsFactors = FALSE)
  colnames(task_count) <- c("Task", "Pair_Count")
  
  # Add threshold label
  task_count$Threshold <- threshold
  
  task_counts_list[[i]] <- task_count
}

# Combine all counts into one data frame
final_task_counts <- do.call(rbind, task_counts_list)

# Write to CSV
threshold_80 <- average_df_tasks[['80%']]
threshold_75 <- average_df_tasks[['75%']]
threshold_70 <- average_df_tasks[['70%']]
threshold_65 <- average_df_tasks[['65%']]
threshold_55 <- average_df_tasks[['55%']]

write.csv(threshold_80, "threshold_80.csv", row.names = FALSE)
write.csv(threshold_75, "threshold_75.csv", row.names = FALSE)
write.csv(threshold_70, "threshold_70.csv", row.names = FALSE)
write.csv(threshold_65, "threshold_65.csv", row.names = FALSE)
write.csv(threshold_55, "threshold_55.csv", row.names = FALSE)

tasks_threshold_80 <- task_counts_list[[1]]
tasks_threshold_75 <- task_counts_list[[2]]
tasks_threshold_70 <- task_counts_list[[3]]
tasks_threshold_65 <- task_counts_list[[4]]
tasks_threshold_55 <- task_counts_list[[5]]

#######################################
library(dplyr)
library(purrr)

tasks_threshold_80 <- tasks_threshold_80 %>%
  mutate(
    Min = map_dbl(Task, ~ min(dataframe[[.x]], na.rm = TRUE)),
    Max = map_dbl(Task, ~ max(dataframe[[.x]], na.rm = TRUE))
  )
tasks_threshold_80 <- tasks_threshold_80 %>%
  mutate(Range = Max - Min)

tasks_threshold_75 <- tasks_threshold_75 %>%
  mutate(
    Min = map_dbl(Task, ~ min(dataframe[[.x]], na.rm = TRUE)),
    Max = map_dbl(Task, ~ max(dataframe[[.x]], na.rm = TRUE))
  )
tasks_threshold_75 <- tasks_threshold_75 %>%
  mutate(Range = Max - Min)

tasks_threshold_70 <- tasks_threshold_70 %>%
  mutate(
    Min = map_dbl(Task, ~ min(dataframe[[.x]], na.rm = TRUE)),
    Max = map_dbl(Task, ~ max(dataframe[[.x]], na.rm = TRUE))
  )
tasks_threshold_70 <- tasks_threshold_70 %>%
  mutate(Range = Max - Min)

tasks_threshold_65 <- tasks_threshold_65 %>%
  mutate(
    Min = map_dbl(Task, ~ min(dataframe[[.x]], na.rm = TRUE)),
    Max = map_dbl(Task, ~ max(dataframe[[.x]], na.rm = TRUE))
  )
tasks_threshold_65 <- tasks_threshold_65 %>%
  mutate(Range = Max - Min)

tasks_threshold_55 <- tasks_threshold_55 %>%
  mutate(
    Min = map_dbl(Task, ~ min(dataframe[[.x]], na.rm = TRUE)),
    Max = map_dbl(Task, ~ max(dataframe[[.x]], na.rm = TRUE))
  )
tasks_threshold_55 <- tasks_threshold_55 %>%
  mutate(Range = Max - Min)

write.csv(tasks_threshold_80, "tasks_threshold_80.csv", row.names = FALSE)
write.csv(tasks_threshold_75, "tasks_threshold_75.csv", row.names = FALSE)
write.csv(tasks_threshold_70, "tasks_threshold_70.csv", row.names = FALSE)
write.csv(tasks_threshold_65, "tasks_threshold_65.csv", row.names = FALSE)
write.csv(tasks_threshold_55, "tasks_threshold_55.csv", row.names = FALSE)

