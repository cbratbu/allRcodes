### Imputations
#Load data

library(mice)
master <- read.csv("master_for_imputation.csv")

percent_missing <- function(df) {
  # calculate the percentage of missing data for each column
  missing_percentages <- sapply(df, function(x) {
    sum(is.na(x)) / length(x) * 100  # % of missing values for each column
  })
  
  # convert the result to a dataframe
  missing_df <- data.frame(
    Variable = names(missing_percentages),  # column names
    Percent_Missing = missing_percentages   # % of missing values
  )
  
  return(missing_df)  # return the dataframe
}

missing_df = percent_missing(master)
write.csv(missing_df, "perc_missing_data_by_task.csv", row.names = FALSE)


percent_missing_by_id <- function(df, id_col = "ID") {
  # Ensure the ID column exists
  if (!(id_col %in% names(df))) {
    stop(paste("Column", id_col, "not found in the dataframe"))
  }
  
  # Remove the ID column from the data for missing calculation
  data_without_id <- df[ , !(names(df) %in% id_col)]
  
  # Calculate percent missing per row (excluding the ID column)
  missing_percentages <- apply(data_without_id, 1, function(row) {
    sum(is.na(row)) / length(row) * 100
  })
  
  # Create a dataframe with ID and percent missing
  missing_df <- data.frame(
    ID = df[[id_col]],
    Percent_Missing = missing_percentages
  )
  
  return(missing_df)
}

pwa_missing_df = percent_missing_by_id(master)
write.csv(pwa_missing_df, "perc_missing_data_by_pwa.csv", row.names = FALSE)


# Impute

imputed <- mice(master,m=5,maxit=50,meth='pmm',seed=500)

imputed_data1 <- complete(imputed, action = 1)
imputed_data2 <- complete(imputed, action = 2)
imputed_data3 <- complete(imputed, action = 3)
imputed_data4 <- complete(imputed, action = 4)
imputed_data5 <- complete(imputed, action = 5)

# Export CSVs
write.csv(imputed_data1, "imputed_data1.csv", row.names = FALSE)
write.csv(imputed_data2, "imputed_data2.csv", row.names = FALSE)
write.csv(imputed_data3, "imputed_data3.csv", row.names = FALSE)
write.csv(imputed_data4, "imputed_data4.csv", row.names = FALSE)
write.csv(imputed_data5, "imputed_data5.csv", row.names = FALSE)

#get summary stats
summary(master)
summary(imputed_data1)
summary(imputed_data2)
summary(imputed_data3)
summary(imputed_data4)
summary(imputed_data5)

all <- rbind(master,imputed_data1,imputed_data2,imputed_data3,imputed_data4,imputed_data5)
dataset_master <- rep("master", 41)
dataset_1 <- rep("1", 41)
dataset_2 <- rep("2", 41)
dataset_3 <- rep("3", 41)
dataset_4 <- rep("4", 41)
dataset_5 <- rep("5", 41)
group <- c(dataset_master, dataset_1, dataset_2, dataset_3, dataset_4, dataset_5)

group <- group[1:nrow(all)]
all$group <- group

# Combine the two vectors
group <- c(controls, patients)

#visualize
ggplot(all, aes(x = group, y = WAB_AQ, fill = group)) +
  geom_boxplot() +
  labs(x = "Dataset") +
  theme_minimal()



#################
### Normalization
#Remove ID
imputed_data1_noID <- imputed_data1[, !names(imputed_data1) %in% c("ID")]
imputed_data2_noID <- imputed_data2[, !names(imputed_data2) %in% c("ID")]
imputed_data3_noID <- imputed_data3[, !names(imputed_data3) %in% c("ID")]
imputed_data4_noID <- imputed_data4[, !names(imputed_data4) %in% c("ID")]
imputed_data5_noID <- imputed_data5[, !names(imputed_data5) %in% c("ID")]

# Calculate the z-scores for all columns
imputed_normalized_data1_noID <- as.data.frame(scale(imputed_data1_noID))
imputed_normalized_data2_noID <- as.data.frame(scale(imputed_data2_noID))
imputed_normalized_data3_noID <- as.data.frame(scale(imputed_data3_noID))
imputed_normalized_data4_noID <- as.data.frame(scale(imputed_data4_noID))
imputed_normalized_data5_noID <- as.data.frame(scale(imputed_data5_noID))

# Add ID back in
imputed_normalized_data1 <- cbind(ID = imputed_data1$ID, imputed_normalized_data1_noID)
imputed_normalized_data2 <- cbind(ID = imputed_data2$ID, imputed_normalized_data2_noID)
imputed_normalized_data3 <- cbind(ID = imputed_data3$ID, imputed_normalized_data3_noID)
imputed_normalized_data4 <- cbind(ID = imputed_data4$ID, imputed_normalized_data4_noID)
imputed_normalized_data5 <- cbind(ID = imputed_data5$ID, imputed_normalized_data5_noID)

# Export CSVs
write.csv(imputed_normalized_data1, "imputed_normalized_data1.csv", row.names = FALSE)
write.csv(imputed_normalized_data2, "imputed_normalized_data2.csv", row.names = FALSE)
write.csv(imputed_normalized_data3, "imputed_normalized_data3.csv", row.names = FALSE)
write.csv(imputed_normalized_data4, "imputed_normalized_data4.csv", row.names = FALSE)
write.csv(imputed_normalized_data5, "imputed_normalized_data5.csv", row.names = FALSE)

imputed_normalized_data1 <- read.csv("imputed_normalized_data1.csv")
imputed_normalized_data2 <- read.csv("imputed_normalized_data2.csv")
imputed_normalized_data3 <- read.csv("imputed_normalized_data3.csv")
imputed_normalized_data4 <- read.csv("imputed_normalized_data4.csv")
imputed_normalized_data5 <- read.csv("imputed_normalized_data5.csv")

#################
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
imputed_data1 <- read.csv("imputed_data1.csv")
nowab_imputed_normalized_data1 <- imputed_normalized_data1 %>% select(correlation_tasks)
nowab_imputed_normalized_data2 <- imputed_normalized_data2 %>% select(correlation_tasks)
nowab_imputed_normalized_data3 <- imputed_normalized_data3 %>% select(correlation_tasks)
nowab_imputed_normalized_data4 <- imputed_normalized_data4 %>% select(correlation_tasks)
nowab_imputed_normalized_data5 <- imputed_normalized_data5 %>% select(correlation_tasks)

data1_corr <- cor(nowab_imputed_normalized_data1[, correlation_tasks])
data2_corr <- cor(nowab_imputed_normalized_data2[, correlation_tasks])
data3_corr <- cor(nowab_imputed_normalized_data3[, correlation_tasks])
data4_corr <- cor(nowab_imputed_normalized_data4[, correlation_tasks])
data5_corr <- cor(nowab_imputed_normalized_data5[, correlation_tasks])

data1_corr_matrix <- data.frame(data1_corr)
data2_corr_matrix <- data.frame(data2_corr)
data3_corr_matrix <- data.frame(data3_corr)
data4_corr_matrix <- data.frame(data4_corr)
data5_corr_matrix <- data.frame(data5_corr)


write.csv(data1_corr_matrix, "data1_corr_matrix.csv", row.names = FALSE)
write.csv(data2_corr_matrix, "data2_corr_matrix.csv", row.names = FALSE)
write.csv(data3_corr_matrix, "data3_corr_matrix.csv", row.names = FALSE)
write.csv(data4_corr_matrix, "data4_corr_matrix.csv", row.names = FALSE)
write.csv(data5_corr_matrix, "data5_corr_matrix.csv", row.names = FALSE)


# Vizualize
library(ggplot2)
library(reshape2)

data1_corr_long <- melt(data1_corr)
data2_corr_long <- melt(data2_corr)
data3_corr_long <- melt(data3_corr)
data4_corr_long <- melt(data4_corr)
data5_corr_long <- melt(data5_corr)

# Create the heatmap plot
ggplot(data1_corr_long, aes(x = Var1, y = Var2,fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6),
        axis.text.y = element_text(size = 4)) +
  labs(fill = "Correlation") +
  labs(title = "Correlation Matrix 1")

ggplot(data2_corr_long, aes(x = Var1, y = Var2,fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6),
        axis.text.y = element_text(size = 4)) +
  labs(fill = "Correlation") +
  labs(title = "Correlation Matrix 2")

ggplot(data3_corr_long, aes(x = Var1, y = Var2,fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6),
        axis.text.y = element_text(size = 4)) +
  labs(fill = "Correlation") +
  labs(title = "Correlation Matrix 3")

ggplot(data4_corr_long, aes(x = Var1, y = Var2,fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6),
        axis.text.y = element_text(size = 4)) +
  labs(fill = "Correlation") +
  labs(title = "Correlation Matrix 4")

ggplot(data5_corr_long, aes(x = Var1, y = Var2,fill = value)) +
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 6),
        axis.text.y = element_text(size = 4)) +
  labs(fill = "Correlation") +
  labs(title = "Correlation Matrix 5")


###Thresholding
diag(data1_corr_matrix) <- NA
diag(data2_corr_matrix) <- NA
diag(data3_corr_matrix) <- NA
diag(data4_corr_matrix) <- NA
diag(data5_corr_matrix) <- NA


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
thresholds <- c(80, 70, 75, 65, 55)

# Get variable names meeting each threshold
tasks_by_threshold1 <- lapply(thresholds, function(t) tasks_above_threshold(data1_corr_matrix, t))
names(tasks_by_threshold1) <- paste0(thresholds, "%")

tasks_by_threshold2 <- lapply(thresholds, function(t) tasks_above_threshold(data2_corr_matrix, t))
names(tasks_by_threshold2) <- paste0(thresholds, "%")

tasks_by_threshold3 <- lapply(thresholds, function(t) tasks_above_threshold(data3_corr_matrix, t))
names(tasks_by_threshold3) <- paste0(thresholds, "%")

tasks_by_threshold4 <- lapply(thresholds, function(t) tasks_above_threshold(data4_corr_matrix, t))
names(tasks_by_threshold4) <- paste0(thresholds, "%")

tasks_by_threshold5 <- lapply(thresholds, function(t) tasks_above_threshold(data5_corr_matrix, t))
names(tasks_by_threshold5) <- paste0(thresholds, "%")

for (name in names(tasks_by_threshold1)) {
  write.csv(tasks_by_threshold1[[name]], file = paste0(name, ".csv"), row.names = FALSE)
}

#############
data55_1 <- read.csv('55data_by_threshold1.csv')
data55_2 <- read.csv('55data_by_threshold2.csv')
data55_3 <- read.csv('55data_by_threshold3.csv')
data55_4 <- read.csv('55data_by_threshold4.csv')
data55_5 <- read.csv('55data_by_threshold5.csv')

# Your list of dataframes
data55_list <- list(data55_1, data55_2, data55_3, data55_4, data55_5)

# Create a vector of all pairs (directional) and their source dataframes
all_pairs <- c()  # Initialize empty vector
all_sources <- c()  # Initialize empty vector to store dataframes

# Loop through the dataframes
for (i in seq_along(data55_list)) {
  df <- data55_list[[i]]
  df_name <- paste0("data55_", i)  # Name for each dataframe, e.g., df70_1, df70_2, etc.
  
  if (!("Row" %in% names(df) && "Column" %in% names(df))) {
    stop("Each dataframe must have 'Row' and 'Column' columns.")
  }
  
  # Normalize the pairs: make (A, B) same as (B, A)
  normalized_pairs <- mapply(function(a, b) {
    paste(sort(c(a, b)), collapse = "|")
  }, df$Row, df$Column)
  
  all_pairs <- c(all_pairs, normalized_pairs)
  all_sources <- c(all_sources, rep(df_name, length(normalized_pairs)))
}

# Count frequency of each directional pair
pair_counts <- as.data.frame(table(all_pairs))
colnames(pair_counts) <- c("pair", "count")

# Create a dataframe with pairs and their corresponding dataframes
pair_sources <- data.frame(pair = all_pairs, source_df = all_sources, stringsAsFactors = FALSE)

# For each pair, count how many times it appears and in which dataframes
library(dplyr)

data55_threshold <- pair_sources %>%
  group_by(pair) %>%
  summarise(
    count = n()/2,
    dataframes = paste(sort(unique(source_df)), collapse = ", "),
    .groups = "drop"
  ) %>%
  arrange(desc(count))

data80_threshold$row <- sapply(strsplit(data80_threshold$pair, "\\|"), `[`, 1)
data80_threshold$column <- sapply(strsplit(data80_threshold$pair, "\\|"), `[`, 2)

write.csv(data80_threshold, "data80_threshold.csv", row.names = FALSE)

ggplot(data55_threshold, aes(x = pair, y = count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_hline(yintercept = 2.5, color = "hotpink", size = 1) +
  theme_minimal() +
  labs(title = "55 Threshold Pair Frequencies", x = "Pair", y = "Count") +
  theme(
    axis.text.x = element_text(angle = 78, hjust = 1, size = 1)
  )
