library(ggplot2)
library(dplyr)

data <- read.csv('membership_imputed_pooled.csv')


set.seed(123)  # For reproducibility

# Parameters
n_perm <- 10000

# Get task columns (all except 'group')
task_cols <- setdiff(names(data),"membership_gn_pos")

# Function to compute p-value via permutation ANOVA
perm_anova <- function(task, data, group_col = "membership_gn_pos", n_perm = 10000) {
  # Extract variables
  values <- data[[task]]
  groups <- data[[group_col]]
  
  # Observed F-statistic
  aov_result <- aov(values ~ groups)
  F_obs <- summary(aov_result)[[1]][["F value"]][1]
  
  # Permutation
  F_perm <- numeric(n_perm)
  for (i in 1:n_perm) {
    shuffled <- sample(values)
    perm_result <- aov(shuffled ~ groups)
    F_perm[i] <- summary(perm_result)[[1]][["F value"]][1]
  }
  
  # Permutation-based p-value
  p_val <- mean(F_perm >= F_obs)
  
  return(c(F_stat = F_obs, p_value = p_val))
}

# Apply to all tasks
task_cols <- task_cols[!(task_cols %in% "ID")]
results <- sapply(task_cols, perm_anova, data = data, n_perm = n_perm)
results <- as.data.frame(t(results))
results$task <- rownames(results)
rownames(results) <- NULL

write.csv(results, "anova_results.csv")


# Ensure 'group' is a factor
data$membership_gn_pos <- as.factor(data$membership_gn_pos)

# Compute mean values per group
MEC_emot_comprehension_angry <- data %>%
  group_by(membership_gn_pos) %>%
  summarise(mean_MEC_emot_comprehension_angry  = mean(`MEC_emot_comprehension_angry`, na.rm = TRUE))

cluster_colors <- c("1" = "orange2", "2" = "skyblue2", "3" = "green4", "4" = "yellow2")

# Plot
ggplot(MEC_emot_comprehension_angry , aes(x = membership_gn_pos, y = mean_MEC_emot_comprehension_angry , fill = membership_gn_pos)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = cluster_colors) +
  labs(
    x = "Group",
    y = "MEC Emot. Comp. - Angry "
  ) +
  theme_minimal() +
  theme(legend.position = "none")




###
library(dplyr)
library(tidyr)
library(ggplot2)

# Filter for membership_gn_pos == 4
filtered3 <- data %>% filter(membership_gn_pos == 3)

# Convert to long format
filtered3_long <- filtered3 %>%
  pivot_longer(
    cols = -c(ID, membership_gn_pos),
    names_to = "task",
    values_to = "value"
  )

# Calculate mean for each task and reorder factor levels
task_means3 <- filtered3_long %>%
  group_by(task) %>%
  summarise(mean_value = mean(value, na.rm = TRUE)) %>%
  arrange(desc(mean_value))

# Update factor levels based on descending mean
filtered3_long$task <- factor(filtered3_long$task, levels = task_means3$task)

# Plot
ggplot(filtered3_long, aes(x = task, y = value)) +
  geom_boxplot(outlier.size = 0.8, outlier.alpha = 0.4) +
  theme_minimal() +
  labs(
    title = "Cluster 3",
    x = "Task (ordered by mean performance)",
    y = "Value"
  ) +
  theme(
    axis.text.x = element_text(angle = 70, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
