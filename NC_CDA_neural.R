# List all CSV files in the directory
csv_files <- list.files(pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
data_list <- lapply(csv_files, read.csv, header = FALSE)

names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

# Function to replace NaN values with 1 in a data frame
replace_nan_with_1 <- function(df) {
  # Apply is.nan() to each column individually and replace NaN with 1
  df[] <- lapply(df, function(x) { x[is.nan(x)] <- 1; return(x) })
  return(df)
}

# Apply the function to each data frame in the list
data_list <- lapply(data_list, replace_nan_with_1)

# Calculate the average of each df
averages <- lapply(data_list, function(df) mean(as.numeric(unlist(df)), na.rm = TRUE))

write.csv(averages, "whole_brain_connectivity.csv", row.names = FALSE)

#####
wholebrain_t <- read.csv("whole_brain_connectivity.csv")
network <- read.csv("FC_17network.csv")

# Transpose the data frame
wholebrain <- as.data.frame(t(wholebrain_t))

# Store original column names in a new column called 'ID'
wholebrain$ID <- rownames(wholebrain_t)

# Reorder columns so ID comes first
wholebrain <- wholebrain[, c(ncol(wholebrain), 1:(ncol(wholebrain)-1))]

wholebrain$ID <- rownames(wholebrain)

wholebrain$wholebrain <- wholebrain$V1
  
lesionvol <- read.csv("lesion_vol.csv")

library(dplyr)

# Merge the 'wholebrain' column from 'wb' into 'networks' using 'ID' as the key
network <- network %>%
  left_join(wholebrain %>% select(ID, wholebrain), by = "ID")

# Merge the 'wholebrain' column from 'wb' into 'networks' using 'ID' as the key
neural <- network %>%
  left_join(lesionvol %>% select(ID, volume_mm3), by = "ID")

neural <- neural %>%
  left_join(lesionvol %>% select(ID, BU_ID), by = "ID")

membership <- read.csv("membership.csv")

membership$BU_ID <- membership$ID

neural <- neural %>%
  left_join(membership %>% select(BU_ID, membership_louvain), by = "BU_ID")

neural <- neural[-41, ]

write.csv(neural, "neural.csv", row.names = FALSE)



###WAB###
neural <- read.csv("neural.csv")

wab_membership <- membership %>%
  left_join(WAB %>% select(ID,WAB_AQ), by = "ID")

# Fit a multinomial logistic regression model
wab_membership$membership_louvain <- factor(wab_membership$membership_louvain, ordered = FALSE)
wab_membership$membership_louvain <- relevel(wab_membership$membership_louvain, ref = "1")

wab_model1 <- multinom(membership_louvain ~ WAB_AQ, data = wab_membership)
wab_model2 <- multinom(membership_louvain ~ WAB_AQ, data = wab_membership)
wab_model3 <- multinom(membership_louvain ~ WAB_AQ, data = wab_membership)

# View the model summary
summary(wab_model1)
summary(wab_model2)
summary(wab_model3)

# Get p-values using z-tests (because summary doesn't show them by default)
wab_z1 <- summary(wab_model1)$coefficients / summary(wab_model1)$standard.errors
wab_p_values1 <- 2 * (1 - pnorm(abs(wab_z1)))
wab_p_values1

wab_z2 <- summary(wab_model2)$coefficients / summary(wab_model2)$standard.errors
wab_p_values2 <- 2 * (1 - pnorm(abs(wab_z2)))
wab_p_values2

wab_z3 <- summary(wab_model3)$coefficients / summary(wab_model3)$standard.errors
wab_p_values3 <- 2 * (1 - pnorm(abs(wab_z3)))
wab_p_values3

wab_p_vec1 <- as.vector(wab_p_values1)
wab_p_vec2 <- as.vector(wab_p_values2)
wab_p_vec3 <- as.vector(wab_p_values3)

wab_p_all <- c(wab_p_vec1, wab_p_vec2, wab_p_vec3)
wab_p_all_bh <- p.adjust(wab_p_all, method = "BH")
wab_p_all_bh

wab_p_all <- data.frame(wab_p_all)
wab_p_all_bh <- data.frame(wab_p_all_bh)
wab_p_all$wab_p_all_bh <- wab_p_all_bh$wab_p_all_bh





##
wab_anova <- aov(WAB_AQ ~ as.factor(membership_louvain), data = wab_membership)
summary(wab_anova)

library(emmeans)
emm <- emmeans(wab_anova, ~ membership_louvain)

# Marginal means for task by membership
emm_pair <- pairs(emm, adjust = "bh")
emm_pair <- as.data.frame(emm_pair)

emm_pair_sig <- subset(emm_pair, p.value < 0.05)
write.csv(emm_pair_sig,"lmem_pair_sig.csv")



write.csv(emm_pair_sig,"lmem_pair_sig.csv")

