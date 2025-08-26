# List all CSV files in the directory
csv_files <- list.files(pattern = "*.csv", full.names = TRUE)

# Read all CSV files into a list of data frames
data_list <- lapply(csv_files, read.csv, header = FALSE)

names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

column_names <- c("DefaultA","DefaultA","DefaultA","DefaultA","DefaultA","DefaultA",
                  "DefaultA","DefaultA","DefaultA","DefaultA","DefaultA","DefaultA",
                  "DefaultA","DefaultA","DefaultB","DefaultB","DefaultB","DefaultB",
                  "DefaultB","DefaultB","DefaultB","DefaultB","DefaultB","DefaultB",
                  "DefaultB","DefaultB","DefaultB","DefaultB","DefaultB","DefaultB",
                  "DefaultC","DefaultC","DefaultC","DefaultC","DefaultC","DefaultC",
                  "DefaultC","DefaultC","DefaultC","DefaultC","DefaultC","Language",
                  "Language","Language","Language","Language","Language","Language",
                  "Language","Language","Language","ContA","ContA","ContA","ContA",
                  "ContA","ContA","ContA","ContA","ContA","ContA","ContA","ContA",
                  "ContB","ContB","ContB","ContB","ContB","ContB","ContB","ContB",
                  "ContB","ContB","ContB","ContB","ContC","ContC","ContC","ContC",
                  "ContC","ContC","ContC","ContC","ContC","SalVenAttnA","SalVenAttnA",
                  "SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA",
                  "SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnB",
                  "SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB",
                  "SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB",
                  "SalVenAttnB","SalVenAttnB","DorsAttnA","DorsAttnA","DorsAttnA",
                  "DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA",
                  "DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA",
                  "DorsAttnA","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB",
                  "DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB",
                  "DorsAttnB","Aud","Aud","Aud","Aud","Aud","Aud","Aud","Aud","Aud","Aud",
                  "SomMotA","SomMotA","SomMotA","SomMotA","SomMotA","SomMotA","SomMotA",
                  "SomMotA","SomMotA","SomMotA","SomMotA","SomMotA","SomMotA","SomMotB",
                  "SomMotB","SomMotB","SomMotB","SomMotB","SomMotB","SomMotB","SomMotB",
                  "SomMotB","SomMotB","SomMotB","SomMotB","VisualA","VisualA","VisualA",
                  "VisualA","VisualA","VisualA","VisualA","VisualA","VisualA","VisualA",
                  "VisualA","VisualA","VisualA","VisualB","VisualB","VisualB","VisualB",
                  "VisualB","VisualB","VisualB","VisualB","VisualB","VisualB","VisualB",
                  "VisualB","VisualC","VisualC","VisualC","VisualC","DefaultA","DefaultA",
                  "DefaultA","DefaultA","DefaultA","DefaultA","DefaultA","DefaultA",
                  "DefaultA","DefaultA","DefaultA","DefaultA","DefaultA","DefaultA",
                  "DefaultB","DefaultB","DefaultB","DefaultB","DefaultB","DefaultB",
                  "DefaultB","DefaultB","DefaultB","DefaultB","DefaultB","DefaultC",
                  "DefaultC","DefaultC","DefaultC","DefaultC","DefaultC","DefaultC",
                  "DefaultC","DefaultC","DefaultC","DefaultC","Language","Language",
                  "Language","Language","Language","Language","Language","Language","ContA",
                  "ContA","ContA","ContA","ContA","ContA","ContA","ContA","ContA","ContA",
                  "ContA","ContA","ContA","ContB","ContB","ContB","ContB","ContB","ContB",
                  "ContB","ContB","ContB","ContB","ContB","ContB","ContB","ContC","ContC",
                  "ContC","ContC","ContC","ContC","ContC","ContC","ContC","ContC","ContC",
                  "ContC","ContC","ContC","SalVenAttnA","SalVenAttnA","SalVenAttnA",
                  "SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA",
                  "SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA","SalVenAttnA",
                  "SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB",
                  "SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB",
                  "SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB","SalVenAttnB",
                  "DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA",
                  "DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnA","DorsAttnB",
                  "DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB",
                  "DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","DorsAttnB","Aud","Aud",
                  "Aud","Aud","Aud","Aud","Aud","Aud","Aud","SomMotA","SomMotA","SomMotA",
                  "SomMotA","SomMotA","SomMotA","SomMotA","SomMotA","SomMotA","SomMotA",
                  "SomMotA","SomMotA","SomMotA","SomMotB","SomMotB","SomMotB","SomMotB",
                  "SomMotB","SomMotB","SomMotB","SomMotB","SomMotB","SomMotB","SomMotB",
                  "VisualA","VisualA","VisualA","VisualA","VisualA","VisualA","VisualA",
                  "VisualA","VisualA","VisualA","VisualA","VisualA","VisualA","VisualA",
                  "VisualA","VisualB","VisualB","VisualB","VisualB","VisualB","VisualB",
                  "VisualB","VisualB","VisualB","VisualB","VisualB","VisualB","VisualB",
                  "VisualC","VisualC","VisualC","VisualC")

data_list <- lapply(data_list, function(df) {
  colnames(df) <- column_names[1:ncol(df)]  # Ensure we only assign as many names as columns
  return(df)
})

ROI <- column_names

# Add the new variable to all data frames in the list
add_character_vector <- function(df, ROI) {
  # Check if the new_var length matches the number of rows in the data frame
  if (length(ROI) == nrow(df)) {
    df$ROI <- ROI
  } else {
    warning("Length of new_var does not match the number of rows in the data frame.")
  }
  return(df)
}

data_list <- lapply(data_list, add_character_vector, ROI = ROI)


# Function to replace NaN values with 1 in a data frame
replace_nan_with_1 <- function(df) {
  # Apply is.nan() to each column individually and replace NaN with 1
  df[] <- lapply(df, function(x) { x[is.nan(x)] <- 1; return(x) })
  return(df)
}

# Apply the function to each data frame in the list
data_list <- lapply(data_list, replace_nan_with_1)


# Function to compute averages for 'language' columns and others where ROI is "md"
average_VisualC <- function(df) {
  # Filter data where ROI is 'language'
  filtered_df <- df[df$ROI == "VisualC", ]
  
  # Select the 'language' columns
  language_columns <- grep("^VisualC", colnames(df), value = TRUE)
  
  # Calculate the mean of all 'language' columns for rows where ROI is 'language'
  avg_values <- colMeans(filtered_df[, language_columns], na.rm = TRUE)
  
  # Now compute the average of those average values
  avg <- mean(avg_values, na.rm = TRUE)
  
  return(avg)
}

# Apply the function to each data frame in the list and store the results
VisualC_fc <- lapply(data_list, average_VisualC)


ControlA_fc <- data.frame(ControlA_fc)
ControlB_fc <- data.frame(ControlB_fc)
ControlC_fc <- data.frame(ControlC_fc)
DefaultA_fc <- data.frame(DefaultA_fc)
DefaultB_fc <- data.frame(DefaultB_fc)
DefaultC_fc <- data.frame(DefaultC_fc)
DorsAttnA_fc <- data.frame(DorsAttnA_fc)
DorsAttnB_fc <- data.frame(DorsAttnB_fc)
SalVenAttnA_fc <- data.frame(SalVenAttnA_fc)
SalVenAttnB_fc <- data.frame(SalVenAttnB_fc)
Language_fc <- data.frame(Language_fc)
Aud_fc <- data.frame(Aud_fc)
SomMotA_fc <- data.frame(SomMotA_fc)
SomMotB_fc <- data.frame(SomMotB_fc)
VisualA_fc <- data.frame(VisualA_fc)
VisualB_fc <- data.frame(VisualB_fc)
VisualC_fc <- data.frame(VisualC_fc)

ControlA_fc <- ControlA_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
ControlB_fc <- ControlB_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
ControlC_fc <- ControlC_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
DefaultA_fc <- DefaultA_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
DefaultB_fc <- DefaultB_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
DefaultC_fc <- DefaultC_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
DorsAttnA_fc <- DorsAttnA_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
DorsAttnB_fc <- DorsAttnB_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
SalVenAttnA_fc <- SalVenAttnA_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
SalVenAttnB_fc <- SalVenAttnB_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
Language_fc <- Language_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
SomMotA_fc <- SomMotA_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
SomMotB_fc <- SomMotB_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
VisualA_fc <- VisualA_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
VisualB_fc <- VisualB_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
VisualC_fc <- VisualC_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "FC")
Aud_fc <- Aud_fc %>%
  pivot_longer(cols = everything(),  # This selects all columns
               names_to = "ID",     # The variable names will go into this column
               values_to = "Aud_fc")


ControlA_fc <- data.frame(ControlA_fc)
ControlB_fc <- data.frame(ControlB_fc)
ControlC_fc <- data.frame(ControlC_fc)
DefaultA_fc <- data.frame(DefaultA_fc)
DefaultB_fc <- data.frame(DefaultB_fc)
DefaultC_fc <- data.frame(DefaultC_fc)
DorsAttnA_fc <- data.frame(DorsAttnA_fc)
DorsAttnB_fc <- data.frame(DorsAttnB_fc)
SalVenAttnA_fc <- data.frame(SalVenAttnA_fc)
SalVenAttnB_fc <- data.frame(SalVenAttnB_fc)
Language_fc <- data.frame(Language_fc)

ControlA_fc <- rename(ControlA_fc, ControlA_fc = FC)
ControlB_fc <- rename(ControlB_fc, ControlB_fc = FC)
ControlC_fc <- rename(ControlC_fc, ControlC_fc = FC)
DefaultA_fc <- rename(DefaultA_fc, DefaultA_fc = FC)
DefaultB_fc <- rename(DefaultB_fc, DefaultB_fc = FC)
DefaultC_fc <- rename(DefaultC_fc, DefaultC_fc = FC)
DorsAttnA_fc <- rename(DorsAttnA_fc, DorsAttnA_fc = FC)
DorsAttnB_fc <- rename(DorsAttnB_fc, DorsAttnB_fc = FC)
Language_fc <- rename(Language_fc, Language_fc = FC)
SalVenAttnA_fc <- rename(SalVenAttnA_fc, SalVenAttnA_fc = FC)
SalVenAttnB_fc <- rename(SalVenAttnB_fc, SalVenAttnB_fc = FC)
SomMotA_fc <- rename(SomMotA_fc, SomMotA_fc = FC)
SomMotB_fc <- rename(SomMotB_fc, SomMotB_fc = FC)
VisualA_fc <- rename(VisualA_fc, VisualA_fc = FC)
VisualB_fc <- rename(VisualB_fc, VisualB_fc = FC)
VisualC_fc <- rename(VisualC_fc, VisualC_fc = FC)

FC_17network <- ControlA_fc %>%
  left_join(ControlB_fc, by = "ID") %>%
  left_join(ControlC_fc, by = "ID")%>%
  left_join(Aud_fc, by = "ID")%>%
  left_join(DefaultA_fc, by = "ID")%>%
  left_join(DefaultB_fc, by = "ID")%>%
  left_join(DefaultC_fc, by = "ID")%>%
  left_join(DorsAttnA_fc, by = "ID")%>%
  left_join(DorsAttnB_fc, by = "ID")%>%
  left_join(Language_fc, by = "ID")%>%
  left_join(SalVenAttnA_fc, by = "ID")%>%
  left_join(SalVenAttnB_fc, by = "ID")%>%
  left_join(SomMotA_fc, by = "ID")%>%
  left_join(SomMotB_fc, by = "ID")%>%
  left_join(VisualA_fc, by = "ID")%>%
  left_join(VisualB_fc, by = "ID")%>%
  left_join(VisualC_fc, by = "ID")


write.csv(FC_17network, "FC_17network.csv", row.names = FALSE)

#Linear Models
ContA_ContA_FC <- lm(FC ~ group, data = ControlA_fc)
summary(ContA_ContA_FC)

ContB_ContB_FC <- lm(FC ~ group, data = ControlB_fc)
summary(ContB_ContB_FC)

ContC_ContC_FC <- lm(FC ~ group, data = ControlC_fc)
summary(ContC_ContC_FC)

DefaultA_DefaultA_FC <- lm(FC ~ group, data = DefaultA_fc)
summary(DefaultA_DefaultA_FC)

DefaultB_DefaultB_FC <- lm(FC ~ group, data = DefaultB_fc)
summary(DefaultB_DefaultB_FC)

DefaultC_DefaultC_FC <- lm(FC ~ group, data = DefaultC_fc)
summary(DefaultC_DefaultC_FC)

DorsAttnA_DorsAttnA_FC <- lm(FC ~ group, data = DorsAttnA_fc)
summary(DorsAttnA_DorsAttnA_FC)

DorsAttnB_DorsAttnB_FC <- lm(FC ~ group, data = DorsAttnB_fc)
summary(DorsAttnB_DorsAttnB_FC)

SalVenAttnA_SalVenAttnA_FC <- lm(FC ~ group, data = SalVenAttnA_fc)
summary(SalVenAttnA_SalVenAttnA_FC)

SalVenAttnB_SalVenAttnB_FC <- lm(FC ~ group, data = SalVenAttnB_fc)
summary(SalVenAttnB_SalVenAttnB_FC)

Language_Language_FC <- lm(FC ~ group, data = Language_fc)
summary(Language_Language_FC)

# Load the ggplot2 package
library(ggplot2)

# Plots
ggplot(ControlA_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Control A Network") +
  theme_bw()

ggplot(ControlB_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Control B Network") +
  theme_bw()

ggplot(ControlC_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Control C Network") +
  theme_bw()

ggplot(DefaultA_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Default A Network") +
  theme_bw()

ggplot(DefaultB_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Default B Network") +
  theme_bw()

ggplot(DefaultC_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Default C Network") +
  theme_bw()

ggplot(DorsAttnA_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the DorsAttn A Network") +
  theme_bw()

ggplot(DorsAttnB_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the DorsAttn B Network") +
  theme_bw()

ggplot(Language_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the Language Network") +
  theme_bw()

ggplot(SalVenAttnA_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the SalVenAttn A Network") +
  theme_bw()

ggplot(SalVenAttnB_fc,aes(x = group, y = FC)) +
  geom_boxplot() +
  labs(title = "Intranetwork FC of the SalVenAttn B Network") +
  theme_bw()

  