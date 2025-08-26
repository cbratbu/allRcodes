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