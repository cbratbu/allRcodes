library(dplyr)

#load data
data_dir <- "~/Documents/CBR/tmp/acute"

# Get list of CSV files in the directory
csv_files <- list.files(path = data_dir, pattern = "\\.csv$", full.names = TRUE)

# Load all CSV files into a list of data frames
data_list <- lapply(csv_files, read.csv)
names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

#Clean up dfs
for (name in names(data_list)) {
  if (grepl("_listen$", name)) {
    df <- data_list[[name]]
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("Intact", "Listen_Intact", df$Effect)
    }
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("Degr", "Listen_Degr", df$Effect)
    }
    
    # Save back to the list
    data_list[[name]] <- df
  }
}

for (name in names(data_list)) {
  if (grepl("_math$", name)) {
    df <- data_list[[name]]
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("H", "Math_H", df$Effect)
    }
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("E", "Math_E", df$Effect)
    }
    
    # Save back to the list
    data_list[[name]] <- df
  }
}

for (name in names(data_list)) {
  if (grepl("_pattern$", name)) {
    df <- data_list[[name]]
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("H", "Pattern_H", df$Effect)
    }
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("E", "Pattern_E", df$Effect)
    }
    
    # Save back to the list
    data_list[[name]] <- df
  }
}

for (name in names(data_list)) {
  if (grepl("_read$", name)) {
    df <- data_list[[name]]
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("S", "Read_S", df$Effect)
    }
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("N", "Read_N", df$Effect)
    }
    
    # Save back to the list
    data_list[[name]] <- df
  }
}

for (name in names(data_list)) {
  if (grepl("_spwm$", name)) {
    df <- data_list[[name]]
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("H", "spWM_H", df$Effect)
    }
    
    # Check if 'Effect' column exists and is character
    if ("Effect" %in% names(df) && is.character(df$Effect)) {
      df$Effect <- gsub("E", "spWM_E", df$Effect)
    }
    
    # Save back to the list
    data_list[[name]] <- df
  }
}

#combine data by task
first_tasks <- c("listen", "read", "math", "pattern", "spwm")

combined_data <- list()

for (task in first_tasks) {
  # Find data frames where the second element in the name (split by "_") matches the task
  matching_names <- names(data_list)[sapply(names(data_list), function(n) {
    parts <- strsplit(n, "_")[[1]]
    length(parts) >= 2 && parts[2] == task
  })]
  
  # Combine the matching data frames if any exist
  if (length(matching_names) > 0) {
    combined_data[[task]] <- do.call(rbind, data_list[matching_names])
  }
}

# Create a new list to hold the summarized data
avg_by_effect_and_roi <- lapply(names(combined_data), function(task) {
  combined_data[[task]] %>%
    group_by(Effect, ROI) %>%
    summarise(avg_effect_size = mean(`EffectSize`, na.rm = TRUE), .groups = "drop") %>%
    mutate(task = task)
})

#Combine into one summary data frame
avg_effect_summary <- bind_rows(avg_by_effect_and_roi)

avg_effect_lh <- avg_effect_summary[grepl("^LH_", avg_effect_summary$ROI), ]
avg_effect_rh <- avg_effect_summary[grepl("^RH_", avg_effect_summary$ROI), ]


#Plot the data
library(ggplot2)

ggplot(subset(avg_effect_lh, task == "listen"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average LH Language Listen Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  scale_y_continuous(limits = c(-2, 4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_rh, task == "listen"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average RH Language Listen Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  theme_minimal() +
  scale_y_continuous(limits = c(-2, 4)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_lh, task == "read"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average LH Language Read Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  scale_y_continuous(limits = c(-1.5, 2.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_rh, task == "read"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average RH Language Read Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  theme_minimal() +
  scale_y_continuous(limits = c(-1.5, 2.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_lh, task == "math"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average LH Math Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  scale_y_continuous(limits = c(-1.5, 7)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_rh, task == "math"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average RH Math Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  theme_minimal() +
  scale_y_continuous(limits = c(-1.5, 7)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_lh, task == "spwm"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average LH spWM Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  scale_y_continuous(limits = c(-1, 4.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_rh, task == "spwm"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average RH spWM Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  theme_minimal() +
  scale_y_continuous(limits = c(-1, 4.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_lh, task == "pattern"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average LH Pattern Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(subset(avg_effect_rh, task == "pattern"), aes(x = ROI, y = avg_effect_size, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average RH Pattern Effect Size",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  theme_minimal() +
  scale_y_continuous(limits = c(-1.5, 1.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
