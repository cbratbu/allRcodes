##### LOAD THE DATA
# Get the list of CSV files in the directory
control_langloc <- list.files("control_langloc", pattern = "*.csv", full.names = TRUE)
control_langlisten <- list.files("control_langlisten", pattern = "*.csv", full.names = TRUE)
control_movie <- list.files("control_movie", pattern = "*.csv", full.names = TRUE)
control_rest <- list.files("control_rest", pattern = "*.csv", full.names = TRUE)
control_story <- list.files("control_story", pattern = "*.csv", full.names = TRUE)
pwa_langloc <- list.files("pwa_langloc", pattern = "*.csv", full.names = TRUE)
pwa_langlisten <- list.files("pwa_langlisten", pattern = "*.csv", full.names = TRUE)
pwa_movie <- list.files("pwa_movie", pattern = "*.csv", full.names = TRUE)
pwa_rest <- list.files("pwa_rest", pattern = "*.csv", full.names = TRUE)
pwa_story <- list.files("pwa_story", pattern = "*.csv", full.names = TRUE)

# Load the CSV files into a list of data frames
control_langloc_list <- lapply(control_langloc, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
control_langlisten_list <- lapply(control_langlisten, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
control_movie_list <- lapply(control_movie, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
control_rest_list <- lapply(control_rest, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
control_story_list <- lapply(control_story, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
pwa_langloc_list <- lapply(pwa_langloc, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
pwa_langlisten_list <- lapply(pwa_langlisten, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
pwa_movie_list <- lapply(pwa_movie, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
pwa_rest_list <- lapply(pwa_rest, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})
pwa_story_list <- lapply(pwa_story, function(file) {
  read.csv(file, header = FALSE)  # header = FALSE means no header row
})

# Optionally, if you want to name each data frame according to the file name
names(control_langloc_list) <- basename(control_langloc)
names(control_langlisten_list) <- basename(control_langlisten)
names(control_movie_list) <- basename(control_movie)
names(control_rest_list) <- basename(control_rest)
names(control_story_list) <- basename(control_story)
names(pwa_langloc_list) <- basename(pwa_langloc)
names(pwa_langlisten_list) <- basename(pwa_langlisten)
names(pwa_movie_list) <- basename(pwa_movie)
names(pwa_rest_list) <- basename(pwa_rest)
names(pwa_story_list) <- basename(pwa_story)


##### Replace NaN
# Apply the function to each data frame in the list
control_langloc_list <- lapply(control_langloc_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
control_langlisten_list <- lapply(control_langlisten_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
control_movie_list <- lapply(control_movie_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
control_rest_list <- lapply(control_rest_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
control_story_list <- lapply(control_story_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
pwa_langloc_list <- lapply(pwa_langloc_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
pwa_langlisten_list <- lapply(pwa_langlisten_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
pwa_movie_list <- lapply(pwa_movie_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
pwa_rest_list <- lapply(pwa_rest_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})
pwa_story_list <- lapply(pwa_story_list, function(df) {
  df[] <- lapply(df, function(col) {
    col[is.nan(col)] <- 1  # Replace NaN with 1 in each column
    return(col)
  })
  return(df)
})


##### CALCULATE WHOLE BRAIN AVG
# Compute the average of all values in each data frame
control_langloc_wb_averages <- lapply(control_langloc_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_wb_averages <- lapply(control_langlisten_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_wb_averages <- lapply(control_movie_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_wb_averages <- lapply(control_rest_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_wb_averages <- lapply(control_story_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_wb_averages <- lapply(pwa_langloc_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_wb_averages <- lapply(pwa_langlisten_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_wb_averages <- lapply(pwa_movie_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_wb_averages <- lapply(pwa_rest_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_wb_averages <- lapply(pwa_story_list, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})

#####MAKE WB DF
#get average values
control_langloc_wb <- unlist(lapply(control_langloc_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_wb <- unlist(lapply(control_langlisten_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_wb <- unlist(lapply(control_movie_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_wb <- unlist(lapply(control_rest_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_wb <- unlist(lapply(control_story_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_wb <- unlist(lapply(pwa_langloc_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_wb <- unlist(lapply(pwa_langlisten_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_wb <- unlist(lapply(pwa_movie_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_wb <- unlist(lapply(pwa_rest_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_wb <- unlist(lapply(pwa_story_wb_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

# Make whole brain sheets
control <- rep("Control", 180)
pwa <- rep("PWA", 160)
group <- c(control,pwa)

control_langloc <- rep("langloc", 36)
control_langlisten <- rep("langlisten", 36)
control_movie <- rep("movie", 36)
control_rest <- rep("rest", 36)
control_story <- rep("story", 36)
pwa_langloc <- rep("langloc", 32)
pwa_langlisten <- rep("langlisten", 32)
pwa_movie <- rep("movie", 32)
pwa_rest <- rep("rest", 32)
pwa_story <- rep("story", 32)
task <- c(control_langloc,control_langlisten,control_movie,control_rest,control_story,
          pwa_langloc,pwa_langlisten,pwa_movie,pwa_rest,pwa_story)

wholebrain_fc <- c(control_langloc_wb,control_langlisten_wb,control_movie_wb,control_rest_wb,
                   control_story_wb,pwa_langloc_wb,pwa_langlisten_wb,pwa_movie_wb,pwa_rest_wb,
                   pwa_story_wb)

wholebrain <- data.frame(wholebrain_fc,task,group)

##### WHOLE BRAIN MODELS
wholebrain$task <- factor(wholebrain$task, ordered = FALSE)
wholebrain$task <- relevel(wholebrain$task, ref = "langloc") 

wholebrain_lm <- lm(formula = wholebrain_fc ~ group * task, data = wholebrain)
summary(wholebrain_lm)

ggplot(wholebrain, aes(task, wholebrain_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Whole Brain Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))



##### ORGANIZE DATA SET
rename_columns <- function(df) {
  colnames(df) <- c("DefaultModeMPFC","DefaultModeLeftLP","DefaultModeRightLP","DefaultModePCC",
                    "SensorimotorLeftLateral","SensorimotorRightLateral","SensorimotorSuperior","VisualMedial",
                    "VisualOccipital","VisualLeftLateral","VisualRightLateral","SalienceACC","SalienceLeftAntInsula",
                    "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                    "SalienceRightSMG","DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                    "DorsalAttentionRightIPS","FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                    "FrontoparietalRightLPFC","FrontoParietalRightPPC","LanguageLeftIFG","LanguageRightIFG",
                    "LanguageLeftPSTG","LanguageRightPSTG","CerebellarAnterior","CerebellarPosterior")
  return(df)
}

control_langloc_list <- lapply(control_langloc_list, rename_columns)
control_langlisten_list <- lapply(control_langlisten_list, rename_columns)
control_movie_list <- lapply(control_movie_list, rename_columns)
control_rest_list <- lapply(control_rest_list, rename_columns)
control_story_list <- lapply(control_story_list, rename_columns)
pwa_langloc_list <- lapply(pwa_langloc_list, rename_columns)
pwa_langlisten_list <- lapply(pwa_langlisten_list, rename_columns)
pwa_movie_list <- lapply(pwa_movie_list, rename_columns)
pwa_rest_list <- lapply(pwa_rest_list, rename_columns)
pwa_story_list <- lapply(pwa_story_list, rename_columns)


# Vector to add as a new column
roi <- c("DefaultMode","DefaultMode","DefaultMode","DefaultMode","Sensorimotor","Sensorimotor",
         "Sensorimotor","Visual","Visual","Visual","Visual","Salience","Salience","Salience",
         "Salience","Salience","Salience","Salience","DorsalAttention","DorsalAttention",
         "DorsalAttention","DorsalAttention","FrontoParietal","FrontoParietal","Frontoparietal",
         "FrontoParietal","Language","Language","Language","Language","Cerebellar","Cerebellar")

# Function to add the vector as a new column to each data frame
add_vector <- function(df, roi) {
  df$roi <- roi  # Add the vector as a new column
  return(df)
}

# Apply the function to each data frame in the list
control_langloc_list <- lapply(control_langloc_list, add_vector, roi = roi)
control_langlisten_list <- lapply(control_langlisten_list, add_vector, roi = roi)
control_movie_list <- lapply(control_movie_list, add_vector, roi = roi)
control_rest_list <- lapply(control_rest_list, add_vector, roi = roi)
control_story_list <- lapply(control_story_list, add_vector, roi = roi)
pwa_langloc_list <- lapply(pwa_langloc_list, add_vector, roi = roi)
pwa_langlisten_list <- lapply(pwa_langlisten_list, add_vector, roi = roi)
pwa_movie_list <- lapply(pwa_movie_list, add_vector, roi = roi)
pwa_rest_list <- lapply(pwa_rest_list, add_vector, roi = roi)
pwa_story_list <- lapply(pwa_story_list, add_vector, roi = roi)





################################################################################
##### GET NETWORK VALUES
#DMN
# Apply the transformation to each data frame in the list
control_langloc_dmn_list_filtered  <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
control_langlisten_dmn_list_filtered  <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
control_movie_dmn_list_filtered  <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
control_rest_dmn_list_filtered  <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
control_story_dmn_list_filtered  <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
pwa_langloc_dmn_list_filtered  <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
pwa_langlisten_dmn_list_filtered  <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
pwa_movie_dmn_list_filtered  <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
pwa_rest_dmn_list_filtered  <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})
pwa_story_dmn_list_filtered  <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "DefaultMode", c("DefaultModeMPFC","DefaultModeLeftLP",
                                               "DefaultModeRightLP","DefaultModePCC")]
  return(df_filtered)
})

#Sensorimotor
control_langloc_sensorimotor_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
control_langlisten_sensorimotor_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
control_movie_sensorimotor_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
control_rest_sensorimotor_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
control_story_sensorimotor_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
pwa_langloc_sensorimotor_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
pwa_langlisten_sensorimotor_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
pwa_movie_sensorimotor_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
pwa_rest_sensorimotor_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})
pwa_story_sensorimotor_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "Sensorimotor", c("SensorimotorLeftLateral","SensorimotorRightLateral",
                                                "SensorimotorSuperior")]
  return(df_filtered)
})

#Visual
control_langloc_visual_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
control_langlisten_visual_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
control_movie_visual_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
control_rest_visual_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
control_story_visual_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
pwa_langloc_visual_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
pwa_langlisten_visual_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
pwa_movie_visual_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
pwa_rest_visual_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})
pwa_story_visual_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "Visual", c("VisualMedial","VisualOccipital","VisualLeftLateral",
                                          "VisualRightLateral")]
  return(df_filtered)
})

#Salience
control_langloc_salience_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
control_langlisten_salience_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
control_movie_salience_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
control_rest_salience_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
control_story_salience_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
pwa_langloc_salience_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
pwa_langlisten_salience_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
pwa_movie_salience_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
pwa_rest_salience_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})
pwa_story_salience_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "Salience", c("SalienceACC","SalienceLeftAntInsula",
                                            "SalienceRightAntInsula","SalienceLeftRPFC","SalienceRightRPFC","SalienceLeftSMG",
                                            "SalienceRightSMG")]
  return(df_filtered)
})

#DorsalAttention
control_langloc_dorsalattention_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
control_langlisten_dorsalattention_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
control_movie_dorsalattention_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
control_rest_dorsalattention_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
control_story_dorsalattention_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
pwa_langloc_dorsalattention_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
pwa_langlisten_dorsalattention_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
pwa_movie_dorsalattention_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
pwa_rest_dorsalattention_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})
pwa_story_dorsalattention_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "DorsalAttention", c("DorsalAttentionLeftFEF","DorsalAttentionRightFEF","DorsalAttentionLeftIPS",
                                                   "DorsalAttentionRightIPS")]
  return(df_filtered)
})

#FrontoParietal
control_langloc_frontoparietal_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
control_langlisten_frontoparietal_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
control_movie_frontoparietal_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
control_rest_frontoparietal_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
control_story_frontoparietal_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
pwa_langloc_frontoparietal_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
pwa_langlisten_frontoparietal_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
pwa_movie_frontoparietal_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
pwa_rest_frontoparietal_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})
pwa_story_frontoparietal_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "FrontoParietal", c("FrontoParietalLeftLPFC","FrontoParietalLeftPPC",
                                                  "FrontoparietalRightLPFC","FrontoParietalRightPPC")]
  return(df_filtered)
})

#Language
control_langloc_language_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
control_langlisten_language_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
control_movie_language_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
control_rest_language_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
control_story_language_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
pwa_langloc_language_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
pwa_langlisten_language_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
pwa_movie_language_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
pwa_rest_language_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})
pwa_story_language_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "Language", c("LanguageLeftIFG","LanguageRightIFG",
                                            "LanguageLeftPSTG","LanguageRightPSTG")]
  return(df_filtered)
})

#Cerebellar
control_langloc_cerebellar_list_filtered <- lapply(control_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
control_langlisten_cerebellar_list_filtered <- lapply(control_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
control_movie_cerebellar_list_filtered <- lapply(control_movie_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
control_rest_cerebellar_list_filtered <- lapply(control_rest_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
control_story_cerebellar_list_filtered <- lapply(control_story_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
pwa_langloc_cerebellar_list_filtered <- lapply(pwa_langloc_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
pwa_langlisten_cerebellar_list_filtered <- lapply(pwa_langlisten_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
pwa_movie_cerebellar_list_filtered <- lapply(pwa_movie_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
pwa_rest_cerebellar_list_filtered <- lapply(pwa_rest_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})
pwa_story_cerebellar_list_filtered <- lapply(pwa_story_list, function(df) {
  df_filtered <- df[df$roi == "Cerebellar", c("CerebellarAnterior","CerebellarPosterior")]
  return(df_filtered)
})


#####
#get average values
control_langloc_dmn_averages <- lapply(control_langloc_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_dmn_averages <- lapply(control_langlisten_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_dmn_averages <- lapply(control_movie_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_dmn_averages <- lapply(control_rest_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_dmn_averages <- lapply(control_story_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_dmn_averages <- lapply(pwa_langloc_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_dmn_averages <- lapply(pwa_langlisten_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_dmn_averages <- lapply(pwa_movie_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_dmn_averages <- lapply(pwa_rest_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_dmn_averages <- lapply(pwa_story_dmn_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_sensorimotor_averages <- lapply(control_langloc_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_sensorimotor_averages <- lapply(control_langlisten_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_sensorimotor_averages <- lapply(control_movie_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_sensorimotor_averages <- lapply(control_rest_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_sensorimotor_averages <- lapply(control_story_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_sensorimotor_averages <- lapply(pwa_langloc_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_sensorimotor_averages <- lapply(pwa_langlisten_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_sensorimotor_averages <- lapply(pwa_movie_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_sensorimotor_averages <- lapply(pwa_rest_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_sensorimotor_averages <- lapply(pwa_story_sensorimotor_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_visual_averages <- lapply(control_langloc_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_visual_averages <- lapply(control_langlisten_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_visual_averages <- lapply(control_movie_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_visual_averages <- lapply(control_rest_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_visual_averages <- lapply(control_story_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_visual_averages <- lapply(pwa_langloc_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_visual_averages <- lapply(pwa_langlisten_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_visual_averages <- lapply(pwa_movie_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_visual_averages <- lapply(pwa_rest_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_visual_averages <- lapply(pwa_story_visual_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_dorsalattention_averages <- lapply(control_langloc_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_dorsalattention_averages <- lapply(control_langlisten_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_dorsalattention_averages <- lapply(control_movie_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_dorsalattention_averages <- lapply(control_rest_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_dorsalattention_averages <- lapply(control_story_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_dorsalattention_averages <- lapply(pwa_langloc_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_dorsalattention_averages <- lapply(pwa_langlisten_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_dorsalattention_averages <- lapply(pwa_movie_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_dorsalattention_averages <- lapply(pwa_rest_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_dorsalattention_averages <- lapply(pwa_story_dorsalattention_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_frontoparietal_averages <- lapply(control_langloc_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_frontoparietal_averages <- lapply(control_langlisten_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_frontoparietal_averages <- lapply(control_movie_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_frontoparietal_averages <- lapply(control_rest_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_frontoparietal_averages <- lapply(control_story_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_frontoparietal_averages <- lapply(pwa_langloc_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_frontoparietal_averages <- lapply(pwa_langlisten_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_frontoparietal_averages <- lapply(pwa_movie_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_frontoparietal_averages <- lapply(pwa_rest_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_frontoparietal_averages <- lapply(pwa_story_frontoparietal_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_language_averages <- lapply(control_langloc_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_language_averages <- lapply(control_langlisten_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_language_averages <- lapply(control_movie_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_language_averages <- lapply(control_rest_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_language_averages <- lapply(control_story_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_language_averages <- lapply(pwa_langloc_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_language_averages <- lapply(pwa_langlisten_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_language_averages <- lapply(pwa_movie_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_language_averages <- lapply(pwa_rest_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_language_averages <- lapply(pwa_story_language_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_salience_averages <- lapply(control_langloc_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_salience_averages <- lapply(control_langlisten_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_salience_averages <- lapply(control_movie_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_salience_averages <- lapply(control_rest_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_salience_averages <- lapply(control_story_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_salience_averages <- lapply(pwa_langloc_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_salience_averages <- lapply(pwa_langlisten_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_salience_averages <- lapply(pwa_movie_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_salience_averages <- lapply(pwa_rest_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_salience_averages <- lapply(pwa_story_salience_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langloc_cerebellar_averages <- lapply(control_langloc_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_langlisten_cerebellar_averages <- lapply(control_langlisten_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_movie_cerebellar_averages <- lapply(control_movie_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_rest_cerebellar_averages <- lapply(control_rest_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
control_story_cerebellar_averages <- lapply(control_story_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langloc_cerebellar_averages <- lapply(pwa_langloc_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_langlisten_cerebellar_averages <- lapply(pwa_langlisten_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_movie_cerebellar_averages <- lapply(pwa_movie_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_rest_cerebellar_averages <- lapply(pwa_rest_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})
pwa_story_cerebellar_averages <- lapply(pwa_story_cerebellar_list_filtered, function(df) {
  # Flatten the data frame to a vector of numeric values and calculate the mean
  mean(as.numeric(unlist(df)), na.rm = TRUE)
})


#####
control_langloc_dmn <- unlist(lapply(control_langloc_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_dmn <- unlist(lapply(control_langlisten_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_dmn <- unlist(lapply(control_movie_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_dmn <- unlist(lapply(control_rest_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_dmn <- unlist(lapply(control_story_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_dmn <- unlist(lapply(pwa_langloc_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_dmn <- unlist(lapply(pwa_langlisten_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_dmn <- unlist(lapply(pwa_movie_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_dmn <- unlist(lapply(pwa_rest_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_dmn <- unlist(lapply(pwa_story_dmn_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_sensorimotor <- unlist(lapply(control_langloc_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_sensorimotor <- unlist(lapply(control_langlisten_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_sensorimotor <- unlist(lapply(control_movie_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_sensorimotor <- unlist(lapply(control_rest_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_sensorimotor <- unlist(lapply(control_story_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_sensorimotor <- unlist(lapply(pwa_langloc_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_sensorimotor <- unlist(lapply(pwa_langlisten_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_sensorimotor <- unlist(lapply(pwa_movie_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_sensorimotor <- unlist(lapply(pwa_rest_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_sensorimotor <- unlist(lapply(pwa_story_sensorimotor_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_visual <- unlist(lapply(control_langloc_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_visual <- unlist(lapply(control_langlisten_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_visual <- unlist(lapply(control_movie_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_visual <- unlist(lapply(control_rest_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_visual <- unlist(lapply(control_story_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_visual <- unlist(lapply(pwa_langloc_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_visual <- unlist(lapply(pwa_langlisten_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_visual <- unlist(lapply(pwa_movie_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_visual <- unlist(lapply(pwa_rest_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_visual <- unlist(lapply(pwa_story_visual_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_dorsalattention <- unlist(lapply(control_langloc_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_dorsalattention <- unlist(lapply(control_langlisten_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_dorsalattention <- unlist(lapply(control_movie_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_dorsalattention <- unlist(lapply(control_rest_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_dorsalattention <- unlist(lapply(control_story_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_dorsalattention <- unlist(lapply(pwa_langloc_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_dorsalattention <- unlist(lapply(pwa_langlisten_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_dorsalattention <- unlist(lapply(pwa_movie_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_dorsalattention <- unlist(lapply(pwa_rest_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_dorsalattention <- unlist(lapply(pwa_story_dorsalattention_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_frontoparietal <- unlist(lapply(control_langloc_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_frontoparietal <- unlist(lapply(control_langlisten_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_frontoparietal <- unlist(lapply(control_movie_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_frontoparietal <- unlist(lapply(control_rest_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_frontoparietal <- unlist(lapply(control_story_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_frontoparietal <- unlist(lapply(pwa_langloc_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_frontoparietal <- unlist(lapply(pwa_langlisten_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_frontoparietal <- unlist(lapply(pwa_movie_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_frontoparietal <- unlist(lapply(pwa_rest_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_frontoparietal <- unlist(lapply(pwa_story_frontoparietal_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_language <- unlist(lapply(control_langloc_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_language <- unlist(lapply(control_langlisten_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_language <- unlist(lapply(control_movie_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_language <- unlist(lapply(control_rest_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_language <- unlist(lapply(control_story_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_language <- unlist(lapply(pwa_langloc_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_language <- unlist(lapply(pwa_langlisten_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_language <- unlist(lapply(pwa_movie_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_language <- unlist(lapply(pwa_rest_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_language <- unlist(lapply(pwa_story_language_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_salience <- unlist(lapply(control_langloc_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_salience <- unlist(lapply(control_langlisten_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_salience <- unlist(lapply(control_movie_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_salience <- unlist(lapply(control_rest_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_salience <- unlist(lapply(control_story_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_salience <- unlist(lapply(pwa_langloc_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_salience <- unlist(lapply(pwa_langlisten_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_salience <- unlist(lapply(pwa_movie_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_salience <- unlist(lapply(pwa_rest_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_salience <- unlist(lapply(pwa_story_salience_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))

control_langloc_cerebellar <- unlist(lapply(control_langloc_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_langlisten_cerebellar <- unlist(lapply(control_langlisten_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_movie_cerebellar <- unlist(lapply(control_movie_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_rest_cerebellar <- unlist(lapply(control_rest_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
control_story_cerebellar <- unlist(lapply(control_story_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langloc_cerebellar <- unlist(lapply(pwa_langloc_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_langlisten_cerebellar <- unlist(lapply(pwa_langlisten_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_movie_cerebellar <- unlist(lapply(pwa_movie_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_rest_cerebellar <- unlist(lapply(pwa_rest_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))
pwa_story_cerebellar <- unlist(lapply(pwa_story_cerebellar_averages, function(df) {
  # Flatten each data frame and return as a vector
  as.numeric(unlist(df))
}))


defaultmodenetwork_fc <- c(control_langloc_defaultmode,control_langlisten_defaultmode,control_movie_defaultmode,control_rest_defaultmode,
                           control_story_defaultmode,pwa_langloc_defaultmode,pwa_langlisten_defaultmode,pwa_movie_defaultmode,pwa_rest_defaultmode,
                           pwa_story_defaultmode)
sensorimotor_fc <- c(control_langloc_sensorimotor,control_langlisten_sensorimotor,control_movie_sensorimotor,control_rest_sensorimotor,
                     control_story_sensorimotor,pwa_langloc_sensorimotor,pwa_langlisten_sensorimotor,pwa_movie_sensorimotor,pwa_rest_sensorimotor,
                     pwa_story_sensorimotor)
visual_fc <- c(control_langloc_visual,control_langlisten_visual,control_movie_visual,control_rest_visual,
               control_story_visual,pwa_langloc_visual,pwa_langlisten_visual,pwa_movie_visual,pwa_rest_visual,
               pwa_story_visual)
dorsalattention_fc <- c(control_langloc_dorsalattention,control_langlisten_dorsalattention,control_movie_dorsalattention,control_rest_dorsalattention,
                        control_story_dorsalattention,pwa_langloc_dorsalattention,pwa_langlisten_dorsalattention,pwa_movie_dorsalattention,pwa_rest_dorsalattention,
                        pwa_story_dorsalattention)
frontoparietal_fc <- c(control_langloc_frontoparietal,control_langlisten_frontoparietal,control_movie_frontoparietal,control_rest_frontoparietal,
                       control_story_frontoparietal,pwa_langloc_frontoparietal,pwa_langlisten_frontoparietal,pwa_movie_frontoparietal,pwa_rest_frontoparietal,
                       pwa_story_frontoparietal)
language_fc <- c(control_langloc_language,control_langlisten_language,control_movie_language,control_rest_language,
                 control_story_language,pwa_langloc_language,pwa_langlisten_language,pwa_movie_language,pwa_rest_language,
                 pwa_story_language)
salience_fc <- c(control_langloc_salience,control_langlisten_salience,control_movie_salience,control_rest_salience,
                 control_story_salience,pwa_langloc_salience,pwa_langlisten_salience,pwa_movie_salience,pwa_rest_salience,
                 pwa_story_salience)
cerebellar_fc <- c(control_langloc_cerebellar,control_langlisten_cerebellar,control_movie_cerebellar,control_rest_cerebellar,
                   control_story_cerebellar,pwa_langloc_cerebellar,pwa_langlisten_cerebellar,pwa_movie_cerebellar,pwa_rest_cerebellar,
                   pwa_story_cerebellar)




dmn <- data.frame(dmn_fc,task,group)
sensorimotor <- data.frame(sens_fc,task,group)
visual <- data.frame(vis_fc,task,group)
dorsalattention <- data.frame(da_fc,task,group)
frontoparietal <- data.frame(front_fc,task,group)
language <- data.frame(lang_fc,task,group)
salience <- data.frame(salience_fc,task,group)
cerebellar <- data.frame(cere_fc,task,group)


##### NETWORK MODELS
dmn$task <- factor(dmn$task, ordered = FALSE)
dmn$task <- relevel(dmn$task, ref = "langloc")

sensorimotor$task <- factor(sensorimotor$task, ordered = FALSE)
sensorimotor$task <- relevel(sensorimotor$task, ref = "langloc")

visual$task <- factor(visual$task, ordered = FALSE)
visual$task <- relevel(visual$task, ref = "langloc")

dorsalattention$task <- factor(dorsalattention$task, ordered = FALSE)
dorsalattention$task <- relevel(dorsalattention$task, ref = "langloc")

frontoparietal$task <- factor(frontoparietal$task, ordered = FALSE)
frontoparietal$task <- relevel(frontoparietal$task, ref = "langloc")

language$task <- factor(language$task, ordered = FALSE)
language$task <- relevel(language$task, ref = "langloc")

salience$task <- factor(salience$task, ordered = FALSE)
salience$task <- relevel(salience$task, ref = "langloc")

cerebellar$task <- factor(cerebellar$task, ordered = FALSE)
cerebellar$task <- relevel(cerebellar$task, ref = "langloc")

defaultmode_lm <- lm(formula = defaultmode_fc ~ group * task, data = defaultmode)
summary(defaultmode_lm)

sensorimotor_lm <- lm(formula = sens_fc ~ group * task, data = sensorimotor)
summary(sensorimotor_lm)

visual_lm <- lm(formula = vis_fc ~ group * task, data = visual)
summary(visual_lm)

dorsalattention_lm <- lm(formula = da_fc ~ group * task, data = dorsalattention)
summary(dorsalattention_lm)

frontoparietal_lm <- lm(formula = front_fc ~ group * task, data = frontoparietal)
summary(frontoparietal_lm)

language_lm <- lm(formula = lang_fc ~ group * task, data = language)
summary(language_lm)

salience_lm <- lm(formula = sal_fc ~ group * task, data = salience)
summary(salience_lm)

cerebellar_lm <- lm(formula = cere_fc ~ group * task, data = cerebellar)
summary(cerebellar_lm)


#### PLOTS
ggplot(defaultmode, aes(task, defaultmode_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Default Mode Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(sensorimotor, aes(task, sensorimotor_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Sensorimotor Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(visual, aes(task, visual_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Visual Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(dorsalattention, aes(task, dorsalattention_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Dorsal Attention Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(frontoparietal, aes(task, frontoparietal_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Frontoparietal Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(language, aes(task, language_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Language Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(salience, aes(task, salience_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Salience Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))

ggplot(cerebellar, aes(task, cerebellar_fc)) +
  geom_boxplot(aes(fill = group)) +
  ylim(0,2) +
  labs(x = 'Task', y = 'Average FC', title = 'Cerebellar Network Average FC') +
  scale_x_discrete(labels = c(langloc = 'Vis. Struct. Language', 
                              langlisten = 'Aud. Struct. Language', 
                              movie = 'Movie Watching', rest = 'Resting State',
                              story = 'Story Listening')) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust = .9))


### CORRECTING FOR MULITPLE COMPARISONS
p <- c(2e-20,0.0187,0.1005,0.2228,0.0660,0.4310,0.2745,0.2061,0.0894,0.6883,2e-20,0.3480,
       0.2852,0.2047,0.0483,0.3508,0.9270,0.3203,0.6014,0.7306,2e-20,0.4150,0.2057,0.2611,
       0.0743,0.1822,0.4948,0.3629,0.5726,0.8190,2e-20,0.205,0.552,0.861,0.975,0.269,0.481,
       0.917,0.586,0.760,2e-20,0.000224,0.506989,0.759086,0.284199,0.609773,0.314513,
       0.456241,0.378476,0.823775,2e-20,0.000186,0.261178,0.693281,0.672193,0.220283,
       0.413464,0.203333,0.119202,0.753957,2e-20,0.000339,0.153669,0.104489,0.409792,
       0.183591,0.336422,0.063737,0.773002,0.311833,2e-20,0.0191,0.0276,0.1564,0.1804,
       0.1709,0.2990,0.2010,0.3098,0.6672,2e-20,0.00291,0.19940,0.23337,0.15020,0.99568,
       0.21338,0.12034,0.34919,0.84868)
label <- c("wholebrain_intercept","wholebrain_group","wholebrain_langlisten",
           "wholebrain_movie","wholebrain_rest","wholebrain_story","wholebrain_pwalanglisten",
           "wholebrain_pwamovie","wholebrain_pwarest","wholebrain_pwastory",
           "cerebellar_intercept","cerebellar_group","cerebellar_langlisten",
           "cerebellar_movie","cerebellar_rest","cerebellar_story","cerebellar_pwalanglisten",
           "cerebellar_pwamovie","cerebellar_pwarest","cerebellar_pwastory","vis_intercept",
           "vis_group","vis_langlisten","vis_movie","vis_rest","vis_story","vis_pwalanglisten",
           "vis_pwamovie","vis_pwarest","vis_pwastory","dmn_intercept","dmn_group",
           "dmn_langlisten","dmn_movie","dmn_rest","dmn_story","dmn_pwalanglisten",
           "dmn_pwamovie","dmn_pwarest","dmn_pwastory","lang_intercept","lang_group",
           "lang_langlisten","lang_movie","lang_rest","lang_story","lang_pwalanglisten",
           "lang_pwamovie","lang_pwarest","lang_pwastory","sal_intercept","sal_group",
           "sal_langlisten","sal_movie","sal_rest","sal_story","sal_pwalanglisten",
           "sal_pwamovie","sal_pwarest","sal_pwastory","sens_intercept","sens_group",
           "sens_langlisten","sens_movie","sens_rest","sens_story","sens_pwalanglisten",
           "sens_pwamovie","sens_pwarest","sens_pwastory","da_intercept","da_group",
           "da_langlisten","da_movie","da_rest","da_story","da_pwalanglisten","da_pwamovie",
           "da_pwarest","da_pwastory","fp_intercept","fp_group","fp_langlisten","fp_movie",
           "fp_rest","fp_story","fp_pwalanglisten","fp_pwamovie","fp_pwarest","fp_pwastory")
p_bh <- p.adjust(p, method = "BH", n = length(p))

label <- data.frame(label)
pdf <- data.frame(p_bh,label)



wab <- c("42.8","31.5","84.2","46.7","93","94.9","92.7","89","99.6","79.8","85.3","90","94.6","99.1",
         "66.3","80.1","61.7","88.4","98.1","65.9","95.2","43.1","96","71.2","92.9","79.9",
         "95.4","12.4","85.7","94.2","72.4","97.2","42.8","31.5","84.2","46.7","93","94.9","92.7",
         "89","99.6","79.8","85.3","90","94.6","99.1","66.3","80.1","61.7","88.4","98.1",
         "65.9","95.2","43.1","96","71.2","92.9","79.9","95.4","12.4","85.7","94.2","72.4","97.2",
         "42.8","31.5","84.2","46.7","93","94.9","92.7","89","99.6","79.8","85.3","90","94.6","99.1",
         "66.3","80.1","61.7","88.4","98.1","65.9","95.2","43.1","96","71.2","92.9","79.9",
         "95.4","12.4","85.7","94.2","72.4","97.2","42.8","31.5","84.2","46.7","93","94.9","92.7",
         "89","99.6","79.8","85.3","90","94.6","99.1","66.3","80.1","61.7","88.4","98.1","65.9",
         "95.2","43.1","96","71.2","92.9","79.9","95.4","12.4","85.7","94.2","72.4","97.2","42.8",
         "31.5","84.2","46.7","93","94.9","92.7","89","99.6","79.8","85.3","90","94.6","99.1",
         "66.3","80.1","61.7","88.4","98.1","65.9","95.2","43.1","96","71.2","92.9","79.9",
         "95.4","12.4","85.7","94.2","72.4","97.2")

wab_task <- c("langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc",
              "langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc",
              "langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc","langloc",
              "langloc","langloc","langloc","langlisten","langlisten","langlisten","langlisten","langlisten","langlisten",
              "langlisten","langlisten","langlisten","langlisten","langlisten","langlisten","langlisten",
              "langlisten","langlisten","langlisten","langlisten","langlisten","langlisten","langlisten",
              "langlisten","langlisten","langlisten","langlisten","langlisten","langlisten","langlisten",
              "langlisten","langlisten","langlisten","langlisten","langlisten","movie","movie","movie","movie",
              "movie","movie","movie","movie","movie","movie","movie","movie","movie","movie","movie","movie",
              "movie","movie","movie","movie","movie","movie","movie","movie","movie","movie","movie","movie",
              "movie","movie","movie","movie","story","story","story","story","story","story","story","story",
              "story","story","story","story","story","story","story","story","story","story","story","story",
              "story","story","story","story","story","story","story","story","story","story","story","story",
              "rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest",
              "rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest","rest",
              "rest","rest","rest","rest")

lesion_0 <- c("210333","333891","90078","240229","265784","18681","97934","62031","6064","104055","95842",
              "73720","104791","2100","273059","75631","111894","280181","23892","137140","159105","187230",
              "83580","58450","103856","28135","2033","310091","155909","65550","141008","22024","210333","333891","90078","240229","265784","18681","97934","62031","6064","104055","95842",
              "73720","104791","2100","273059","75631","111894","280181","23892","137140","159105","187230",
              "83580","58450","103856","28135","2033","310091","155909","65550","141008","22024","210333","333891","90078","240229","265784","18681","97934","62031","6064","104055","95842",
              "73720","104791","2100","273059","75631","111894","280181","23892","137140","159105","187230",
              "83580","58450","103856","28135","2033","310091","155909","65550","141008","22024","210333","333891","90078","240229","265784","18681","97934","62031","6064","104055","95842",
              "73720","104791","2100","273059","75631","111894","280181","23892","137140","159105","187230",
              "83580","58450","103856","28135","2033","310091","155909","65550","141008","22024","210333","333891","90078","240229","265784","18681","97934","62031","6064","104055","95842",
              "73720","104791","2100","273059","75631","111894","280181","23892","137140","159105","187230",
              "83580","58450","103856","28135","2033","310091","155909","65550","141008","22024")

langfc_wab <- c(pwa_langloc_language,pwa_langlisten_language,pwa_movie_language,pwa_rest_language,
                pwa_story_language)

df <- data.frame(langfc_wab = langfc_wab,wab_task = wab_task,lesion_0 = lesion_0,wab = wab)
df$wab <- as.numeric(df$wab)
df$lesion <- as.numeric(df$lesion)

ggplot(df, aes(wab, langfc_wab)) +
  geom_point(aes(col = wab_task)) +
  labs(x = 'WAB AQ', y = 'Average FC', title = 'Language Network Average FC') +
  theme_bw() +
  ylim(0,2) +
  geom_smooth(method=lm,se=FALSE, aes(col = wab_task))
