library(dplyr)
library(ggplot2)

#load data
listen_listen <- read.csv('listen_listen_spm_ss_mROI_data.csv')
listen_math <- read.csv('listen_math_spm_ss_mROI_data.csv')
#listen_pattern <- read.csv('listen_pattern_spm_ss_mROI_data.csv')
listen_spwm <- read.csv('listen_spwm_spm_ss_mROI_data.csv')
listen_read <- read.csv('listen_read_spm_ss_mROI_data.csv')

math_listen <- read.csv('math_listen_spm_ss_mROI_data.csv')
math_math <- read.csv('math_math_spm_ss_mROI_data.csv')
#math_pattern <- read.csv('math_pattern_spm_ss_mROI_data.csv')
math_spwm <- read.csv('math_spwm_spm_ss_mROI_data.csv')
math_read <- read.csv('math_read_spm_ss_mROI_data.csv')

#pattern_math <- read.csv('pattern_math_spm_ss_mROI_data.csv')
#pattern_listen <- read.csv('pattern_listen_spm_ss_mROI_data.csv')
#pattern_read <- read.csv('pattern_read_spm_ss_mROI_data.csv')

spwm_listen <- read.csv('spwm_listen_spm_ss_mROI_data.csv')
spwm_math <- read.csv('spwm_math_spm_ss_mROI_data.csv')
spwm_spwm <- read.csv('spwm_spwm_spm_ss_mROI_data.csv')
spwm_read <- read.csv('spwm_read_spm_ss_mROI_data.csv')

read_listen <- read.csv('read_listen_spm_ss_mROI_data.csv')
read_math <- read.csv('read_math_spm_ss_mROI_data.csv')
#read_pattern <- read.csv('read_pattern_spm_ss_mROI_data.csv')
read_spwm <- read.csv('read_spwm_spm_ss_mROI_data.csv')
read_read <- read.csv('read_read_spm_ss_mROI_data.csv')

#Clean data NOTE: you may have to adjust what is commented out based on tasks that were done
listen_listen$Effect[listen_listen$Effect == "Intact"] <- "Intact_listen"
listen_listen$Effect[listen_listen$Effect == "Degr"] <- "Degr_listen"
listen_math$Effect[listen_math$Effect == "H"] <- "H_math"
listen_math$Effect[listen_math$Effect == "E"] <- "E_math"
#listen_pattern$Effect[listen_pattern$Effect == "H"] <- "H_pattern"
#listen_pattern$Effect[listen_pattern$Effect == "E"] <- "E_pattern"
listen_spwm$Effect[listen_spwm$Effect == "H"] <- "H_spwm"
listen_spwm$Effect[listen_spwm$Effect == "E"] <- "E_spwm"
listen_read$Effect[listen_read$Effect == "S"] <- "S_read"
listen_read$Effect[listen_read$Effect == "N"] <- "N_read"

math_listen$Effect[math_listen$Effect == "Intact"] <- "Intact_listen"
math_listen$Effect[math_listen$Effect == "Degr"] <- "Degr_listen"
math_math$Effect[math_math$Effect == "H"] <- "H_math"
math_math$Effect[math_math$Effect == "E"] <- "E_math"
#math_pattern$Effect[math_pattern$Effect == "H"] <- "H_pattern"
#math_pattern$Effect[math_pattern$Effect == "E"] <- "E_pattern"
math_spwm$Effect[math_spwm$Effect == "H"] <- "H_spwm"
math_spwm$Effect[math_spwm$Effect == "E"] <- "E_spwm"
math_read$Effect[math_read$Effect == "S"] <- "S_read"
math_read$Effect[math_read$Effect == "N"] <- "N_read"

pattern_listen$Effect[pattern_listen$Effect == "Intact"] <- "Intact_listen"
pattern_listen$Effect[pattern_listen$Effect == "Degr"] <- "Degr_listen"
pattern_math$Effect[pattern_math$Effect == "H"] <- "H_math"
pattern_math$Effect[pattern_math$Effect == "E"] <- "E_math"
pattern_read$Effect[pattern_read$Effect == "S"] <- "S_read"
pattern_read$Effect[pattern_read$Effect == "N"] <- "N_read"

spwm_listen$Effect[spwm_listen$Effect == "Intact"] <- "Intact_listen"
spwm_listen$Effect[spwm_listen$Effect == "Degr"] <- "Degr_listen"
spwm_math$Effect[spwm_math$Effect == "H"] <- "H_math"
spwm_math$Effect[spwm_math$Effect == "E"] <- "E_math"
spwm_spwm$Effect[spwm_spwm$Effect == "H"] <- "H_spwm"
spwm_spwm$Effect[spwm_spwm$Effect == "E"] <- "E_spwm"
spwm_read$Effect[spwm_read$Effect == "S"] <- "S_read"
spwm_read$Effect[spwm_read$Effect == "N"] <- "N_read"

read_listen$Effect[read_listen$Effect == "Intact"] <- "Intact_listen"
read_listen$Effect[read_listen$Effect == "Degr"] <- "Degr_listen"
read_math$Effect[read_math$Effect == "H"] <- "H_math"
read_math$Effect[read_math$Effect == "E"] <- "E_math"
#read_pattern$Effect[read_pattern$Effect == "H"] <- "H_pattern"
#read_pattern$Effect[read_pattern$Effect == "E"] <- "E_pattern"
read_spwm$Effect[read_spwm$Effect == "H"] <- "H_spwm"
read_spwm$Effect[read_spwm$Effect == "E"] <- "E_spwm"
read_read$Effect[read_read$Effect == "S"] <- "S_read"
read_read$Effect[read_read$Effect == "N"] <- "N_read"

## Combine table
listen <- bind_rows(listen_listen,listen_math,listen_spwm,listen_read)
math <- bind_rows(math_listen,math_math,math_read,math_spwm)
read <- bind_rows(read_listen,read_math,read_spwm,read_read)
spwm <- bind_rows(spwm_listen,spwm_math,spwm_read,spwm_spwm)

listen_lh <- listen[grepl("^LH_", listen$ROI), ]
listen_rh <- listen[grepl("^RH_", listen$ROI), ]
math_lh <- math[grepl("^LH_", math$ROI), ]
math_rh <- math[grepl("^RH_", math$ROI), ]
read_lh <- read[grepl("^LH_", read$ROI), ]
read_rh <- read[grepl("^RH_", read$ROI), ]
spwm_lh <- spwm[grepl("^LH_", spwm$ROI), ]
spwm_rh <- spwm[grepl("^RH_", spwm$ROI), ]

#Order tasks NOTE: you may have to change these to include/exclude tasks based on which tasks were done
listen_lh$Effect <- factor(listen_lh$Effect, levels = c("Intact_listen",
                                                          "Degr_listen",
                                                          "S_read","N_read",
                                                          "E_spwm","H_spwm",
                                                          "E_math","H_math"))

listen_rh$Effect <- factor(listen_rh$Effect, levels = c("Intact_listen",
                                                        "Degr_listen",
                                                        "S_read","N_read",
                                                        "E_spwm","H_spwm",
                                                        "E_math","H_math"))

read_lh$Effect <- factor(read_lh$Effect, levels = c("Intact_listen",
                                                    "Degr_listen",
                                                    "S_read","N_read",
                                                    "E_spwm","H_spwm",
                                                    "E_math","H_math"))

read_rh$Effect <- factor(read_rh$Effect, levels = c("Intact_listen",
                                                    "Degr_listen",
                                                    "S_read","N_read",
                                                    "E_spwm","H_spwm",
                                                    "E_math","H_math"))

spwm_lh$Effect <- factor(spwm_lh$Effect, levels = c("Intact_listen",
                                                          "Degr_listen",
                                                          "S_read","N_read",
                                                          "E_spwm","H_spwm",
                                                          "E_math","H_math"))

spwm_rh$Effect <- factor(spwm_rh$Effect, levels = c("Intact_listen",
                                                          "Degr_listen",
                                                          "S_read","N_read",
                                                          "E_spwm","H_spwm",
                                                          "E_math","H_math"))

math_lh$Effect <- factor(math_lh$Effect, levels = c("Intact_listen",
                                                    "Degr_listen",
                                                    "S_read","N_read",
                                                    "E_spwm","H_spwm",
                                                    "E_math","H_math"))

math_rh$Effect <- factor(math_rh$Effect, levels = c("Intact_listen",
                                                    "Degr_listen",
                                                    "S_read","N_read",
                                                    "E_spwm","H_spwm",
                                                    "E_math","H_math"))



#plot data NOTE: change this to get the different plots
ggplot(spwm_rh, aes(x = ROI, y = EffectSize, fill = Effect)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "BUBA149 spWM Effect Size by RH ROI",
       x = "Region of Interest (ROI)",
       y = "Effect Size") +
  theme_minimal() +
  scale_y_continuous(limits = c(-2, 4.5))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

