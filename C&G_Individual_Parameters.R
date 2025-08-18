library(singcar)
library(psycho)
library(tidyverse)
library(ggplot2)
library(viridis)
library(splitstackshape)
df <- data.frame(CGMod6)

####spanish P1
#P1 parameter values from Model 6 -- intrinsic
P1_Spa_LIFG_LMTG_A <- df[1,]$Spa_LIFG.LMTG_A
P1_Spa_RIFG_RMTG_A <- df[1,]$Spa_RIFG.RMTG_A
P1_Spa_LIFG_RIFG_A <- df[1,]$Spa_LIFG.RIFG_A
P1_Spa_RIFG_LIFG_A <- df[1,]$Spa_RIFG.LIFG_A

#P1 parameter values from Model 6 -- task-modulated
P1_Spa_LIFG_LMTG_B <- df[1,]$Spa_LIFG.LMTG_B
P1_Spa_RIFG_RMTG_B <- df[1,]$Spa_RIFG.RMTG_B
P1_Spa_LIFG_RIFG_B <- df[1,]$Spa_LIFG.RIFG_B
P1_Spa_RIFG_LIFG_B <- df[1,]$Spa_RIFG.LIFG_B

#P1 age
P1_Age <- df[1,]$Age

#####english P1
#P1 parameter values from Model 6 -- intrinsic
P1_Eng_LIFG_LMTG_A <- df[1,]$Eng_LIFG.LMTG_A
P1_Eng_RIFG_RMTG_A <- df[1,]$Eng_RIFG.RMTG_A
P1_Eng_LIFG_RIFG_A <- df[1,]$Eng_LIFG.RIFG_A
P1_Eng_RIFG_LIFG_A <- df[1,]$Eng_RIFG.LIFG_A

#P1 parameter values from Model 6 -- task-modulated
P1_Eng_LIFG_LMTG_B <- df[1,]$Eng_LIFG.LMTG_B
P1_Eng_RIFG_RMTG_B <- df[1,]$Eng_RIFG.RMTG_B
P1_Eng_LIFG_RIFG_B <- df[1,]$Eng_LIFG.RIFG_B
P1_Eng_RIFG_LIFG_B <- df[1,]$Eng_RIFG.LIFG_B

########################################################

#####spanish P2
#P2 parameter values from Model 6 -- intrinsic
P2_Spa_LIFG_LMTG_A <- df[2,]$Spa_LIFG.LMTG_A
P2_Spa_RIFG_RMTG_A <- df[2,]$Spa_RIFG.RMTG_A
P2_Spa_LIFG_RIFG_A <- df[2,]$Spa_LIFG.RIFG_A
P2_Spa_RIFG_LIFG_A <- df[2,]$Spa_RIFG.LIFG_A

#P2 parameter values from Model 6 -- task-modulated
P2_Spa_LIFG_LMTG_B <- df[2,]$Spa_LIFG.LMTG_B
P2_Spa_RIFG_RMTG_B <- df[2,]$Spa_RIFG.RMTG_B
P2_Spa_LIFG_RIFG_B <- df[2,]$Spa_LIFG.RIFG_B
P2_Spa_RIFG_LIFG_B <- df[2,]$Spa_RIFG.LIFG_B

#P2 age
P2_Age <- df[2,]$Age

#####english P2
#P2 parameter values from Model 6 -- intrinsic
P2_Eng_LIFG_LMTG_A <- df[2,]$Eng_LIFG.LMTG_A
P2_Eng_RIFG_RMTG_A <- df[2,]$Eng_RIFG.RMTG_A
P2_Eng_LIFG_RIFG_A <- df[2,]$Eng_LIFG.RIFG_A
P2_Eng_RIFG_LIFG_A <- df[2,]$Eng_RIFG.LIFG_A

#P2 parameter values from Model 6 -- task-modulated
P2_Eng_LIFG_LMTG_B <- df[2,]$Eng_LIFG.LMTG_B
P2_Eng_RIFG_RMTG_B <- df[2,]$Eng_RIFG.RMTG_B
P2_Eng_LIFG_RIFG_B <- df[2,]$Eng_LIFG.RIFG_B
P2_Eng_RIFG_LIFG_B <- df[2,]$Eng_RIFG.LIFG_B

########################################################

#####spanish P3
#P3 parameter values from Model 6 -- intrinsic
P3_Spa_LIFG_LMTG_A <- df[3,]$Spa_LIFG.LMTG_A
P3_Spa_RIFG_RMTG_A <- df[3,]$Spa_RIFG.RMTG_A
P3_Spa_LIFG_RIFG_A <- df[3,]$Spa_LIFG.RIFG_A
P3_Spa_RIFG_LIFG_A <- df[3,]$Spa_RIFG.LIFG_A

#P3 parameter values from Model 6 -- task-modulated
P3_Spa_LIFG_LMTG_B <- df[3,]$Spa_LIFG.LMTG_B
P3_Spa_RIFG_RMTG_B <- df[3,]$Spa_RIFG.RMTG_B
P3_Spa_LIFG_RIFG_B <- df[3,]$Spa_LIFG.RIFG_B
P3_Spa_RIFG_LIFG_B <- df[3,]$Spa_RIFG.LIFG_B

####english P3

#P3 parameter values from Model 6 -- intrinsic
P3_Eng_LIFG_LMTG_A <- df[3,]$Eng_LIFG.LMTG_A
P3_Eng_RIFG_RMTG_A <- df[3,]$Eng_RIFG.RMTG_A
P3_Eng_LIFG_RIFG_A <- df[3,]$Eng_LIFG.RIFG_A
P3_Eng_RIFG_LIFG_A <- df[3,]$Eng_RIFG.LIFG_A

#P3 parameter values from Model 6 -- task-modulated
P3_Eng_LIFG_LMTG_B <- df[3,]$Eng_LIFG.LMTG_B
P3_Eng_RIFG_RMTG_B <- df[3,]$Eng_RIFG.RMTG_B
P3_Eng_LIFG_RIFG_B <- df[3,]$Eng_LIFG.RIFG_B
P3_Eng_RIFG_LIFG_B <- df[3,]$Eng_RIFG.LIFG_B
#P3 age
P3_Age <- df[3,]$Age

########################################################

####spanish P4
#P4 parameter values from Model 6 -- intrinsic
P4_Spa_LIFG_LMTG_A <- df[4,]$Spa_LIFG.LMTG_A
P4_Spa_RIFG_RMTG_A <- df[4,]$Spa_RIFG.RMTG_A
P4_Spa_LIFG_RIFG_A <- df[4,]$Spa_LIFG.RIFG_A
P4_Spa_RIFG_LIFG_A <- df[4,]$Spa_RIFG.LIFG_A

#P4 parameter values from Model 6 -- task-modulated
P4_Spa_LIFG_LMTG_B <- df[4,]$Spa_LIFG.LMTG_B
P4_Spa_RIFG_RMTG_B <- df[4,]$Spa_RIFG.RMTG_B
P4_Spa_LIFG_RIFG_B <- df[4,]$Spa_LIFG.RIFG_B
P4_Spa_RIFG_LIFG_B <- df[4,]$Spa_RIFG.LIFG_B

####english P4

#P3 parameter values from Model 6 -- intrinsic
P4_Eng_LIFG_LMTG_A <- df[4,]$Eng_LIFG.LMTG_A
P4_Eng_RIFG_RMTG_A <- df[4,]$Eng_RIFG.RMTG_A
P4_Eng_LIFG_RIFG_A <- df[4,]$Eng_LIFG.RIFG_A
P4_Eng_RIFG_LIFG_A <- df[4,]$Eng_RIFG.LIFG_A

#P3 parameter values from Model 6 -- task-modulated
P4_Eng_LIFG_LMTG_B <- df[4,]$Eng_LIFG.LMTG_B
P4_Eng_RIFG_RMTG_B <- df[4,]$Eng_RIFG.RMTG_B
P4_Eng_LIFG_RIFG_B <- df[4,]$Eng_LIFG.RIFG_B
P4_Eng_RIFG_LIFG_B <- df[4,]$Eng_RIFG.LIFG_B
#P4 age
P4_Age <- df[4,]$Age

####english P4

########################################################

#Controls -- spanish;  Model 6 -- intrinsic

Control_Spa_LIFG_LMTG_A <- df[5:8,]$Spa_LIFG.LMTG_A
Control_Spa_RIFG_RMTG_A <- df[5:8,]$Spa_RIFG.RMTG_A
Control_Spa_LIFG_RIFG_A <- df[5:8,]$Spa_LIFG.RIFG_A
Control_Spa_RIFG_LIFG_A <- df[5:8,]$Spa_RIFG.LIFG_A

#Control -- spanish; Model 6 task-modulated
Control_Spa_LIFG_LMTG_B <- df[5:8,]$Spa_LIFG.LMTG_B
Control_Spa_RIFG_RMTG_B <- df[5:8,]$Spa_RIFG.RMTG_B
Control_Spa_LIFG_RIFG_B <- df[5:8,]$Spa_LIFG.RIFG_B
Control_Spa_RIFG_LIFG_B <- df[5:8,]$Spa_RIFG.LIFG_B

#Control -- english; Model 6 -- intrinsic

Control_Eng_LIFG_LMTG_A <- df[5:8,]$Eng_LIFG.LMTG_A
Control_Eng_RIFG_RMTG_A <- df[5:8,]$Eng_RIFG.RMTG_A
Control_Eng_LIFG_RIFG_A <- df[5:8,]$Eng_LIFG.RIFG_A
Control_Eng_RIFG_LIFG_A <- df[5:8,]$Eng_RIFG.LIFG_A

#Control -- english; Model 6 task-modulated
Control_Eng_LIFG_LMTG_B <- df[5:8,]$Eng_LIFG.LMTG_B
Control_Eng_RIFG_RMTG_B <- df[5:8,]$Eng_RIFG.RMTG_B
Control_Eng_LIFG_RIFG_B <- df[5:8,]$Eng_LIFG.RIFG_B
Control_Eng_RIFG_LIFG_B <- df[5:8,]$Eng_RIFG.LIFG_B

#control age
Control_Age <- df[5:8,]$Age

###############################################################################

#P1 for Spanish --> intrinsic
BTD_cov(case_task=P1_Spa_LIFG_LMTG_A, case_covar=P1_Age, control_task=Control_Spa_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Spa_RIFG_RMTG_A, case_covar=P1_Age, control_task=Control_Spa_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Spa_LIFG_RIFG_A, case_covar=P1_Age, control_task=Control_Spa_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Spa_RIFG_LIFG_A, case_covar=P1_Age, control_task=Control_Spa_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P1 for Spanish --> task-mod
BTD_cov(case_task=P1_Spa_LIFG_LMTG_B, case_covar=P1_Age, control_task=Control_Spa_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Spa_RIFG_RMTG_B, case_covar=P1_Age, control_task=Control_Spa_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Spa_LIFG_RIFG_B, case_covar=P1_Age, control_task=Control_Spa_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Spa_RIFG_LIFG_B, case_covar=P1_Age, control_task=Control_Spa_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

#P1 for English --> intrinsic
BTD_cov(case_task=P1_Eng_LIFG_LMTG_A, case_covar=P1_Age, control_task=Control_Eng_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Eng_RIFG_RMTG_A, case_covar=P1_Age, control_task=Control_Eng_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Eng_LIFG_RIFG_A, case_covar=P1_Age, control_task=Control_Eng_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Eng_RIFG_LIFG_A, case_covar=P1_Age, control_task=Control_Eng_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P1 for English --> task-mod
BTD_cov(case_task=P1_Eng_LIFG_LMTG_B, case_covar=P1_Age, control_task=Control_Eng_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Eng_RIFG_RMTG_B, case_covar=P1_Age, control_task=Control_Eng_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Eng_LIFG_RIFG_B, case_covar=P1_Age, control_task=Control_Eng_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P1_Eng_RIFG_LIFG_B, case_covar=P1_Age, control_task=Control_Eng_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

BTD(case=P1_Spa_LIFG_RIFG_B, controls=Control_Spa_LIFG_RIFG_B, alternative = "two.sided")
?BTD
#####################################################################################

#P2 for Spanish --> intrinsic
BTD_cov(case_task=P2_Spa_LIFG_LMTG_A, case_covar=P2_Age, control_task=Control_Spa_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Spa_RIFG_RMTG_A, case_covar=P2_Age, control_task=Control_Spa_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Spa_LIFG_RIFG_A, case_covar=P2_Age, control_task=Control_Spa_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Spa_RIFG_LIFG_A, case_covar=P2_Age, control_task=Control_Spa_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P2 for Spanish --> task-mod
BTD_cov(case_task=P2_Spa_LIFG_LMTG_B, case_covar=P2_Age, control_task=Control_Spa_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Spa_RIFG_RMTG_B, case_covar=P2_Age, control_task=Control_Spa_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Spa_LIFG_RIFG_B, case_covar=P2_Age, control_task=Control_Spa_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Spa_RIFG_LIFG_B, case_covar=P2_Age, control_task=Control_Spa_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

#P2 for English --> intrinsic
BTD_cov(case_task=P2_Eng_LIFG_LMTG_A, case_covar=P2_Age, control_task=Control_Eng_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Eng_RIFG_RMTG_A, case_covar=P2_Age, control_task=Control_Eng_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Eng_LIFG_RIFG_A, case_covar=P2_Age, control_task=Control_Eng_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Eng_RIFG_LIFG_A, case_covar=P2_Age, control_task=Control_Eng_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P2 for English --> task-mod
BTD_cov(case_task=P2_Eng_LIFG_LMTG_B, case_covar=P2_Age, control_task=Control_Eng_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Eng_RIFG_RMTG_B, case_covar=P2_Age, control_task=Control_Eng_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Eng_LIFG_RIFG_B, case_covar=P2_Age, control_task=Control_Eng_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P2_Eng_RIFG_LIFG_B, case_covar=P2_Age, control_task=Control_Eng_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

####################################################
#P3 for Spanish --> intrinsic
BTD_cov(case_task=P3_Spa_LIFG_LMTG_A, case_covar=P3_Age, control_task=Control_Spa_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Spa_RIFG_RMTG_A, case_covar=P3_Age, control_task=Control_Spa_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Spa_LIFG_RIFG_A, case_covar=P3_Age, control_task=Control_Spa_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Spa_RIFG_LIFG_A, case_covar=P3_Age, control_task=Control_Spa_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P3 for Spanish -->3task-mod
BTD_cov(case_task=P3_Spa_LIFG_LMTG_B, case_covar=P3_Age, control_task=Control_Spa_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Spa_RIFG_RMTG_B, case_covar=P3_Age, control_task=Control_Spa_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Spa_LIFG_RIFG_B, case_covar=P3_Age, control_task=Control_Spa_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Spa_RIFG_LIFG_B, case_covar=P3_Age, control_task=Control_Spa_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

#P3 for English -->3intrinsic
BTD_cov(case_task=P3_Eng_LIFG_LMTG_A, case_covar=P3_Age, control_task=Control_Eng_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Eng_RIFG_RMTG_A, case_covar=P3_Age, control_task=Control_Eng_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Eng_LIFG_RIFG_A, case_covar=P3_Age, control_task=Control_Eng_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Eng_RIFG_LIFG_A, case_covar=P3_Age, control_task=Control_Eng_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P3 for English -->3task-mod
BTD_cov(case_task=P3_Eng_LIFG_LMTG_B, case_covar=P3_Age, control_task=Control_Eng_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Eng_RIFG_RMTG_B, case_covar=P3_Age, control_task=Control_Eng_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Eng_LIFG_RIFG_B, case_covar=P3_Age, control_task=Control_Eng_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P3_Eng_RIFG_LIFG_B, case_covar=P3_Age, control_task=Control_Eng_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

####################################################
#P4 for Spanish --> intrinsic
BTD_cov(case_task=P4_Spa_LIFG_LMTG_A, case_covar=P4_Age, control_task=Control_Spa_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Spa_RIFG_RMTG_A, case_covar=P4_Age, control_task=Control_Spa_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Spa_LIFG_RIFG_A, case_covar=P4_Age, control_task=Control_Spa_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Spa_RIFG_LIFG_A, case_covar=P4_Age, control_task=Control_Spa_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P4 for Spanish -->4task-mod
BTD_cov(case_task=P4_Spa_LIFG_LMTG_B, case_covar=P4_Age, control_task=Control_Spa_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Spa_RIFG_RMTG_B, case_covar=P4_Age, control_task=Control_Spa_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Spa_LIFG_RIFG_B, case_covar=P4_Age, control_task=Control_Spa_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Spa_RIFG_LIFG_B, case_covar=P4_Age, control_task=Control_Spa_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

#P4 for English -->4intrinsic
BTD_cov(case_task=P4_Eng_LIFG_LMTG_A, case_covar=P4_Age, control_task=Control_Eng_LIFG_LMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Eng_RIFG_RMTG_A, case_covar=P4_Age, control_task=Control_Eng_RIFG_RMTG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Eng_LIFG_RIFG_A, case_covar=P4_Age, control_task=Control_Eng_LIFG_RIFG_A, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Eng_RIFG_LIFG_A, case_covar=P4_Age, control_task=Control_Eng_RIFG_LIFG_A, control_covar = Control_Age, alternative = "two.sided")

#P4 for English -->4task-mod
BTD_cov(case_task=P4_Eng_LIFG_LMTG_B, case_covar=P4_Age, control_task=Control_Eng_LIFG_LMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Eng_RIFG_RMTG_B, case_covar=P4_Age, control_task=Control_Eng_RIFG_RMTG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Eng_LIFG_RIFG_B, case_covar=P4_Age, control_task=Control_Eng_LIFG_RIFG_B, control_covar = Control_Age, alternative = "two.sided")
BTD_cov(case_task=P4_Eng_RIFG_LIFG_B, case_covar=P4_Age, control_task=Control_Eng_RIFG_LIFG_B, control_covar = Control_Age, alternative = "two.sided")

#####################################

Control_Eng_RIFG_LIFG_B <- df[5:8,]$Eng_RIFG.LIFG_B
Patients_Eng_RIFG_LIFG_B <- df[1:4,]$Eng_RIFG.LIFG_B

df[5,]$ID <- "C1"
df[6,]$ID<- "C2"
df[7,]$ID<- "C3"
df[8,]$ID<- "C4"

ggplot(df, aes(x=ID, y=Eng_RIFG.LIFG_B, fill = Eng_RIFG.LIFG_B)) +
  scale_fill_viridis_c(name = "Hz") +theme_minimal() +
  geom_bar(stat = "identity", width = 0.75) + xlab("Participants") + ylab("Hertz")+
  ggtitle("English Ep.B for RIFG -> LIFG")
