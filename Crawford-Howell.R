library(singcar)
library(psycho)
library(tidyverse)
library(splitstackshape)
df1 <- data.frame(Beahv)

PatientEngRT <- df1[1:4,]$EngRT
PatientSpaRT <- df1[1:4,]$SpaRT
#P1
P1EngAcc <- df1[1,]$EngAcc
P1SpaAcc <- df1[1,]$SpaAc
P1EngRT <- df1[1,]$EngRT
P1SpaRT <- df1[1,]$SpaRT
#P2
P2EngAcc <- df1[2,]$EngAcc
P2SpaAcc <- df1[2,]$SpaAc
P2EngRT <- df1[2,]$EngRT
P2SpaRT <- df1[2,]$SpaRT
#P3
P3EngAcc <- df1[3,]$EngAcc
P3SpaAcc <- df1[3,]$SpaAc
P3EngRT <- df1[3,]$EngRT
P3SpaRT <- df1[3,]$SpaRT
#P4
P4EngAcc <- df1[4,]$EngAcc
P4SpaAcc <- df1[4,]$SpaAc
P4EngRT <- df1[4,]$EngRT
P4SpaRT <- df1[4,]$SpaRT
#Controls
ControlEngAcc <- df1[5:8,]$EngAcc
ControlSpaAcc <- df1[5:8,]$SpaAc
ControlEngRT <- df1[5:8,]$EngRT
ControlSpaRT <- df1[5:8,]$SpaRT
#ages
ControlAge <- df1[5:8,]$Age
P1Age <- df1[1,]$Age
P2Age <- df1[2,]$Age
P3Age <- df1[3,]$Age
P4Age <- df1[4,]$Age

#P1
BTD_cov(case_task = P1EngAcc, case_covar = P1Age, control_task = ControlEngAcc, control_covar = ControlAge)
BTD_cov(case_task = P1SpaAcc, case_covar = P1Age, control_task = ControlSpaAcc, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P1EngRT, case_covar = P1Age, control_task = ControlEngRT, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P1SpaRT, case_covar = P1Age, control_task = ControlSpaRT, control_covar = ControlAge, alternative = "two.sided")

#P2
BTD_cov(case_task = P2EngAcc, case_covar = P2Age, control_task = ControlEngAcc, control_covar = ControlAge)
BTD_cov(case_task = P2SpaAcc, case_covar = P2Age, control_task = ControlSpaAcc, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P2EngRT, case_covar = P2Age, control_task = ControlEngRT, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P2SpaRT, case_covar = P2Age, control_task = ControlSpaRT, control_covar = ControlAge, alternative = "two.sided")
BTD(P2SpaAcc, ControlSpaAcc, alternative = "two.sided")
#P3
BTD_cov(case_task = P3EngAcc, case_covar = P3Age, control_task = ControlEngAcc, control_covar = ControlAge)
BTD_cov(case_task = P3SpaAcc, case_covar = P3Age, control_task = ControlSpaAcc, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P3EngRT, case_covar = P3Age, control_task = ControlEngRT, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P3SpaRT, case_covar = P3Age, control_task = ControlSpaRT, control_covar = ControlAge, alternative = "two.sided")

#P4
BTD_cov(case_task = P4EngAcc, case_covar = P4Age, control_task = ControlEngAcc, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P4SpaAcc, case_covar = P4Age, control_task = ControlSpaAcc, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P4EngRT, case_covar = P4Age, control_task = ControlEngRT, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task = P4SpaRT, case_covar = P4Age, control_task = ControlSpaRT, control_covar = ControlAge, alternative = "two.sided")
