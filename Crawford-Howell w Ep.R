library(singcar)
library(psycho)
library(tidyverse)
library(splitstackshape)
df <- data.frame(CGEp)

PatientEngRT <- df[1:4,]$EngRT
PatientSpaRT <- df[1:4,]$SpaRT
#P1
P1EngAcc <- df[1,]$EngAcc
P1SpaAcc <- df[1,]$SpaAc
P1EngRT <- df[1,]$EngRT
P1SpaRT <- df[1,]$SpaRT
P1Age <- df[1,]$Age

#P1 parameter values from Model 6
P1EngM6EpA <- df[1,]$EngModel6AvgEpA
P1EngM6EpB <- df[1,]$EngModel6AvgEpB
P1SpaM6EpA <- df[1,]$SpaModel6AvgEpA
P1SpaM6EpB <- df[1,]$SpaModel6AvgEpB

#P2
P2EngAcc <- df[2,]$EngAcc
P2SpaAcc <- df[2,]$SpaAc
P2EngRT <- df[2,]$EngRT
P2SpaRT <- df[2,]$SpaRT
P2Age <- df[2,]$Age

#P2 parameter values from Model 6
P2EngM6EpA <- df[2,]$EngModel6AvgEpA
P2EngM6EpB <- df[2,]$EngModel6AvgEpB
P2SpaM6EpA <- df[2,]$SpaModel6AvgEpA
P2SpaM6EpB <- df[2,]$SpaModel6AvgEpB

#P3
P3EngAcc <- df[3,]$EngAcc
P3SpaAcc <- df[3,]$SpaAc
P3EngRT <- df[3,]$EngRT
P3SpaRT <- df[3,]$SpaRT
P3Age <- df[3,]$Age

#P3 parameter values from Model 6
P3EngM6EpA <- df[3,]$EngModel6AvgEpA
P3EngM6EpB <- df[3,]$EngModel6AvgEpB
P3SpaM6EpA <- df[3,]$SpaModel6AvgEpA
P3SpaM6EpB <- df[3,]$SpaModel6AvgEpB

#P4
P4EngAcc <- df[4,]$EngAcc
P4SpaAcc <- df[4,]$SpaAc
P4EngRT <- df[4,]$EngRT
P4SpaRT <- df[4,]$SpaRT
P4Age <- df[4,]$Age

#P4 parameter values from Model 6
P4EngM6EpA <- df[4,]$EngModel6AvgEpA
P4EngM6EpB <- df[4,]$EngModel6AvgEpB
P4SpaM6EpA <- df[4,]$SpaModel6AvgEpA
P4SpaM6EpB <- df[4,]$SpaModel6AvgEpB

#Controls
ControlEngAcc <- df[5:8,]$EngAcc
ControlSpaAcc <- df[5:8,]$SpaAc
ControlEngRT <- df[5:8,]$EngRT
ControlSpaRT <- df[5:8,]$SpaRT
ControlAge <- df[5:8,]$Age

#EpA, B for controls on Model 6 in both languages
ControlEngM6EpA <- df[5:8,]$EngModel6AvgEpA
ControlEngM6EpB <- df[5:8,]$EngModel6AvgEpB
ControlSpaM6EpA <- df[5:8,]$SpaModel6AvgEpA
ControlSpaM6EpB <- df[5:8,]$SpaModel6AvgEpB


#P1 for Ep values
BTD(P1EngM6EpA, ControlEngM6EpA)
BTD(P1EngM6EpB, ControlEngM6EpB)
BTD(P1SpaM6EpA, ControlSpaM6EpA)
BTD(P1SpaM6EpB, ControlSpaM6EpB)

#P3 for Spanish Ep Model 6 (selected model)
P3SpaM6EpA <- df2[3,]$SpaModel6AvgEpA
P3SpaM6EpB <- df2[3,]$SpaModel6AvgEpB

#P3 for Ep values in Spanish
crawford.test(P3SpaM6EpA, ControlSpaM6EpA)
plot(crawford.test(P3SpaM6EpB, ControlSpaM6EpB))



#P1
crawford.test(P1EngAcc, ControlEngAcc)
crawford.test(P1SpaAcc, ControlSpaAcc)
crawford.test(P1EngRT, ControlEngRT)
crawford.test(P1SpaRT, ControlSpaRT)

#P1 for Ep values
P1EpAEng <- BTD_cov(case_task=P1EngM6EpA, case_covar=P1Age, control_task=ControlEngM6EpA, control_covar = ControlAge, alternative = "two.sided")

P1EpAEng$interval

BTD_cov(case_task=P1EngM6EpB, case_covar=P1Age, control_task=ControlEngM6EpB, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P1SpaM6EpA, case_covar=P1Age, control_task=ControlSpaM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P1SpaM6EpB, case_covar=P1Age, control_task=ControlSpaM6EpB, control_covar = ControlAge, alternative = "two.sided")

#P2
crawford.test(P2EngAcc, ControlEngAcc)
crawford.test(P2SpaAcc, ControlSpaAcc)
crawford.test(P2EngRT, ControlEngRT)
crawford.test(P2SpaRT, ControlSpaRT)

#P2 for Ep values
BTD_cov(case_task=P2EngM6EpA, case_covar=P2Age, control_task=ControlEngM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P2EngM6EpB, case_covar=P2Age, control_task=ControlEngM6EpB, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P2SpaM6EpA, case_covar=P2Age, control_task=ControlSpaM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P2SpaM6EpB, case_covar=P2Age, control_task=ControlSpaM6EpB, control_covar = ControlAge, alternative = "two.sided")

#P3
crawford.test(P3EngAcc, ControlEngAcc)
crawford.test(P3SpaAcc, ControlSpaAcc)
crawford.test(P3EngRT, ControlEngRT)
crawford.test(P3SpaRT, ControlSpaRT)

#P3 for Ep values
BTD_cov(case_task=P3EngM6EpA, case_covar=P3Age, control_task=ControlEngM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P3EngM6EpB, case_covar=P3Age, control_task=ControlEngM6EpB, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P3SpaM6EpA, case_covar=P3Age, control_task=ControlSpaM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P3SpaM6EpB, case_covar=P3Age, control_task=ControlSpaM6EpB, control_covar = ControlAge, alternative = "two.sided")
#P43
crawford.test(P4EngAcc, ControlEngAcc)
crawford.test(P4SpaAcc, ControlSpaAcc)
crawford.test(P4EngRT, ControlEngRT)
crawford.test(P4SpaRT, ControlSpaRT)

#P4 for Ep values
BTD_cov(case_task=P4EngM6EpA, case_covar=P4Age, control_task=ControlEngM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P4EngM6EpB, case_covar=P4Age, control_task=ControlEngM6EpB, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P4SpaM6EpA, case_covar=P4Age, control_task=ControlSpaM6EpA, control_covar = ControlAge, alternative = "two.sided")
BTD_cov(case_task=P4SpaM6EpB, case_covar=P4Age, control_task=ControlSpaM6EpB, control_covar = ControlAge, alternative = "two.sided")
