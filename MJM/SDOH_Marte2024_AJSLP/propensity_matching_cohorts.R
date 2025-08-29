library(tidyverse)
library(tidylog)
library(pwr)
library(naniar)
library(MatchIt)
library(MASS)
library(survival)
library(survminer)

#to do matching
Hisp.Matched <- matchit(Stroke ~ SexMale + AGE + South + Married + FAMSIZE + BMICALC + Smoker, data=IPUMS_prop, method = 'nearest', ratio = 1, replace = TRUE)
summary(Hisp.Matched)
View(Hisp.Matched)

hispmatch <- match.data(Hisp.Matched)
write.csv(hispmatch, "hispmatch.csv")

sum(hispmatch$NHWFlag==0, na.rm = T)
sum(hispmatch$HispFlag==1, na.rm = T)


sum(unique(hispmatch), na.rm=T)



