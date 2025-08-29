library(ggstance)
library(jtools)
svy <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = IPUMS, nest = TRUE, check.strata = FALSE)

summary(svyglm(FnLimStroke~AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeHisp == 1))

summary(svyglm(FnLimStroke~AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeNWH == 0))

#Everyone included in the study ~124k

#Healthy subset ~120k

#Stroke subset ~4k [nHw stroke + Hispanics stroke]

#nHw healthy subset ~84k

#Hispanic healthy subset ~23k

#nHw stroke subset ~4k

#Hispanic stroke subset ~.5k\

#high-level disparities between hispanics and nonhispanic > disparities within hispanics

#high-level disparities between stroke versions of these cohorts

summary(svyglm(BP~AGE + SexMale + Hispanic*Cit + Hispanic*Poverty+ South + Education + Married + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = HealthySubset == 1))

## stroke version
summary(svyglm(BP~AGE + SexMale + Hispanic*Cit  + Hispanic*Poverty+ South + Married + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeSubset == 1))

#summary(svyglm(VisitYN~AGE + Hispanic*Cit + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               #subset = HealthySubset == 1))

summary(svyglm(Htn~Hispanic*Cit + UsualPlace*Hispanic + AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = HealthySubset == 1))

summary(svyglm(Confuse~ UsualPlace*Hispanic + AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = HealthySubset == 1))

summary(svyglm(BP~ Hispanic*Htn + AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = HealthySubset == 1))

summary(svyglm(BP~ Hispanic*Htn + AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeSubset == 1))
pshfit <- svyglm(BP~ Hispanic*Htn + AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
                 subset = StrokeSubset == 1)
summ(pshfit, vifs=T)
#does having insurance predict office visits?
####
summary(svyglm(LAIADL2~AGE + Hispanic*VisitYN + South + MStLivPtnr +FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeSubset == 1))
summary(svyglm(ADL~AGE + Hispanic*VisitYN + South + MStLivPtnr +FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeSubset == 1))
summary(svyglm(FnLimStroke~AGE + Hispanic*VisitYN + South + MStLivPtnr +FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeSubset == 1))
####
summ(svyglm(ADL~AGE + South + MStLivPtnr +FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeNWH == 0))

summ(svyglm(ChronStroke~AGE + South + MStLivPtnr + FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeHisp == 1))

summ(svyglm(ChronStroke~AGE + South  +  MStLivPtnr +FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = StrokeNWH == 0))
########### new models including both cohorts IPUMS$AllSubset == 1; $HealthySubset == "HealthySubset"; $StrokeSubset == "Stroke"
summary(svyglm(Stroke~AGE*Hispanic + South  +  MStLivPtnr +FAMSIZE + ALC5UPYR + HRSLEEP + BMICALC, family = quasibinomial, design = svy,
               subset = HealthySubset ))