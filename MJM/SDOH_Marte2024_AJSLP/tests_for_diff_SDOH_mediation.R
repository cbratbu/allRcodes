vars_final_subset_glm <- c('AGE', 'SexMale', 'BMICALC', 'Married', 'South', 'FAMSIZE', 'Smoker', 
                           'Education', 'WebUse', 'VisitYN', 'HRSLEEP', 'Cit', 
                           'UsualPlace', 'DelAppt', 'SawGen', 'ERUse', 'SurgeryYorN12Mos', 
                           'CHD','HEALTH_Binary', 'DELAYCOST', 'RxPres', 
                           'BP', 'Chol', 'Db', 'Sadness', 'Hopelessness', 'Worthless', 
                           'Deldue2Hrs', 'DelPhone', 'DelTrans', 'Notaffordcare',
                           'ProbsPaying', 'Satisfaction', 'Insured',
                           'ComDiff', 'PovertyLevel','LimByChronDx', 'LANY_Bin')

summary(svyglm(SawGen ~ Hispanic + SexMale + AGE + South + Married + FAMSIZE + BMICALC, family = binomial, design = svy,
                  subset = StrokeSubset == 1))

summary(svyglm(ComDiff ~ SawGen + Hispanic + SexMale + AGE + South + Married + FAMSIZE + BMICALC, family = binomial, design = svy,
                       subset = StrokeSubset == 1))
summary(pov_com_diff)

deltrans_mediateed <- mediate(deltrans, deltrans_hisp, treat = "Hispanic", mediator = "DelTrans")
summary(deltrans_mediateed)

##
edu_med <- svyglm(PovertyLevel ~ Hispanic + AGE + SexMale + South + Married + FAMSIZE + BMICALC, family = binomial, design = svy, subset = StrokeSubset == 1)
summary(edu_med)
edu_adl <- svyglm(ComDiff ~ PovertyLevel*AGE + Hispanic + AGE + SexMale + South + Married + FAMSIZE + BMICALC, family = binomial, design = svy,
                        subset = StrokeSubset == 1)
summary(edu_adl)
edu_adl_med <- mediate(edu_med, edu_adl, treat = "Hispanic", mediator = "PovertyLevel", covariates = list(AGE = 70))
summary(edu_adl_med)