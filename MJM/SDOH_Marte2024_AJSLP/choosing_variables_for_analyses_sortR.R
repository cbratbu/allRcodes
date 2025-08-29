vars <- c("AGE", "MStLivPtnr", "FAMSIZE", "Smoker", "ALC5UPYR", "HRSLEEP",
          "Cit", "South", "BP", "Education", "ERUse", "SurgeryYorN12Mos", "VisitYN", "UsualPlace",
          "DelAppt", "CholDx", "Db", "Htn", "Chol", "BP", "ADL", "LimByChronDx", 
          "ChronStroke", "FnLimStroke", "Hopelessness", "Sadness", "Worthless", "South", "CHD", "PovertyAdj", "Health")
vars
#absolutely necessary 
#vars4 <- c("AGE", 'SexMale', 'South', 'Married', 'FAMSIZE', 'BMI', 'Smoker', 'HRSLEEP','WebUse', 'ComDif', 'PovertyAdj', 'Education')

vars5 <- c("AGE", 'SexMale', 'South', 'Married', 'FAMSIZE', 'Smoker', 'BMI', 'HRSLEEP','WebUse', 'PovertyAdj',
           'Education', 'Cit', 'LANY_Bin', 'VisitYN', 'ERUse', 'SurgeryYorN12Mos','UsualPlace','DelAppt',
           'RxPres', 'BP', 'Chol','Db', 'Hopelessness','Worthless', 'Sadness', 'CHD', 'LimByChronDx', 
           'DELAYCOST', 'SawGen', 'HEALTH_Binary')


vars_final <- c('AGE','SexMale', 'BMICALC', 'Married', 'South', 'FAMSIZE', 'Smoker', 
                'Education', 'PovertyLevel', 'WebUse', 'VisitYN', 'HRSLEEP', 'Cit', 
                'UsualPlace', 'SawGen', 'ERUse', 'SurgeryYorN12Mos', 
                'CHD','HEALTH_Binary', 'RxPres', 
                'BP', 'Chol', 'Db', 'Sadness', 'Hopelessness', 'Worthless', 
                'DelayInCare', 'ProbsPaying', 'Satisfaction', 'Insured','LimByChronDx')

IPUMS_cc <- IPUMS
IPUMS_cc <- IPUMS_cc %>% drop_na(NHISPID, any_of(vars_final))

sum(IPUMS_cc$HispStrokeFlag == 1, na.rm=T)



vars_final_subset_glm <- c('AGE', 'SexMale', 'BMICALC', 'Married', 'South', 'FAMSIZE', 'Smoker', 
                           'Education', 'WebUse', 'VisitYN', 'HRSLEEP', 'Cit', 
                           'UsualPlace', 'SawGen', 'ERUse', 'SurgeryYorN12Mos', 
                           'CHD','HEALTH_Binary', 'RxPres', 
                           'BP', 'Chol', 'Db', 'Sadness', 'Hopelessness', 'Worthless',
                           'Notaffordcare','ProbsPaying', 'Satisfaction', 'Insured',
                           'DelayInCare', 'ComDiff', 'PovertyLevel','LimByChronDx', 'LANY_Bin')
IPUMS_glm <- IPUMS
IPUMS_glm <- IPUMS_glm %>% drop_na(NHISPID, any_of(vars_final_subset_glm))

IPUMS_glm2 <- subset(IPUMS_glm, HispStrokeFlag == 1 | HispStrokeFlag == 0)

sum(IPUMS_glm$HispStrokeFlag == 1, na.rm=T)

sum(IPUMS_glm$StrokeSubsetGLM == 1, na.rm=T)
IPUMS_glm$StrokeSubsetGLM[is.na(IPUMS_glm$StrokeSubsetGLM)] <- 0

propscore_vars <- c('AGE', 'SexMale', 'BMICALC', 'Married', 'South', 'FAMSIZE', 'Smoker', 
                           'Education', 'PovertyLevel', 'WebUse', 'VisitYN', 'HRSLEEP', 'Cit', 
                           'UsualPlace', 'DelAppt', 'SawGen', 'ERUse', 'SurgeryYorN12Mos', 
                           'CHD','HEALTH_Binary', 'RxPres', 
                           'BP', 'Chol', 'Db', 'Sadness', 'Hopelessness', 'Worthless', 
                           'DelayInCare', 'Notaffordcare',
                           'ProbsPaying', 'Satisfaction', 'Insured','Stroke')
IPUMS_prop <- IPUMS
IPUMS_prop <- IPUMS_prop %>% drop_na(NHISPID, any_of(propscore_vars))
sum(IPUMS_prop$HispStrokeFlag == 0, na.rm=T)
#subset with Comdiff

#variables to consider excluding due to data loss:
#alcohol
#cholesteral dx
#ChronStroke problems
#FnLimStroke
#Htn

#numbers to beat: 2938/423 & 1813, 256