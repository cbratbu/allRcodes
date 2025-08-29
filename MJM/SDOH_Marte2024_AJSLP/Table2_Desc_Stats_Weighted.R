library(radiant.data)
library(haven)
library(survey)
library(ipumsr)
library(srvyr)
library(jtools)
svy <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = IPUMS, nest = TRUE, check.strata = FALSE)
vars_final <- c('AGE','SexMale', 'BMI', 'Married', 'South', 'FAMSIZE', 'Smoker', 
                'Education', 'PovertyAdj', 'WebUse', 'VisitYN', 'HRSLEEP', 'Cit', 
                'UsualPlace', 'DelAppt', 'SawGen', 'ERUse', 'SurgeryYorN12Mos', 
                'CHD', 'LimByChronDx','HEALTH_Binary', 'DELAYCOST', 'RxPres', 
                'BP', 'Chol', 'Db', 'Sadness', 'Hopelessness', 'Worthless', 
                'Deldue2Hrs', 'DelPhone', 'DelTrans', 'Noaffordcare',
                'ProbsPaying', 'Satisfaction', 'Insured')
options(pillar.sigfig=4)

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(LANY_Bin, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+LANY_Bin, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(AGE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHTAdj, na.rm =T))
svyttest(AGE~HispStrokeFlag, design = svy)

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(SexMale, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SexMale, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+SexMale, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Married, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Married, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Married, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(FAMSIZE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(FAMSIZE, SAMPWEIGHTAdj, na.rm =T))
svyttest(FAMSIZE~HispStrokeFlag, design = svy)

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Smoker, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Smoker, SAMPWEIGHTAdj, na.rm =T))

summary(svytable(~HispStrokeFlag+Smoker, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ALC5UPYR, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ALC5UPYR, SAMPWEIGHTAdj, na.rm =T))
svyttest(ALC5UPYR~HispStrokeFlag, design = svy)

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(HRSLEEP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HRSLEEP, SAMPWEIGHTAdj, na.rm =T))
svyttest(HRSLEEP~HispStrokeFlag, design = svy)

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Cit, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Cit, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+Cit, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')
svychisq(~HispStrokeFlag+Cit, svy, statistic = 'Chisq')
IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(TELCEL, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(TELCEL, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+UsualPlace, svy2, statistic = 'Chisq')



summary(svytable(~HispStrokeFlag+TELCEL, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(South, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(South, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+South, svy, statistic = 'Chisq')
summary(svytable(~South+HispStrokeFlag, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')



IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(BP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BP, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+BP, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+BP, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Chol, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Chol, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Chol, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Chol, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(RxPres, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(RxPres, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+RxPres, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+RxPres, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+SurgeryYorN12Mos, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+SurgeryYorN12Mos, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ERUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ERUse, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+ERUse, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+ERUse, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(VisitYN, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(VisitYN, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+VisitYN, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+VisitYN, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(UsualPlace, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(UsualPlace, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+UsualPlace, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+UsualPlace, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(NoCareBcLang, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(NoCareBcLang, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+NoCareBcLang, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(DelAppt, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(DelAppt, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+DelAppt, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+DelAppt, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Htn, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Htn, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Htn, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Htn, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Db, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Db, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Db, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Db, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(CholDx, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(CholDx, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+CholDx, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+CholDx, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(BMICALC, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BMICALC, SAMPWEIGHTAdj, na.rm =T))
svyttest(BMICALC~HispStrokeFlag, design = svy, subset(svy, StrokeSubset == 'Stroke'))


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Worthless, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Worthless, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Worthless, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Worthless, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Hopelessness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Hopelessness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Hopelessness, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Hopelessness, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Sadness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Sadness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Sadness, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Sadness, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(CHD, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(CHD, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+CHD, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+CHD, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Education, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SAPROXYAVAIL, Education, na.rm =T))
svychisq(~HispStrokeFlag+Education, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(WebUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(WebUse, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+WebUse, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+WebUse, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Confuse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Confuse, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Confuse, svy, statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+Confuse, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Headache, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Headache, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+Headache, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Numb, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Numb, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+Numb, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Seeing, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Seeing, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+Seeing, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Walking, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Walking, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+Walking, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Email, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Email, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+Email, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(PCRX, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(PCRX, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+PCRX, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

summary(svytable(~HispStrokeFlag+South, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')
summary(svytable(~HispStrokeFlag+WebUse, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ChronStroke, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ChronStroke, SAMPWEIGHTAdj, na.rm =T))
summary(svytable(~HispStrokeFlag+ChronStroke, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(UsualPlace, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(UsualPlace, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+LANY_Bin, svy2, statistic = 'Chisq')
summary(svytable(~`PovertyAdj<100%`+HispStrokeFlag, subset(svy, StrokeSubset == 'Stroke')), statistic = 'Chisq')