library(radiant.data)
library(haven)
library(survey)
library(ipumsr)
library(srvyr)
library(jtools)
hispmatchsvy <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = hispmatch, nest = TRUE, check.strata = FALSE)

vars_final <- c('AGE','SexMale', 'BMI', 'Married', 'South', 'FAMSIZE', 'Smoker', 
                'Education', 'PovertyAdj', 'WebUse', 'VisitYN', 'HRSLEEP', 'Cit', 
                'UsualPlace', 'DelAppt', 'SawGen', 'ERUse', 'SurgeryYorN12Mos', 
                'CHD', 'LimByChronDx','HEALTH_Binary', 'DELAYCOST', 'RxPres', 
                'BP', 'Chol', 'Db', 'Sadness', 'Hopelessness', 'Worthless', 
                'Deldue2Hrs', 'DelPhone', 'DelTrans', 'Noaffordcare',
                'ProbsPaying', 'Satisfaction', 'Insured')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(LimByChronDx, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHTAdj, na.rm =T)) %>% as.data.frame()
svychisq(~HispStrokeFlag+LANY_Bin, hispmatchsvy, statistic = 'Chisq')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(AGE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHTAdj, na.rm =T)) %>% as.data.frame()
svyttest(AGE~HispStrokeFlag, design = hispmatchsvy)

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(SexMale, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SexMale, SAMPWEIGHTAdj, na.rm =T)) %>% as.data.frame()
svychisq(~HispStrokeFlag+SexMale, hispmatchsvy, statistic = 'Chisq')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Married, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Married, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Married, hispmatchsvy, statistic = 'Chisq')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(South, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(South, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+South, hispmatchsvy, statistic = 'Chisq')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(FAMSIZE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(FAMSIZE, SAMPWEIGHTAdj, na.rm =T))
svyttest(FAMSIZE~HispStrokeFlag, design = hispmatchsvy)

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Smoker, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Smoker, SAMPWEIGHTAdj, na.rm =T))
smokerchi <- svychisq(~HispStrokeFlag+Smoker, hispmatchsvy, statistic = 'Chisq')
smokerchi$statistic
hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Education, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Education, SAMPWEIGHTAdj, na.rm =T))
educhi <- svychisq(~HispStrokeFlag+Education, hispmatchsvy, statistic = 'Chisq')
educhi
hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(HRSLEEP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HRSLEEP, SAMPWEIGHTAdj, na.rm =T))
svyttest(HRSLEEP~HispStrokeFlag, design = hispmatchsvy)

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(PovertyLevel, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(PovertyLevel, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+PovertyLevel, hispmatchsvy, statistic = 'Chisq')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Cit, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Cit, SAMPWEIGHTAdj, na.rm =T))
Cit_chi <- svychisq(~HispStrokeFlag+Cit, hispmatchsvy, statistic = 'Chisq')
Cit_chi$statistic
Cit_chi

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(BP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BP, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+BP, hispmatchsvy, statistic = 'Chisq')

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Chol, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Chol, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Chol, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Chol, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+SurgeryYorN12Mos, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+SurgeryYorN12Mos, hispmatchsvy, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(RxPres, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(RxPres, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+RxPres, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+RxPres, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(VisitYN, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(VisitYN, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+VisitYN, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+VisitYN, hispmatchsvy, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ERUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ERUse, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+ERUse, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+ERUse, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(UsualPlace, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(UsualPlace, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+UsualPlace, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+UsualPlace, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(DelayInCare, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(DelayInCare, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+DelayInCare, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+DelayInCare, hispmatchsvy, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ProbsPaying, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ProbsPaying, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+ProbsPaying, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+ProbsPaying, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Notaffordcare, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Notaffordcare, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Notaffordcare, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Notaffordcare, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Satisfaction, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Satisfaction, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Satisfaction, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Satisfaction, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Insured, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Insured, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Insured, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Insured, hispmatchsvy, statistic = 'Chisq')$p.value

#health status

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Db, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Db, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Db, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Db, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(CHD, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(CHD, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+CHD, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+CHD, hispmatchsvy, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(BMICALC, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BMICALC, SAMPWEIGHTAdj, na.rm =T))
svyttest(BMICALC~HispStrokeFlag, design = hispmatchsvy)

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(HEALTH_Binary, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HEALTH_Binary, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+HEALTH_Binary, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+HEALTH_Binary, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Worthless, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Worthless, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Worthless, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Worthless, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Hopelessness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Hopelessness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Hopelessness, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Hopelessness, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(Sadness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Sadness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+Sadness, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+Sadness, hispmatchsvy, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(LimByChronDx, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LimByChronDx, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+LimByChronDx, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+LimByChronDx, hispmatchsvy, statistic = 'Chisq')$p.value

#subset
IPUMS_cc %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(LANY_Bin, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LANY_Bin, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+LANY_Bin, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+LANY_Bin, hispmatchsvy, statistic = 'Chisq')$p.value

IPUMS_cc %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ComDiff, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ComDiff, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+ComDiff, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+ComDiff, hispmatchsvy, statistic = 'Chisq')$p.value

#poverty\\

IPUMS2$HispStrokeFlag <- as.factor(hispmatch$HispStrokeFlag)

round(prop.table(svytable(~Living+Country,surveydes))*100,digits=3)
hispmatchsvy

IPUMS_cc %>% group_by(HispStrokeFlag) %>% 
  count(PovertyAdj, wt = SAMPWEIGHTAdj) %>% mutate(per = n/sum(n))


svychisq(~HispStrokeFlag+PovertyAdj, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+PovertyAdj, hispmatchsvy, statistic = 'Chisq')$p.value