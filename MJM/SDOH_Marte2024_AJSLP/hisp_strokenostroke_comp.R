library(radiant.data)
library(haven)
library(survey)
library(ipumsr)
library(srvyr)
library(jtools)

hispmergesvycheck <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = hispmatch, nest = TRUE, check.strata = FALSE)
hispmergesvy <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = join_IPUMS, nest = TRUE, check.strata = FALSE)
options(pillar.sigfig=3)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(AGE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHTAdj, na.rm =T)) %>% 
  as.data.frame()
svyttest(AGE~HispMerge, design = hispmergesvy)

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(SexMale, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SexMale, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~HispMerge+SexMale, hispmergesvy, statistic = 'Chisq')


join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(WebUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(WebUse, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~HispMerge+WebUse, hispmergesvy, statistic = 'Chisq')

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Married, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Married, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~HispMerge+Married, hispmergesvy, statistic = 'Chisq')

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(South, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(South, SAMPWEIGHTAdj, na.rm =T))
south_chi_hisp <- svychisq(~HispMerge+South, hispmergesvy, statistic = 'Chisq')
south_chi_hisp$statistic

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(FAMSIZE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(FAMSIZE, SAMPWEIGHTAdj, na.rm =T))
svyttest(FAMSIZE~HispMerge, design = hispmergesvy)

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Smoker, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Smoker, SAMPWEIGHTAdj, na.rm =T))
smokerchi_hisp <- svychisq(~HispMerge+Smoker, hispmergesvy, statistic = 'Chisq')
smokerchi_hisp

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Education, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Education, SAMPWEIGHTAdj, na.rm =T)) %>% as.data.frame()
educhi_hisp <- svychisq(~HispMerge+Education, hispmergesvy, statistic = 'Chisq')

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(PovertyLevel, SAMPWEIGHTAdj, na.rm =F), sd = weighted.sd(PovertyLevel, SAMPWEIGHTAdj, na.rm =F)) %>% as.data.frame()
pov_hisp_chi <- svychisq(~HispMerge+PovertyLevel, hispmergesvy, statistic = 'Chisq')
pov_hisp_chi$statistic

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(HRSLEEP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HRSLEEP, SAMPWEIGHTAdj, na.rm =T))
svyttest(HRSLEEP~HispMerge, design = hispmergesvy)

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Cit, SAMPWEIGHTAdj, na.rm =F), sd = weighted.sd(Cit, SAMPWEIGHTAdj, na.rm =F))
Cit_chi_hisp <- svychisq(~HispMerge+Cit, hispmergesvy, statistic = 'Chisq')
Cit_chi_hisp

###healthcare access and utilization
join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(BP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BP, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+BP, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+BP, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Chol, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Chol, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Chol, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Chol, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+SurgeryYorN12Mos, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+SurgeryYorN12Mos, hispmergesvy, statistic = 'Chisq')$p.value


join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(RxPres, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(RxPres, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+RxPres, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+RxPres, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(VisitYN, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(VisitYN, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+VisitYN, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+VisitYN, hispmergesvy, statistic = 'Chisq')$p.value


join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(ERUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ERUse, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+ERUse, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+ERUse, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(UsualPlace, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(UsualPlace, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+UsualPlace, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+UsualPlace, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(DelayInCare, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(DelayInCare, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+DELAYCOST, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+DELAYCOST, hispmergesvy, statistic = 'Chisq')$p.value


join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(ProbsPaying, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ProbsPaying, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+ProbsPaying, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+ProbsPaying, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Notaffordcare, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Notaffordcare, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Notaffordcare, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Notaffordcare, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Satisfaction, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Satisfaction, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Satisfaction, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Satisfaction, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Insured, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Insured, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Insured, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Insured, hispmergesvy, statistic = 'Chisq')$p.value

#health status

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Db, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Db, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Db, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Db, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(CHD, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(CHD, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+CHD, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+CHD, hispmergesvy, statistic = 'Chisq')$p.value


join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(BMICALC, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BMICALC, SAMPWEIGHTAdj, na.rm =T)) %>% 
  as.data.frame()
svyttest(BMICALC~HispMerge, design = hispmergesvy)

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(HEALTH_Binary, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HEALTH_Binary, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~HispMerge+HEALTH_Binary, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+HEALTH_Binary, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Worthless, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Worthless, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Worthless, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Worthless, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Hopelessness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Hopelessness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Hopelessness, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Hopelessness, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(Sadness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Sadness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+Sadness, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+Sadness, hispmergesvy, statistic = 'Chisq')$p.value

join_IPUMS %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(LimByChronDx, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LimByChronDx, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+LimByChronDx, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+LimByChronDx, hispmergesvy, statistic = 'Chisq')$p.value

#subset
IPUMS_cc %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(LANY_Bin, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LANY_Bin, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+LANY_Bin, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+LANY_Bin, hispmergesvy, statistic = 'Chisq')$p.value

IPUMS_cc %>% 
  group_by(HispMerge) %>% 
  summarise(mean = weighted.mean(ComDiff, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ComDiff, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispMerge+ComDiff, hispmergesvy, statistic = 'Chisq')$statistic
svychisq(~HispMerge+ComDiff, hispmergesvy, statistic = 'Chisq')$p.value

#poverty\\

IPUMS2$HispMerge <- as.factor(join_IPUMS$HispMerge)

round(prop.table(svytable(~Living+Country,surveydes))*100,digits=3)
hispmergesvy

IPUMS_cc %>% group_by(HispMerge) %>% 
  count(PovertyAdj, wt = SAMPWEIGHTAdj) %>% mutate(per = n/sum(n))
