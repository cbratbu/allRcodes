library(radiant.data)
library(haven)
library(survey)
library(ipumsr)
library(srvyr)
library(jtools)

nhwsurvey <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = hispmatch, nest = TRUE, check.strata = FALSE)
options(pillar.sigfig=3)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(AGE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHTAdj, na.rm =T)) %>% 
  as.data.frame()
svyttest(AGE~NHWMerge, design = nhwsurvey)

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(SexMale, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SexMale, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~NHWMerge+SexMale, nhwsurvey, statistic = 'Chisq')

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Married, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Married, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~NHWMerge+Married, nhwsurvey, statistic = 'Chisq')

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(South, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(South, SAMPWEIGHTAdj, na.rm =T))
south_chi_hisp <- svychisq(~NHWMerge+South, nhwsurvey, statistic = 'Chisq')
south_chi_hisp$statistic
south_chi_hisp

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(FAMSIZE, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(FAMSIZE, SAMPWEIGHTAdj, na.rm =T))
svyttest(FAMSIZE~NHWMerge, design = nhwsurvey)

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Smoker, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Smoker, SAMPWEIGHTAdj, na.rm =T))
smokerchi_hisp <- svychisq(~NHWMerge+Smoker, nhwsurvey, statistic = 'Chisq')
smokerchi_hisp

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Education, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Education, SAMPWEIGHTAdj, na.rm =T)) %>% as.data.frame()
educhi_nhw <- svychisq(~NHWMerge+Education, nhwsurvey, statistic = 'Chisq')
educhi_nhw

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(PovertyLevel, SAMPWEIGHTAdj, na.rm =F), sd = weighted.sd(PovertyLevel, SAMPWEIGHTAdj, na.rm =F)) %>% as.data.frame()
pov_nhw_chi <- svychisq(~NHWMerge+PovertyLevel, nhwsurvey, statistic = 'Chisq')
pov_nhw_chi

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(HRSLEEP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HRSLEEP, SAMPWEIGHTAdj, na.rm =T))
svyttest(HRSLEEP~NHWMerge, design = nhwsurvey)

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Cit, SAMPWEIGHTAdj, na.rm =F), sd = weighted.sd(Cit, SAMPWEIGHTAdj, na.rm =F))
Cit_chi_nhw <- svychisq(~NHWMerge+Cit, nhwsurvey, statistic = 'Chisq')
Cit_chi_nhw

###healthcare access and utilization
hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(BP, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BP, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+BP, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+BP, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Chol, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Chol, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Chol, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Chol, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(SurgeryYorN12Mos, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+SurgeryYorN12Mos, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+SurgeryYorN12Mos, nhwsurvey, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(RxPres, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(RxPres, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+RxPres, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+RxPres, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(VisitYN, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(VisitYN, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+VisitYN, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+VisitYN, nhwsurvey, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(ERUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ERUse, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+ERUse, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+ERUse, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(UsualPlace, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(UsualPlace, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+UsualPlace, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+UsualPlace, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(DelayInCare, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(DelayInCare, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+DELAYCOST, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+DELAYCOST, nhwsurvey, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(ProbsPaying, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ProbsPaying, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+ProbsPaying, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+ProbsPaying, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Notaffordcare, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Notaffordcare, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Notaffordcare, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Notaffordcare, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Satisfaction, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Satisfaction, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Satisfaction, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Satisfaction, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Insured, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Insured, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Insured, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Insured, nhwsurvey, statistic = 'Chisq')$p.value

#health status

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Db, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Db, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Db, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Db, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(CHD, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(CHD, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+CHD, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+CHD, nhwsurvey, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(BMICALC, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(BMICALC, SAMPWEIGHTAdj, na.rm =T)) %>% 
  as.data.frame()
svyttest(BMICALC~NHWMerge, design = nhwsurvey)

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(HEALTH_Binary, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(HEALTH_Binary, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~NHWMerge+HEALTH_Binary, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+HEALTH_Binary, nhwsurvey, statistic = 'Chisq')$p.value


hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(WebUse, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(WebUse, SAMPWEIGHTAdj, na.rm =T))%>% 
  as.data.frame()
svychisq(~NHWMerge+WebUse, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+WebUse, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Worthless, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Worthless, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Worthless, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Worthless, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Hopelessness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Hopelessness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Hopelessness, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Hopelessness, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(Sadness, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(Sadness, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+Sadness, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+Sadness, nhwsurvey, statistic = 'Chisq')$p.value

hispmatch %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(LimByChronDx, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LimByChronDx, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+LimByChronDx, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+LimByChronDx, nhwsurvey, statistic = 'Chisq')$p.value

#subset
IPUMS_cc %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(LANY_Bin, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LANY_Bin, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+LANY_Bin, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+LANY_Bin, nhwsurvey, statistic = 'Chisq')$p.value

IPUMS_cc %>% 
  group_by(NHWMerge) %>% 
  summarise(mean = weighted.mean(ComDiff, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ComDiff, SAMPWEIGHTAdj, na.rm =T))
svychisq(~NHWMerge+ComDiff, nhwsurvey, statistic = 'Chisq')$statistic
svychisq(~NHWMerge+ComDiff, nhwsurvey, statistic = 'Chisq')$p.value

#poverty\\

IPUMS2$NHWMerge <- as.factor(hispmatch$NHWMerge)

round(prop.table(svytable(~Living+Country,surveydes))*100,digits=3)
nhwsurvey

IPUMS_cc %>% group_by(NHWMerge) %>% 
  count(PovertyAdj, wt = SAMPWEIGHTAdj) %>% mutate(per = n/sum(n))
