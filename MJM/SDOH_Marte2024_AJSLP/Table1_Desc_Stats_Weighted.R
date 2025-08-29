library(radiant.data)
library(haven)
library(survey)
library(ipumsr)
library(srvyr)
library(jtools)
svy <- svydesign(~PSU, weights = ~SAMPWEIGHT, strata = ~STRATA, data = IPUMS, nest = TRUE, check.strata = FALSE)

options(pillar.sigfig=4)
typeStringsGadget()
IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(AGE, SAMPWEIGHT, na.rm =T), sd = weighted.sd(AGE, SAMPWEIGHT, na.rm =T))
svyttest(AGE~HispFlag, design = svy)

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(MStLivPtnr, SAMPWEIGHT, na.rm =T), sd = weighted.sd(MStLivPtnr, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+MStLivPtnr, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(FAMSIZE, SAMPWEIGHT, na.rm =T), sd = weighted.sda(FAMSIZE, SAMPWEIGHT, na.rm =T))
svyttest(FAMSIZE~HispFlag, design = svy)

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Smoker, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Smoker, SAMPWEIGHT, na.rm =T))

summary(svytable(~HispFlag+Smoker, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(ALC5UPYR, SAMPWEIGHT, na.rm =T), sd = weighted.sd(ALC5UPYR, SAMPWEIGHT, na.rm =T))
svyttest(ALC5UPYR~HispFlag, design = svy)

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(HRSLEEP, SAMPWEIGHT, na.rm =T), sd = weighted.sd(HRSLEEP, SAMPWEIGHT, na.rm =T))
svyttest(HRSLEEP~HispFlag, design = svy)

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Cit, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Cit, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+Cit, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(TELCEL, SAMPWEIGHT, na.rm =T), sd = weighted.sd(TELCEL, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+TELCEL, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+TELCEL, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(South, SAMPWEIGHT, na.rm =T), sd = weighted.sd(South, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+South, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+South, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(BP, SAMPWEIGHT, na.rm =T), sd = weighted.sd(BP, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+BP, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+BP, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Chol, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Chol, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Chol, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Chol, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(RxPres, SAMPWEIGHT, na.rm =T), sd = weighted.sd(RxPres, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+RxPres, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+RxPres, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(SurgeryYorN12Mos, SAMPWEIGHT, na.rm =T), sd = weighted.sd(SurgeryYorN12Mos, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+SurgeryYorN12Mos, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+SurgeryYorN12Mos, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(ERUse, SAMPWEIGHT, na.rm =T), sd = weighted.sd(ERUse, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+ERUse, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+ERUse, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(VisitYN, SAMPWEIGHT, na.rm =T), sd = weighted.sd(VisitYN, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+VisitYN, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+VisitYN, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(UsualPlace, SAMPWEIGHT, na.rm =T), sd = weighted.sd(UsualPlace, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+UsualPlace, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+UsualPlace, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(NoCareBcLang, SAMPWEIGHT, na.rm =T), sd = weighted.sd(NoCareBcLang, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+NoCareBcLang, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(DelAppt, SAMPWEIGHT, na.rm =T), sd = weighted.sd(DelAppt, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+DelAppt, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+DelAppt, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Htn, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Htn, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Htn, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Htn, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Db, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Db, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Db, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Db, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(CholDx, SAMPWEIGHT, na.rm =T), sd = weighted.sd(CholDx, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+CholDx, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+CholDx, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(BMICALC, SAMPWEIGHT, na.rm =T), sd = weighted.sd(BMICALC, SAMPWEIGHT, na.rm =T))
svyttest(BMICALC~HispFlag, design = svy)


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Worthless, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Worthless, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Worthless, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Worthless, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Hopelessness, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Hopelessness, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Hopelessness, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Hopelessness, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Sadness, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Sadness, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Sadness, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Sadness, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(CHD, SAMPWEIGHT, na.rm =T), sd = weighted.sd(CHD, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+CHD, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+CHD, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(SAPROXYAVAIL, SAMPWEIGHT, na.rm =T), sd = weighted.sd(SAPROXYAVAIL, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+SAPROXYAVAIL, svy, statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(WebUse, SAMPWEIGHT, na.rm =T), sd = weighted.sd(WebUse, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+WebUse, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+WebUse, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Confuse, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Confuse, SAMPWEIGHT, na.rm =T))
svychisq(~HispFlag+Confuse, svy, statistic = 'Chisq')
summary(svytable(~HispFlag+Confuse, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')


IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Headache, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Headache, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+Headache, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Numb, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Numb, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+Numb, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Seeing, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Seeing, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+Seeing, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Walking, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Walking, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+Walking, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(Email, SAMPWEIGHT, na.rm =T), sd = weighted.sd(Email, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+Email, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')

IPUMS %>% 
  group_by(HispFlag) %>% 
  summarise(mean = weighted.mean(PCRX, SAMPWEIGHT, na.rm =T), sd = weighted.sd(PCRX, SAMPWEIGHT, na.rm =T))
summary(svytable(~HispFlag+PCRX, subset(svy, HealthySubset == 'HealthySubset')), statistic = 'Chisq')
