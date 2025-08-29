step <- tibble(
  IndicatorID = c(41, 42, 43, 44, 45, 46), 
  Step = c(1, 2, 1, 4, 5, 6), 
  StepName = c('left', 'right', 'up', 'down', 'under', 'over'))
step

iData <- tibble(
  IndicatorID = c(seq(from = 1, to = 43)), 
  InputA = runif(43), 
  InputB = runif(43)) %>%
  mutate(iresult = InputA + InputB)

iData



hispmatch$NHWMerge <- as.numeric(hispmatch$NHWMerge)
join_IPUMS2 <- left_join(join_IPUMS, hispmatch)
sum(join_IPUMS2$NHWMerge==0, na.rm=T)
svy <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = IPUMS_glm, nest = TRUE, check.strata = FALSE)

sum(IPUMS_glm$HispStrokeFlag==0,na.rm=T)
IPUMS_glm %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(LANY_Bin, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LANY_Bin, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+LANY_Bin, svy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+LANY_Bin, svy, statistic = 'Chisq')$p.value

IPUMS_glm %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(LimByChronDx, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(LANY_Bin, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+LANY_Bin, hispmatchsvy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+LANY_Bin, hispmatchsvy, statistic = 'Chisq')$p.value

IPUMS_glm %>% 
  group_by(HispStrokeFlag) %>% 
  summarise(mean = weighted.mean(ComDiff, SAMPWEIGHTAdj, na.rm =T), sd = weighted.sd(ComDiff, SAMPWEIGHTAdj, na.rm =T))
svychisq(~HispStrokeFlag+ComDiff, svy, statistic = 'Chisq')$statistic
svychisq(~HispStrokeFlag+ComDiff, svy, statistic = 'Chisq')$p.value
