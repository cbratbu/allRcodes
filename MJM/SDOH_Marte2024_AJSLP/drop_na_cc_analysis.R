vars <- c("AGE", "MStLivPtnr", "FAMSIZE", "Smoker", "ALC5UPYR", "HRSLEEP",
          "Cit", "South", "BP", "Education", "ERUse", "SurgeryYorN12Mos", "VisitYN", "UsualPlace",
          "DelAppt", "CholDx", "Db", "Htn", "Chol", "BP", "ADL", "LimByChronDx", 
          "ChronStroke", "FnLimStroke", "Hopelessness", "Sadness", "Worthless", "South", "CHD", "PovertyAdj", "Health")

vars2 <- c("AGE", "SexMale", "LANY_Bin", "Married", "FAMSIZE", "BMICALC", "Hispanic",
           "Cit", "South", "Education","PovertyAdj", "UsualPlace", "WebUse")

vars3 <- c("AGE", "SexMale", "LANY_Bin", 'Smoker', 'BP', 'Chol', 'DelAppt',
           "Married", "FAMSIZE", "BMICALC", "Hispanic", "ComDiff",
           "Cit", "South", "Education","PovertyAdj", "WebUse")

IPUMS_cc <- IPUMS
IPUMS_cc <- IPUMS_cc %>% drop_na(NHISPID, any_of(vars3))
sum(IPUMS_cc$HispStrokeFlag == 0, na.rm=T)
sum(IPUMS_cc$HispStrokeFlag == 1, na.rm=T)
svy2 <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = IPUMS_glm, nest = TRUE, check.strata = FALSE)
View(svy2)
#test
IPUMS_cc$PovertyAdj <- factor(IPUMS_cc$PovertyAdj)
IPUMS_cc$PovertyAdj <- relevel(factor(IPUMS_cc$PovertyAdj), ref = ">=400%")

#ADL regressions

testglm <- svyglm(LANY_Bin~ SexMale + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0)


testglm1 <- svyglm(LANY_Bin~ SexMale + Education  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1)
summary(testglm)
summary(testglm1)
plot_summs(testglm, testglm1, model.names = c("Non-Hispanic White", "Hispanic"))

## test for any citizenship effect

testglm2 <- svyglm(LANY_Bin~ Cit+ SexMale + Education  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
                  subset = StrokeNWH == 0)


testglm3 <- svyglm(LANY_Bin~ Cit +SexMale + Education  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
                   subset = StrokeHisp == 1)
plot_summs(testglm2, testglm3, model.names = c("Non-Hispanic White", "Hispanic"))

#funlim regressions
summary(svyglm(FnLimStroke~ Education  + PovertyAdj + AGE + South + MStLivPtnr + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0))

summary(svyglm(FnLimStroke~ Education  +PovertyAdj + AGE + South + MStLivPtnr + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1))

#diff using language

summ(svyglm(DiffUsingLangYN~ Education  + PovertyAdj + AGE + South + MStLivPtnr + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0))

summ(svyglm(DiffUsingLangYN~ Education  + PovertyAdj + AGE + South + MStLivPtnr + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1))
