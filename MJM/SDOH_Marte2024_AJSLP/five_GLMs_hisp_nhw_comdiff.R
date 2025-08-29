library(srvyr)
library(survey)
surveydesign <- surveydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = IPUMS_dataset, nest = TRUE, check.strata = FALSE)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
###education
edu_nhw <- svyglm(ComDiff~ Education + SexMale  + AGE + South + Married + FAMSIZE + 
                 BMICALC + Smoker, family = quasibinomial, design = surveydesign,
               subset = White == 1)

summ(edu_nhw, exp = TRUE, confint = TRUE, digits = getOption("jtools-digits", default = 3))
edu_h <- svyglm(ComDiff~ Education + SexMale  + AGE + South + Married + FAMSIZE + 
                 BMICALC + Smoker, family = quasibinomial, design = surveydesign,
               subset = StrokeHisp == 1)
summ(edu_nhw, exp = TRUE, confint = TRUE, digits = getOption("jtools-digits", default = 3))
###poverty level
povlvl_nhw <- svyglm(ComDiff~ PovertyLevel + SexMale  + AGE + South + Married + FAMSIZE + 
                 BMICALC + Smoker, family = quasibinomial, design = surveydesign,
               subset = StrokeNWH == 0)
summ(povlvl_nhw, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

povlvl_h <- svyglm(ComDiff~ PovertyLevel + SexMale  + AGE + South + Married + FAMSIZE + 
                 BMICALC + Smoker, family = quasibinomial, design = surveydesign,
               subset = StrokeHisp == 1)
summ(povlvl_h, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

####citizenship
cit_nhw <- svyglm(ComDiff~ Cit + SexMale  + AGE + South + Married + FAMSIZE + 
                       BMICALC + Smoker, family = binomial, design = surveydesign,
                     subset = StrokeNWH == 0)

summ(cit_nhw, exp = TRUE, confint = TRUE, robust = T, 
     digits = getOption("jtools-digits", default = 3))

cit_h <- svyglm(ComDiff~ Cit + SexMale  + AGE + South + Married + FAMSIZE + 
                     BMICALC + Smoker, family = quasibinomial, design = surveydesign,
                   subset = StrokeHisp == 1)
summ(cit_h, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

##delays in care
del_nhw <- svyglm(ComDiff~ DelayInCare + SexMale  + AGE + South + Married + FAMSIZE + 
                    BMICALC + Smoker, family = quasibinomial, design = surveydesign,
                  subset = StrokeNWH == 0)
summ(del_nhw, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

del_h <- svyglm(ComDiff~ DelayInCare + SexMale  + AGE + South + Married + FAMSIZE + 
                  BMICALC + Smoker, family = quasibinomial, design = surveydesign,
                subset = StrokeHisp == 1)
summ(del_h, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

##web use
web_nhw <- svyglm(ComDiff~ WebUse + SexMale  + AGE + South + Married + FAMSIZE + 
                    BMICALC + Smoker, family = quasibinomial, design = surveydesign,
                  subset = StrokeNWH == 0)
summ(web_nhw, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

web_h <- svyglm(ComDiff~ WebUse + SexMale  + AGE + South + Married + FAMSIZE + 
                  BMICALC + Smoker, family = quasibinomial, design = surveydesign,
                subset = StrokeHisp == 1)
summ(web_h, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))
