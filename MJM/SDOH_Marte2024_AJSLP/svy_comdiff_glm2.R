hisp_nhw_comdiff_glm <- svyglm(ComDiff~ Hispanic+ Education + SexMale  + AGE + South + Married + FAMSIZE + 
                    BMICALC + Smoker, family = quasibinomial, design = svydesign)
summ(hisp_nhw_comdiff_glm, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))

glm_test2 <- svyglm(ComDiff ~ FactorSDOHScore+SexMale + AGE + 
                  South + Married + FAMSIZE + BMICALC + Smoker,
                family = binomial, design = svydesign)
summ(glm_test2, exp = TRUE, confint = TRUE,digits = getOption("jtools-digits", default = 3))
