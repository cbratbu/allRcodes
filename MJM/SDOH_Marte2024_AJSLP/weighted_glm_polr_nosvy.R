library(mediation)
olrsurv2 <- svydesign(~PSU, weights = ~SAMPWEIGHTAdj, strata = ~STRATA, data = subset(join_IPUMS, StrokeSubset == 1), nest = TRUE, check.strata = FALSE)

IPUMS_glm2$HispanicNHW <- ifelse(IPUMS_glm2$Hispanic==1, "Hispanic", "non-Hispanic white")

as.factor(IPUMS_glm2$Hispanic)
olr_test4 <-polr(FactorSDOHScore ~ Hispanic +SexMale + AGE + 
                   South + Married + FAMSIZE + BMICALC + Smoker,
                 method = "logistic",Hess = TRUE,data=IPUMS_glm2)
summary(olr_test4)
glm_test <- glm(ComDiff ~ FactorSDOHScore+Hispanic+SexMale + AGE + 
                  South + Married + FAMSIZE + BMICALC + Smoker,
            family = binomial, data = IPUMS_glm2)
summary(glm_test)

summary(svyglm(ComDiff~FactorSDOHScore + Hispanic + SexMale +AGE + 
                 South + Married + FAMSIZE + BMICALC + Smoker, family = binomial, design = olrsurv))
dichSDOHScore
sdoh_score_mediation <- mediate(olr_test4, glm_test, 
                                treat = "Hispanic", mediator = "FactorSDOHScore")

summary(sdoh_score_mediation)

sdoh_score_mediation2 <- mediate(olr_test3, glm_test, 
                                treat = "Hispanic", mediator = "FactorSDOHScore",
                                sims = 5, robustSE = T)

ctable <- coef(summary(olrsvy))
summary(olr_test4)
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
ctable2 <- as.data.frame(ctable)
ctable2$`p value`
olrsvy<-svyolr(FactorSDOHScore ~ Hispanic +SexMale + AGE + 
                 South + Married + FAMSIZE + BMICALC + Smoker,
               method = "logistic",design = svydesign)
summary(olrsvy)
ci <- confint(olrsvy)
exp(coef(olrsvy))
exp(cbind(OR = coef(olrsvy), ci))
