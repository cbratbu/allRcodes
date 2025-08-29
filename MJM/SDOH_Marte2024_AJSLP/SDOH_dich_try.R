firstmod <- svyglm(dichSDOHScore2 ~ Hispanic + SexMale +AGE + 
                 South + Married + FAMSIZE + BMICALC + Smoker, family = binomial, design = olrsurv)
summary(firstmod)

secondmod <- svyglm(ComDiff~dichSDOHScore2 +Hispanic + SexMale +AGE + 
  South + Married + FAMSIZE + BMICALC + Smoker, family = binomial, design = olrsurv)
summary(secondmod)


sdoh_dich_med <- mediate(firstmod, secondmod, 
                                treat = "Hispanic", mediator = "dichSDOHScore2",
                                sims = 50, robustSE = T)
summary(sdoh_dich_med)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(IPUMS_glm2$FactorSDOHScore)
