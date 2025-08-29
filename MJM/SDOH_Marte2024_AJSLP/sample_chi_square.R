ID <- rep(1:50)
Living <- rbinom(50, 1, 0.5)
Funeral <- rbinom(50, 1, 0.5)
Country <- rep(c('USA', 'UK', 'Canada', 'Mexico', 'Germany'), each = 10)
Weight <- rnorm(50,1, .1)
PSU <- rep(1:2, 50)
BMI <- rnorm(50, 25,1)

df <- data.frame(ID, Living, Funeral, Country, Weight, PSU, BMI)

surveydes <- svydesign(~PSU, weights = ~Weight, data = df, nest = TRUE, check.strata = FALSE)


df %>% 
  group_by(Living) %>% 
  summarise(mean = weighted.mean(BMI, Weight, na.rm =T), sd = weighted.sd(BMI, Weight, na.rm =T))
svychisq(~Life+Funeral, svy1, statistic = 'Chisq')