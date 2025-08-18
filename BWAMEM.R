library(tidyverse)
library(gplots)
install_github("yonicd/lmmen")
library(stats)
library(lmmen)
library(lme4)
library(ggplot2)
library(modelsummary)
library(ggeffects)
library(modelsummary)
library(parameters)
library(remotes)
library(gt)
library(cAIC4)
library(lmerTest)
library(magrittr)
library(splines)
library(moments)
library(DHARMa)
library(qqplotr)
library(see)
library(performance)
library(glmmLasso)
library(sjPlot)
library(sjstats)
library(effsize)
library(cowplot)
library(sjmisc)
library(sjlabelled)
#MEMBWA as dataset
#splitting the dataset into L1/L2
L1Perf <- filter(MEMBWA_LexFreq_ALL.4.27.21, (L1 == "Spanish" & BNTLang == "Spa") | (L1 == "English" & BNTLang == "Eng"))
L1Perf$Item<-as.factor(L1Perf$Item)
L1Perf$Patient<-as.factor(L1Perf$Patient)
#effsizes
L1Cog <- filter(L1Perf, (CognateBinary == 1))
L1NonCog <- filter(L1Perf, (CognateBinary == 0))


mean(L1Perf$Frequency, na.rm = TRUE)
mean(L2Perf$Frequency, na.rm = TRUE)
t.test(L1Perf$Frequency,L2Perf$Frequency)
models <- list()
models[['L1 Item Difficulty Model']] <- L1.2
models[['L1 Lexical Frequency Model']] <- Freqglm1
modelplot(models, facet = T)

plot_model(L1.2, type = "int")+theme_ggeffects()
t.test(L1Perf$Frequency, L2Perf$Frequency)
modelsummary(L1.2)
modelplot(L1.2)
model_performance(L1.2)
describe_distribution(L1Perf)

mean(L1Cog$Accuracy)-mean(L1NonCog$Accuracy)

effsize::cohen.d(L1Cog$Accuracy, L1NonCog$Accuracy)

#naming things in L1
L1 <-as.factor(L1Perf$L1)
Patient <- L1Perf$Patient
Item <- L1Perf$Item
ItemDifficulty <- as.numeric(L1Perf$ItemDifficulty)
L1Background <- L1Perf$L1Background
L1Use <- L1Perf$L1Use
L1Environment <- L1Perf$L1Environment
L2BackgroundEnvironment <- L1Perf$L2BackgroundEnvironment
L2Use <- L1Perf$L2Use
Accuracy <- L1Perf$Accuracy
Age <- L1Perf$Age
L1Perf$CognateBinary <- as.numeric(L1Perf$CognateBinary)
Overlap <- L1Perf$Overlap
PhonoOverlap <- MEMBWA$OverlapPhono
OrthoOverlap <- MEMBWA$OverlapOrtho
L1AQ <- L1Perf$L1AQ
L2AQ <- L1Perf$L2AQ

hist(L1Perf$Frequency)
#log transformed due to extreme skewness
Freq <- log(L1Perf$Frequency)
##scaling covariates
ItemDifficulty.s <- c(scale(ItemDifficulty, center = T, scale = T))
L1Background.s <- c(scale(L1Background, center = T, scale = T))
L1Use.s <- c(scale(L1Use, center = T, scale = T))
L1Environment.s <- c(scale(L1Environment, center = T, scale = T))
L2BackgroundEnvironment.s <- c(scale(L2BackgroundEnvironment, center = T, scale = T))
L2Use.s <- c(scale(L2Use, center = T, scale = T))
Age.s <- c(scale(Age, center = T, scale = T))
Overlap.s <- c(scale(Overlap, center = T, scale = T))
L1AQ.s <- c(scale(L1AQ, center = T, scale = T))
L2AQ.s <- c(scale(L2AQ, center = T, scale = T))
Freq.s <- scale(Freq, center = T, scale = T)
nnL2BackgroundEnvironment.s <- (L2BackgroundEnvironment.s)*-1
cor(L1AQ.s, L2AQ.s)
GLM1 <- glmmLasso(Accuracy ~ L1AQ.s + 
                    (ItemDifficulty.s+
                    Overlap.s)^2 + 
                    (L1Background.s +  
                    L1Use.s + 
                    L1Environment.s + 
                    L2BackgroundEnvironment.s + 
                    L2Use.s)^2,
                  data = L1Perf,
                  rnd = list(Patient=~1, Item=~1), 
                  family = binomial(link = "logit"),
                  lambda = 29.01,
                  switch.NR = TRUE,
                  final.re = TRUE)
GLM1$bic
summary(GLM1)

L1.1 <- glmer(Accuracy ~ L1AQ.s + L2AQ.s+
                (ItemDifficulty.s+
                Overlap.s+
                L1Background.s+
                L1Use.s+
                L1Environment.s+
                L2BackgroundEnvironment.s+
                L2Use.s)^2+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1.1)
droppedL1.1 <- drop1(L1.1, test = "Chisq")
droppedL1.1

L1.3 <- glmer(Accuracy ~ L1AQ.s +L2AQ.s+ 
                ItemDifficulty.s + Overlap.s+
                L1Background.s + L1Use.s+
                L1Environment.s + L2Use.s +
                L1Use.s + L2BackgroundEnvironment.s +
                ItemDifficulty.s:Overlap.s+
                L1Background.s:L1Use.s+
                L1Background.s:L1Environment.s   +
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s +L1Use.s:L2BackgroundEnvironment.s+
                L2BackgroundEnvironment.s:L2Use.s+
                (1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.4 <- glmer(Accuracy ~ L1AQ.s +L2AQ.s+ 
                ItemDifficulty.s + Overlap.s+
                L1Background.s + L1Use.s+
                L1Environment.s + L2Use.s +
                L1Use.s + L2BackgroundEnvironment.s +
                L1Background.s:L1Use.s+
                L1Background.s:L1Environment.s   +
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s +L1Use.s:L2BackgroundEnvironment.s+
                L2BackgroundEnvironment.s:L2Use.s+
                (1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.5 <- glmer(Accuracy ~ L2AQ.s+ 
                ItemDifficulty.s + Overlap.s+
                L1Background.s + L1Use.s+
                L1Environment.s + L2Use.s +
                L1Use.s + nnL2BackgroundEnvironment.s +
                L1Background.s:L1Use.s+
                L1Background.s:L1Environment.s   +
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s +L1Use.s:nnL2BackgroundEnvironment.s+
                L2BackgroundEnvironment.s:L2Use.s+
                (1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.6 <- glmer(Accuracy ~ L2AQ.s+ 
                ItemDifficulty.s + Overlap.s+
                L1Background.s + L1Use.s+
                L1Environment.s + L2Use.s +
                L1Use.s + L2BackgroundEnvironment.s +
                L1Background.s:L1Use.s+
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s +L1Use.s:L2BackgroundEnvironment.s+
                L2BackgroundEnvironment.s:L2Use.s+
                (1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.7 <- glmer(Accuracy ~ L2AQ.s+ 
                ItemDifficulty.s + Overlap.s+
                L1Background.s + L1Use.s+
                L1Environment.s + L2Use.s +
                L1Use.s + nnL2BackgroundEnvironment.s +
                L1Background.s:L1Use.s+
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s +
                (1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1.4)
testDispersion(L1.7)
simulateResiduals(fittedModel = L1.7, plot = T)
drop1(L1.2)
compare_performance(L1.5, L1.6, L1.7, rank = TRUE)

?stepcAIC
summary(L1.1.step)
#based a few trials of lambdas, examining AIC/BIC of L1.2, L1.3 (not shown), 
#L1.1 was clearly the best model.
L1.1 <- glmer(Accuracy ~ (L1AQ.s + L2AQ.s + 
                            ItemDifficulty.s+
                            Overlap.s+ 
                            L1Background.s +  
                            L1Use.s + 
                            L1Environment.s + 
                            L2BackgroundEnvironment.s + 
                            L2Use.s +
                            L1)^2+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(L1.1)
testDispersion(L1.3)
simulateResiduals(fittedModel = L1.3, plot = T)
L1.3 <- glmer(Accuracy~L1AQ.s+ItemDifficulty.s+L2Use.s+L1Use.s+L1+Overlap.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1.3)
L1.5 <- glmer(Accuracy~L1AQ.s+ItemDifficulty.s+L1Use.s+L1:L1Use.s+Overlap.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.2 <- glmer(Accuracy~L1AQ.s+
                   ItemDifficulty.s+
                   Overlap.s+
                   L1Use.s+
                   L1+
                   ItemDifficulty.s:Overlap.s+
                   (1|Patient)+(1|Item),
                 data=L1Perf,family=binomial(link=logit),
                 control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.4 <- glmer(Accuracy~L1AQ.s+ItemDifficulty.s:Overlap.s+ItemDifficulty.s+L2Use.s+L1Use.s+L1+Overlap.s+(1|Patient)+(1|Item),
                      data=L1Perf,family=binomial(link=logit),
                      control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

L1ranks <- compare_performance(L1.1, L1.2, L1.3, L1.4, rank = T)
L1ranks
anova(L1.2,L1.5)
vif(L1.2)
vif(L1.5)
compare_performance(L1.5,L1.2)
L1.2 %>% model_parameters() %>% plot()
summary(L1.2)
#tabled
tab_model(L1.2)

#model checks
testDispersion(L1.3)
simulateResiduals(fittedModel = L1.2, plot = T)

#plots
plot_model(L1.2, show.values = TRUE, sort.est = T, title = "L1 Accuracy")
#plotting effects
ggpredict(L1.2, "Overlap.s") %>% plot()
ggpredict(L1.2, "L1Use.s") %>% plot()
ggpredict(L1.2, "L2Use.s") %>% plot()
ggpredict(L1.2, "L1") %>% plot()

#tried for overlap but failed because we're doing a logistic regression. oops.
effects_Overlap.s <- effects::effect(term = "Overlap.s", mod = L1.2)
summary(effects_Overlap.s)
x_over <- as.data.frame(effects_Overlap.s)
overlap_plot <- ggplot() +
  geom_point(data = L1Perf, aes(Overlap.s, Accuracy)) +
  geom_point(data = x_over, aes(x = Overlap.s, y = fit), color = "blue") +
  geom_line(data = x_over, aes(x = Overlap.s, y = fit), color = "blue") +
  geom_ribbon(data = x_over, aes(x = Overlap.s, ymin = lower, ymax = upper), alpha = 0.3, fill = "blue") +
  labs(x = "Overlap (centered & scaled)", y = "Accuracy in L1")

overlap_plot
#with frequency
L1Freq.1 <- glmer(Accuracy ~ L1AQ.s + L2AQ.s+
                (Freq.s+
                   Overlap.s+
                   L1Background.s+
                   L1Use.s+
                   L1Environment.s+
                   L2BackgroundEnvironment.s+
                   L2Use.s)^2+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1Freq.1)
L1Freq1.1 <- drop1(L1Freq.1, test = "Chisq")
L1Freq1.1
testDispersion(L1.3)
simulateResiduals(fittedModel = L1.3, plot = T)

summary(L1.Freq.1)

tab_model(L1.Freq.1)
plot_model(L1.Freq.1, show.values = TRUE, sort.est = T, title = "L1 Accuracy")

L1.Freq.2 <- glmer(Accuracy~L1AQ.s+Freq.s+L2Use.s+L1Use.s+L1+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

L1.Freq.3 <- glmer(Accuracy~L1AQ.s+Freq.s:Overlap.s+L1Use.s+L1+(1|Patient)+(1|Item),
                   data=L1Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
#plotting effects
compare_performance(L1.Freq.1,L1.Freq.3)

ggpredict(L1.Freq.1, "Freq.s") %>% plot()
ggpredict(Freqglm1, "L1Use.s") %>% plot()
ggpredict(L1.Freq.1, c("Overlap.s", "Freq.s")) %>% plot()

#L2
L2Perf <- filter(MEMBWA_LexFreq_ALL.4.27.21, (L1 == "Spanish" & BNTLang == "Eng") | (L1 == "English" & BNTLang == "Spa"))
L2Cog <- filter(L2Perf, (CognateBinary == 1))
L2NonCog <- filter(L2Perf, (CognateBinary == 0))
mean(L2Cog$Accuracy)-mean(L2NonCog$Accuracy)
cohen.d(L2Cog$Accuracy, L2NonCog$Accuracy)
#variables
L2Perf$Item<-as.factor(L2Perf$Item)
L2Perf$Patient<-as.factor(L2Perf$Patient)
L1 <-as.factor(L2Perf$L1)
Patient <- L2Perf$Patient
Item <- L2Perf$Item
ItemDifficulty <- as.numeric(L2Perf$ItemDifficulty)
L1Background <- L2Perf$L1Background
L1Use <- L2Perf$L1Use
L1Environment <- L2Perf$L1Environment
L2BackgroundEnvironment <- L2Perf$L2BackgroundEnvironment
L2Use <- L2Perf$L2Use
Accuracy <- L2Perf$Accuracy
Age <- L2Perf$Age
Overlap <- L2Perf$Overlap
L1AQ <- L2Perf$L1AQ
L2AQ <- L2Perf$L2AQ
#log transformed due to extreme skewness
Freq <- log(L2Perf$Frequency)
##scaling covariates
ItemDifficulty.s <- scale(ItemDifficulty, center = T, scale = T)
L1Background.s <- scale(L1Background, center = T, scale = T)
L1Use.s <- scale(L1Use, center = T, scale = T)
L1Environment.s <- scale(L1Environment, center = T, scale = T)
L2BackgroundEnvironment.s <- scale(L2BackgroundEnvironment, center = T, scale = T)
L2Use.s <- scale(L2Use, center = T, scale = T)
Age.s <- scale(Age, center = T, scale = T)
Overlap.s <- scale(Overlap, center = T, scale = T)
L1AQ.s <- scale(L1AQ, center = T, scale = T)
L2AQ.s <- scale(L2AQ, center = T, scale = T)
Freq.s <- scale(Freq, center = T, scale = T)

#lasso on L2

GLM2 <- glmmLasso(Accuracy ~ Age.s + 
                    L2AQ.s + 
                    ItemDifficulty.s+
                    Overlap.s+
                    (ItemDifficulty.s:Overlap.s) + 
                    (L1Background.s +  
                       L1Use.s + 
                       L1Environment.s + 
                       L2BackgroundEnvironment.s + 
                       L2Use.s +
                       L1)^2,
                  data = L2Perf,
                  rnd = list(Patient=~1, Item=~1), 
                  family = binomial(link = "logit"),
                  lambda = 15,
                  switch.NR = TRUE,
                  final.re = TRUE)
summary(GLM2)
L2.1 <- glmer(Accuracy ~ L1AQ.s + L2AQ.s+
                    (ItemDifficulty.s+
                       Overlap.s+
                       L1Background.s+
                       L1Use.s+
                       L1Environment.s+
                       L2BackgroundEnvironment.s+
                       L2Use.s)^2+(1|Patient)+(1|Item),
                  data=L2Perf,family=binomial(link=logit),
                  control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.1)

L2.1drop <- drop1(L2.1, test = "Chisq")
L2.1drop
testDispersion(L1.3)
simulateResiduals(fittedModel = L1.3, plot = T)



L2lasso <- glmer(Accuracy~L2AQ.s+ItemDifficulty.s+Overlap.s+ItemDifficulty.s:Overlap.s+L2Use.s+(1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2lasso)
L2.1 <- glmer(Accuracy~L2AQ.s+ItemDifficulty.s+Overlap.s+L2Use.s+(1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.1)
L2.2 <- glmer(Accuracy~L2AQ.s+ItemDifficulty.s+Overlap.s+L2Use.s+L1+(1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
compare_performance(L2.2, L2.1)

#plotting
plot_model(L2.1, show.values = TRUE, sort.est = T, title = "L2 Accuracy")
#plotting effects
ggpredict(L2.1, "Overlap.s") %>% plot()
ggpredict(L2.1, "L2Use.s") %>% plot()
#tabled with L1
tab_model(L1.5, L2.1, collapse.ci = T, dv.labels = c("L1 Accuracy", "L2 Accuracy"))

#model checks
testDispersion(L2.1)
simulationOutput2 <- simulateResiduals(fittedModel = L2.1, plot = F)
plot(simulationOutput2)

#with frequency
L2.1Freq <- glmer(Accuracy ~ L1AQ.s + L2AQ.s + 
                            (Freq.s+
                            Overlap.s+ 
                            L1Background.s +  
                            L1Use.s + 
                            L1Environment.s + 
                            L2BackgroundEnvironment.s + 
                            L2Use.s +
                            L1)^2+(1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.1FreqStep <- drop1(L2.1Freq, test = "Chisq")

L2.Freq.1 <- glmer(Accuracy~L2AQ.s+Freq.s+Overlap.s+Freq.s:Overlap.s+L2Use.s+(1|Patient)+(1|Item),
                 data=L2Perf,family=binomial(link=logit),
                 control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.Freq.2 <- glmer(Accuracy~L2AQ.s+Freq.s+Overlap.s+L2Use.s+(1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.Freq.2)
L2.Freq.3 <- glmer(Accuracy~L2AQ.s+Freq.s+L2Use.s+(1|Patient)+(1|Item),
                   data=L2Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.Freq.3)
vif(L2.Freq.2)
L2.Freq.4 <- glmer(Accuracy~L2AQ.s+Freq.s+(1|Patient)+(1|Item),
                   data=L2Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
compare_performance(L2.Freq.1,L2.Freq.2,L2.Freq.3,L2.Freq.4, rank=T)
anova(L2.Freq.1,L2.Freq.2,L2.Freq.3,L2.Freq.4)
tab_model(L1.Freq.1,L2.Freq.3, collapse.ci = T, dv.labels = c("L1 Accuracy", "L2 Accuracy"))
