library(tidyverse)
library(gplots)
library(lme4)
library(ggplot2)
library(ggeffects)
library(splines)
library(moments)
library(performance)
library(DHARMa)
library(car)
library(glmmLasso)
library(sjPlot)
library(sjstats)
library(cowplot)
library(sjmisc)
library(sjlabelled)
attach(MEMBHCLexFreq)
#splitting the dataset into L1/L2
L1Perf.1 <- filter(MEMBHCLexFreq, (L1 == "Spanish" & BNTLang == "Spa") | (L1 == "English" & BNTLang == "Eng"))
L1Cog.1 <- filter(L1Perf, (CognateBinary == 1))
L1NonCog.1 <- filter(L1Perf, (CognateBinary == 0))

mean(L1Cog.1$Accuracy)-mean(L1NonCog.1$Accuracy)
effsize::cohen.d(L1Cog.1$Accuracy, L1NonCog.1$Accuracy)

L1Perf$Item<-as.factor(L1Perf$Item)
L1Perf$Patient<-as.factor(L1Perf$Patient)
#naming things in L1
L1 <-as.factor(L1Perf.$L1)
Patient <- L1Perf.1$Patient
Item <- L1Perf.1$Item
ItemDifficulty <- as.numeric(L1Perf.1$ItemDifficulty)
L1Background_Exp <- L1Perf.1$L1Background_Exp
L2Background_Env <- L1Perf.1$L2Background_Environment
L2Use <- L1Perf.1$L2Use
Accuracy <- L1Perf.1$Accuracy
Age <- L1Perf.1$Age
Overlap <- L1Perf.1$Overlap
#log transformed due to extreme skewness
Freq <- log(L1Perf.1$Frequency)
##scaling covariates
ItemDifficulty.s <- scale(ItemDifficulty, center = T, scale = T)
L1Environment_Exp.s <- scale(L1Background_Exp, center = T, scale = T)
L2Background_Env.s <- scale(L2Background_Env, center = T, scale = T)
L2Use.s <- scale(L2Use, center = T, scale = T)
Age.s <- scale(Age, center = T, scale = T)
Overlap.s <- scale(Overlap, center = T, scale = T)
Freq.s <- scale(Freq, center = T, scale = T)
HBL1.1 <- glmer(Accuracy ~
                    ItemDifficulty.s + 
                    Overlap.s + 
                    L1Environment_Exp.s +  
                    L2Background_Env.s + 
                    L2Use.s + 
                    =L1Environment_Exp.s+
                    L1:L2Background_Env.s+
                    L1:L2Use.s+
                    L1,
                  data = L1Perf,
                  rnd = list(Patient=~1, Item=~1), 
                  family = binomial(link = "logit"),
                  lambda = 20,
                  switch.NR = TRUE,
                  final.re = TRUE)
summary(GLM1)
#based a few trials of lambdas, examining AIC/BIC
#L1.1 was clearly the best model.
HBL1.1 <- glmer(Accuracy ~
                (ItemDifficulty.s + 
                Overlap.s + 
                L1Environment_Exp.s +  
                L2Background_Env.s + 
                L2Use.s)^2 +
                (1|Patient)+(1|Item),
              data=L1Perf.1,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
HBdrop <- drop1(HBL1.1, test = "Chisq")
HBdrop
summary(L1.1)
L1.2 <- glmer(Accuracy~ItemDifficulty.s+Overlap.s+L1:L1Environment_Exp.s+L2Use.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.3 <- glmer(Accuracy~ItemDifficulty.s+Overlap.s+L1Environment_Exp.s+L2Use.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.4 <- glmer(Accuracy~ItemDifficulty.s+Overlap.s+L1+L1Environment_Exp.s+L2Use.s+ItemDifficulty.s:Overlap.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1.4)
compare_performance(L1.4, L1.1, rank = T)
anova(L1.1,L1.4)
#L1.1 is the winner
#tabled

compL1 <- compare_performance(L1.1, L1.2, rank  = TRUE)
compL1
tab_model(L1.1)

#plotting
plot_model(L1.1, show.values = TRUE, sort.est = T, title = "L1 Accuracy")
#plotting effects
ggpredict(L1.1, "Overlap.s") %>% plot()
ggpredict(L1.1, "L2Use.s") %>% plot()
ggpredict(L1.1, c("L1", "L1Environment_Exp.s")) %>% plot()
ggpredict(L1.1, c("Overlap.s", "ItemDifficulty.s")) %>% plot()
#model checks
testDispersion(L1.1)
simulationOutput <- simulateResiduals(fittedModel = L1.1, plot = F)
plot(simulationOutput)

#with frequency
L1.Freq.1 <- glmer(Accuracy~Freq.s+Overlap.s+L1:L1Environment_Exp.s+L2Use.s+Freq.s:Overlap.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1.Freq.1)
L1.Freq.2 <- glmer(Accuracy~Freq.s+Overlap.s+L1:L1Environment_Exp.s+L2Use.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1.Freq.2)
L1.Freq.3 <- glmer(Accuracy~Freq.s+Overlap.s+L1Environment_Exp.s+L2Use.s+(1|Patient)+(1|Item),
                   data=L1Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.Freq.4 <- glmer(Accuracy~Freq.s+Overlap.s+L1Environment_Exp.s+L2Use.s+Freq.s:Overlap.s+(1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.Freq.5 <- glmer(Accuracy ~ Freq.s + Overlap.s + L2Use.s+L1Environment.s+(1|Patient)+(1|Item),
                   data=L1Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L1.Freq.6 <- glmer(Accuracy ~ Freq.s + Overlap.s + L1Environment_Exp.s+(1|Patient)+(1|Item),
                   data=L1Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
compare_performance(L1.Freq.2,L1.Freq.6,rank = TRUE)
anova(L1.Freq.6, L1.Freq.2)
tab_model(L1.Freq.2)
#model checks
testDispersion(L1.Freq.2)
simulationOutput.1 <- simulateResiduals(fittedModel = L1.Freq.2, plot = F)
plot(simulationOutput.1)

#L2
L2Perf.1 <- filter(MEMBHCLexFreq, (L1 == "Spanish" & BNTLang == "Eng") | (L1 == "English" & BNTLang == "Spa"))
#effsize, quickly
L2Cog.1 <- filter(L2Perf.1, (CognateBinary == 1))
L2NonCog.1 <- filter(L2Perf.1, (CognateBinary == 0))

mean(L2Cog.1$Accuracy)-mean(L2NonCog.1$Accuracy)
cohen.d(L2Cog.1$Accuracy, L2NonCog.1$Accuracy)

L1Perf.1$Item<-as.factor(L1Perf$Item)
L1Perf.1$Patient<-as.factor(L1Perf$Patient)
L2Perf.1$Item<-as.factor(L2Perf$Item)
L2Perf.1$Patient<-as.factor(L2Perf$Patient)



L1 <-as.factor(L2Perf$L1)
Patient <- L2Perf.1$Patient
Item <- L2Perf.1$Item
ItemDifficulty <- as.numeric(L2Perf.1$ItemDifficulty)
L1Background_Exp <- L2Perf.1$L1Background_Exp
L2Background_Env <- L2Perf.1$L2Background_Environment
L2Use <- L2Perf.1$L2Use
Accuracy <- L2Perf.1$Accuracy
Age <- L2Perf$Age
Overlap <- L2Perf.1$Overlap
#log transformed due to extreme skewness
Freq <- log(L2Perf.1$Frequency)
##scaling covariates
ItemDifficulty.s <- scale(ItemDifficulty, center = T, scale = T)
L1Environment_Exp.s <- scale(L1Background_Exp, center = T, scale = T)
L2Background_Env.s <- scale(L2Background_Env, center = T, scale = T)
L2Use.s <- scale(L2Use, center = T, scale = T)
Age.s <- scale(Age, center = T, scale = T)
Overlap.s <- scale(Overlap, center = T, scale = T)
Freq.s <- scale(Freq, center = T, scale = T)

#lasso on L2

GLM2 <- glmmLasso(Accuracy ~ Age.s + 
                    ItemDifficulty.s + 
                    Overlap.s + 
                    L1Environment_Exp.s +  
                    L2Background_Env.s + 
                    L2Use.s + 
                    ItemDifficulty.s:Overlap.s+
                    L1:L1Environment_Exp.s+
                    L1:L2Background_Env.s+
                    L1:L2Use.s+
                    L1,
                  data = L2Perf,
                  rnd = list(Patient=~1, Item=~1), 
                  family = binomial(link = "logit"),
                  lambda = 5,
                  switch.NR = TRUE,
                  final.re = TRUE)
summary(GLM2)

#model, L2.1 was close
L2.1 <- glmer(Accuracy ~ ItemDifficulty.s + Overlap.s + L1:L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.1)
L2.4 <- glmer(Accuracy ~ ItemDifficulty.s + Overlap.s +L1+L1Environment_Exp.s+ L1:L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.2 <- glmer(Accuracy ~ ItemDifficulty.s + Overlap.s + ItemDifficulty.s:Overlap.s+L2Use.s + L1Environment_Exp.s+L2Background_Env.s+
                L1:L1Environment_Exp.s+L1:L2Background_Env.s+ (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
vif(L2.2)

summary(L2.4)
L2.7 <- glmer(Accuracy ~ ItemDifficulty.s:Overlap.s+ItemDifficulty.s + Overlap.s + L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.3 <- glmer(Accuracy ~ ItemDifficulty.s:Overlap.s+ItemDifficulty.s + Overlap.s + L1:L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.5 <- glmer(Accuracy ~ ItemDifficulty.s:Overlap.s+ItemDifficulty.s + Overlap.s +L1+L1Environment_Exp.s+ L1:L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.6 <- glmer(Accuracy ~ ItemDifficulty.s:Overlap.s+ItemDifficulty.s + Overlap.s + L2Use.s+ L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.5)
summary(L2.6)

compare_performance(L2.7, L2.6, rank = TRUE)
summary(L2.7)
vif(L2.7)
anova(L2.1, L2.3)

#tabled with L1
tab_model(L1.3, L2.3)
tab_model(L1.4, L2.7, collapse.ci = T, dv.labels = c("L1 Accuracy", "L2 Accuracy"))

#plotting
plot_model(L2.3, show.values = TRUE, sort.est = T, title = "L2 Accuracy")
#plotting effects
ggpredict(L2.3, "Overlap.s") %>% plot()
ggpredict(L2.3, "L2Background_Env.s") %>% plot()
ggpredict(L2.3, c("Overlap.s", "ItemDifficulty.s")) %>% plot()

#model checks
testDispersion(L2.3)
simulationOutput2 <- simulateResiduals(fittedModel = L2.3, plot = F)
plot(simulationOutput2)

#with frequency
L2.Freq.1 <- glmer(Accuracy ~ Freq.s:Overlap.s+Freq.s + Overlap.s + L1:L1Environment_Exp.s + (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.Freq.1)
L2.Freq.2 <- glmer(Accuracy ~ Freq.s + Overlap.s + L1:L1Environment_Exp.s + L2Use.s+(1|Patient)+(1|Item),
                   data=L2Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.Freq.3 <- glmer(Accuracy ~ Freq.s + Overlap.s + L1Environment_Exp.s+(1|Patient)+(1|Item),
                   data=L2Perf,family=binomial(link=logit),
                   control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
compare_performance(L2.Freq.1, L2.Freq.2,L2.Freq.3,rank = TRUE)

tab_model(L1.Freq.2,L2.Freq.3, collapse.ci = T, dv.labels = c("L1 Accuracy", "L2 Accuracy"))