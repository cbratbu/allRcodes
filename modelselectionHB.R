library(tidyverse)
library(moments)
library(performance)
library(MASS)
library(ggpubr)
library(sjPlot)
library(ggeffects)
library(sjmisc)
library(ggplot2)
HBL1.5 <- glmer(Accuracy ~
                  ItemDifficulty.s + 
                     Overlap.s + 
                  L1Environment_Exp.s+
                  ItemDifficulty.s:Overlap.s+
                  Overlap.s:L2Use.s+
                     L2Use.s +
                  (1|Patient)+(1|Item),
                data=L1Perf.1,family=binomial(link=logit),
                control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
HBdrop <- drop1glm(HBL1.1, test = "Chisq")
HBdrop

summary(HBL1.5)
compare_performance(HBL1.2, HBL1.5, rank = TRUE)
testDispersion(HBL1.5)
simulateResiduals(fittedModel = HBL1.5, plot = T)
vif(HBL1.5)

HBL1.Freq.6 <- glmer(Accuracy ~
                  Freq.s + 
                     Overlap.s + 
                     L1Environment_Exp.s +  
                     L2Use.s +
                    Overlap.s:L2Use.s +
                    +L2Background_Env.s+
                    L2Use.s:L2Background_Env.s+
                  (1|Patient)+(1|Item),
                data=L1Perf.1,family=binomial(link=logit),
                control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
HBL1.Freq.1drop <- drop1(HBL1.Freq.1, test = "Chisq")
HBL1.Freq.1drop

summary(HBL1.Freq.3)
compare_performance(HBL1.Freq.6, HBL1.Freq.3, rank = TRUE)
testDispersion(HBL1.Freq.3)
simulateResiduals(fittedModel = HBL1.Freq.3, plot = T)
vif(HBL1.Freq.3)




HBL2.4 <- glmer(Accuracy ~
                  ItemDifficulty.s + 
                     Overlap.s + 
                     L2Background_Env.s + 
                     L2Use.s+ItemDifficulty.s:Overlap.s + Overlap.s:L2Use.s+
                  (1|Patient)+(1|Item),
                data=L2Perf.1,family=binomial(link=logit),
                control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
HBL2.1drop <- drop1(HBL2.1, test = "Chisq")
HBL2.1drop
summary(HBL2.3)
compare_performance(HBL2.4, HBL2.3, rank = TRUE)
testDispersion(HBL2.3)
simulateResiduals(fittedModel = HBL2.3, plot = T)
vif(HBL2.3)


HBL2.Freq.4 <- glmer(Accuracy ~
                       Freq.s + 
                          Overlap.s + 
                          L2Background_Env.s + 
                          L2Use.s +
                       (1|Patient)+(1|Item),
                     data=L2Perf.1,family=binomial(link=logit),
                     control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
HBL2.Freq.1drop <- drop1(HBL2.Freq.1, test = "Chisq")
HBL2.Freq.1drop
summary(HBL2.Freq.4)
compare_performance(HBL2.Freq.3,HBL2.Freq.4, rank = TRUE)
testDispersion(HBL2.Freq.4)
simulateResiduals(fittedModel = HBL2.Freq.4, plot = T)
vif(HBL2.3)
