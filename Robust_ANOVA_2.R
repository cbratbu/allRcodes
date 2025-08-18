library(tidyverse)
library(WRS2)
library(bestNormalize)
library(moments)
str(Model6)
Model6$Group <- as.factor(Model)
M6$Ep.A <- as.numeric(M6$Ep.A)
Model6.1 <- data.frame(Model6)
Model6.1
med2way(Ep.A~Group*Language, data = Model6)
med2way(Ep.B~Group*Language, data = Model6)
summary(aov(Ep.A~Group*Language, data = M6))
summary(aov(Ep.B~Group*Language, data = M6))
Model6$Language <- as.factor(Model6$Language)
Model6$Group <- as.factor(Model6$Group)
nlevels(Model6$Language)

FDCM
MDCM <- Model6 %>% filter(Group == "PWA")
MDCM
MDCM1 <- Model6 %>% filter(Group == "Control")
MDCM1
med2way(MDCM$Ep.A~MDCM$Language)

Model6 == NULL
#trying bestNormalize
BNEp.A <- bestNormalize(FDCM$Ep.A, allow_orderNorm = FALSE)
BNEp.B <- bestNormalize(FDCM$Ep.B,allow_orderNorm = FALSE)