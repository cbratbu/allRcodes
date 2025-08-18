library(WRS2)
install.packages('bestNormalize')
library(bestNormalize)
library(moments)
FDCM
med2way(Ep.A~Group*Language, data = FDCM)
med2way(Ep.B~Group*Language, data = FDCM)
summary(aov(Ep.A~Group*Language, data = FDCM))
summary(aov(Ep.B~Group*Language, data = FDCM))


FDCM
FDCM4 <- FDCM %>% filter(Group == "PWA")
FDCM5 <- FDCM %>% filter(Group == "Control")
length(FDCM5$Ep.A)
length(FDCM5$Ep.B)
FDCM5

#trying bestNormalize
BNEp.A <- bestNormalize(FDCM$Ep.A, allow_orderNorm = FALSE)
BNEp.B <- bestNormalize(FDCM$Ep.B,allow_orderNorm = FALSE)