library(tidyverse)
library(rstatix)
#converting csv to df
DCM <- data.frame(IndivBestFitEp.A)
#splitting this for further analyses within BWA
BWA <- DCM[1:24,]
#exponentiating values to reduce skew; factoring etc. for anova
DCM$Ep.A <- exp(DCM$Ep.A)
DCM$Ep.B <- exp(DCM$Ep.B)
DCM$Language <- as.factor(DCM$Language)
DCM$Group <- as.factor(DCM$Group)
#anova time!
aov.1 <- DCM %>% anova_test(Ep.A ~ Language*Group)
aov.1
aov.2 <- DCM %>% anova_test(Ep.B ~ Language*Group)
aov.2

#Ep.B for BWA is significant, so let's see if it's significant within the group, but between languages
BWA$Ep.A <- exp(BWA$Ep.A)
BWA$Ep.B <- exp(BWA$Ep.B)
BWA$Language <- as.factor(BWA$Language)
BWA$Group <- as.factor(BWA$Group)
aov.3 <- BWA %>% anova_test(Ep.A ~ Language)
aov.3
aov.4 <- BWA %>% anova_test(Ep.B ~ Language)
aov.4
#interaction plots; neither are significant
interaction.plot(x.factor     = Group,
                 trace.factor = Language, 
                 response     = Ep.A, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

interaction.plot(x.factor     = Group,
                 trace.factor = Language, 
                 response     = Ep.B, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")

#Ep.A & Ep.B across both languages
Ep.A.p1 = boxplot(Ep.A~Language,col=c("darkorchid4","darkgreen"),main="Ep.A Values for Single-Subject Best-Fit Model Between Languages", xlab="Language",ylab="Ep.A (in Hz)",
                  ylim=c(-0.75,1.75))
Ep.A.p2 = boxplot(Ep.A~Group,col=c("darksalmon","darkgoldenrod1"),main="Ep.A Values for Single-Subject Best-Fit Model Between Groups", xlab="Hemisphere",ylab="Ep.A (in Hz)",
                  ylim=c(-0.75,1.75))
Ep.A.p3 = boxplot(Ep.A~Language+Group,col=c("cyan1","darkblue"),main="Ep.A Values for Single-Subject Best-Fit Model Between LanguagexGroup", xlab="Group by Language",ylab="Ep.A (in Hz)",
                  ylim=c(-0.75,1.75))

Ep.B.p1 = boxplot(Ep.B~Language,col=c("darkorchid4","darkgreen"),main="Ep.B Values for Single-Subject Best-Fit Model", xlab="Language",ylab="Ep.B (in Hz)",
                  ylim=c(-0.75,1.75))
Ep.B.p2 = boxplot(Ep.B~Hemisphere,col=c("darksalmon","darkgoldenrod1"),main="Ep.B Values for Single-Subject Best-Fit Model", xlab="Hemisphere",ylab="Ep.B (in Hz)",
                  ylim=c(-0.75,1.75))
Ep.B.p3 = boxplot(Ep.B~Language+Group,col=c("cyan1","darkblue"),main="Ep.B Values for Single-Subject Best-Fit Model", xlab="Group by Language",ylab="Ep.B (in Hz)",
                  ylim=c(-0.75,1.75))