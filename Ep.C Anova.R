Ep.C.exp <- exp(EpCANOVA$Ep.C)
Language1 <- as.factor(EpCANOVA$Language)
Group1 <- as.factor(EpCANOVA$Group)
Patients <- EpCANOVA[1:11,]
m5 <- aov(Ep.C.exp ~ Language, data =Patients)

m3 <- aov(Ep.C.exp ~ Language1*Group1)
summary(m3)
capture.output(summary(m3),file="test3.doc")
