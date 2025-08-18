BFDCM1 <- data.frame(BFDCM)
BWA <- BFDCM1[1:24,]
Controls <- BFDCM1[25:50,]

summary(aov(exp(Ep.A)~Language*Group, data = BFDCM1))
summary(aov(exp(Ep.B)~Group*Language, data = BFDCM1))
summary(aov(exp(Ep.B)~Language, data = BWA))
summary(aov(exp(Ep.A)~Language, data = Controls))

attach(BFDCM1)
Plot2WayANOVA(Ep.B ~ Group * Language, BFDCM1, plottype = "line",
              title = "Model 6: Ep.B ~ Group*Language",
              errorbar.display = "none")

interaction.plot(x.factor     = Group,
                 trace.factor = Language, 
                 response     = Ep.B, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o")
