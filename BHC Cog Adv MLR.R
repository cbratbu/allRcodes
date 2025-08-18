NEW2 <- BHC.CogAdv.4.16.21 %>% distinct()
View(NEW)
Patient<-as.factor(NEW2$Patient)
L1BackgroundExp.s <- c(scale(NEW2$L1Background_Exp, center = T, scale = T))
L2BackgroundEnv.s <- c(scale(NEW2$L2Background_Environment, center = T, scale = T))
L2Use.s <- c(scale(NEW2$L2Use, center = T, scale = T))
Age.s <- scale(NEW2$Age, center = T, scale = T)
CogAdv.s <- NEW2$CogAdv
nL2BackgroundEnv.s <- L2BackgroundEnv.s*-1
hist(NEW$CogAdv)

mod1 <- lm(CogAdv.s ~ (L1BackgroundExp.s +
                         L2BackgroundEnv.s +
             L2Use.s)^2, data = NEW2)
summary(mod1)
chosenhb <- stepAIC(mod1, direction = "both")

summary(chosenhb)
plot_model(chosenhb, type = "pred", terms = c("L2Use.s", "nL2BackgroundEnv.s"))
ggpredict(chosenhb, "L2BackgroundEnv.s") %>% plot()

plot(chosen)

