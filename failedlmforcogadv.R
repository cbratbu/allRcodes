library(tidyverse)
library(moments)
library(performance)
library(MASS)
library(ggpubr)
library(sjPlot)
library(ggeffects)
library(sjmisc)
library(ggplot2)
View(BPWAforLinReg)
Patient<-as.factor(BPWAforLinReg$Patient)
L1Background.s <- c(scale(BPWAforLinReg$L1Background, center = T, scale = T))
L1Use.s <- c(scale(BPWAforLinReg$L1Use, center = T, scale = T))
L1Environment.s <- c(scale(BPWAforLinReg$L1Environment, center = T, scale = T))
L2BackgroundEnvironment.s <- c(scale(BPWAforLinReg$L2BackgroundEnvironment, center = T, scale = T))
L2Use.s <- c(scale(BPWAforLinReg$L2Use, center = T, scale = T))
Age.s <- scale(BPWAforLinReg$Age, center = T, scale = T)
L1AQ.s <- c(scale(BPWAforLinReg$L1AQ, center = T, scale = T))
L2AQ.s <- c(scale(BPWAforLinReg$L2AQ, center = T, scale = T))
CogAdv <- BPWAforLinReg$CogAdvCol
nL2BackgroundEnvironment.s <- (L2BackgroundEnvironment.s)*-1
L2BackgroundEnvironment.s.1 <- L2BackgroundEnvironment.s
mod1 <- lm(CogAdv ~ (L1Background.s +
               L1Use.s +
               L1Environment.s +
                 L2BackgroundEnvironment.s.1 +
               L2Use.s)^2 +
               L1AQ.s + L2AQ.s, data = BPWAforLinReg)
summary(chosen)
chosen1 <- stepAIC(mod1, direction = "backward")
plot_model(chosen)
plot_model(chosen, type = "pred", terms = c("L1Background.s", "L1Use.s"))
plot_model(chosen, type = "pred", terms = c("L1Background.s", "L2BackgroundEnvironment.s.1"))
plot_model(chosen, type = "pred", terms = c("L1Background.s", "L2Use.s"))
plot_model(chosen, type = "pred", terms = c("L1Use.s", "L1Environment.s"))
plot_model(chosen, type = "pred", terms = c("L1Use.s", "L2BackgroundEnvironment.s.1 "))
plot_model(chosen, type = "pred", terms = c("L1Environment.s","L2BackgroundEnvironment.s.1"))
ggpredict(chosen, "L2BackgroundEnvironment.s.1") %>% plot()
ggpredict(chosen, "L1AQ.s") %>% plot()
