##SELECTED MODEL FOR L1 ITEM NUM ANALYSIS -- MODEL 7
summary(L1.7)
plot_model(L1.7)
plot_model(L1.7, type = "pred", terms = c("ItemDifficulty.s", "Overlap.s")) #same in cogadv
plot_model(L1.7, type = "pred", terms = c("Overlap.s", "L2Use.s")) #same in cogadv
ggpredict(L1.7, "L2Background_Env.s") %>% plot()
##SELECTED MODEL FOR L1 FREQUENCY ANALYSIS -- MODEL 3
summary(L1Freq.1)
plot_model(L1Freq.1, type = "pred", terms = c("L1Background.s", "L1Use.s")) #same in cogadv
plot_model(L1Freq.1, type = "pred", terms = c("L1Background.s", "L2Use.s")) #same in cogadv
plot_model(L1Freq.1, type = "pred", terms = c("L1Use.s", "L1Environment.s")) #same in cogadv
ggpredict(L1Freq.1, "L2BackgroundEnvironment.s") %>% plot()
##SELECTED MODEL FOR L2 Item NUM ANALYSIS -- MODEL 1
summary(L2.1)
plot_model(L2.1)
plot_model(L2.1, type = "pred", terms = c("L1Background.s", "L1Use.s")) #same in cogadv
plot_model(L2.1, type = "pred", terms = c("L1Background.s", "L2Use.s")) #same in cogadv
plot_model(L2.1, type = "pred", terms = c("L1Use.s", "L1Environment.s")) #same in cogadv
ggpredict(L2.1, "L2BackgroundEnvironment.s") %>% plot()

##SELECTED MODEL FOR L2 FREQUENCY-- MODEL 3
plot_model(L2.3Freq)
plot_model(L2.3Freq, type = "pred", terms = c("L1Background.s", "L1Use.s")) #same in cogadv
plot_model(L2.3Freq, type = "pred", terms = c("L1Background.s", "L2Use.s")) #same in cogadv
plot_model(L2.3Freq, type = "pred", terms = c("L1Use.s", "L1Environment.s")) #same in cogadv
ggpredict(L2.3Freq, "nnL2BackgroundEnvironment.s") %>% plot()