##SELECTED MODEL FOR L1 ITEM NUM ANALYSIS -- MODEL 7
summary(HBL1.5)
compare_performance(HBL1.5)
plot_model(HBL1.5)
plot_model(HBL1.2, type = "pred", terms = c("ItemDifficulty.s", "Overlap.s")) #same in cogadv
plot_model(HBL1.2, type = "pred", terms = c("Overlap.s", "L2Use.s")) #same in cogadv
ggpredict(HBL1.2, "L2Background_Env.s") %>% plot()
##SELECTED MODEL FOR L1 FREQUENCY ANALYSIS -- MODEL 3
summary(HBL1.Freq.3)
plot_model(HBL1.Freq.3)
plot_model(HBL1.Freq.3, type = "pred", terms = c("Overlap.s", "L2Use.s")) #same in cogadv
ggpredict(HBL1.Freq.3, "Overlap.s") %>% plot()
##SELECTED MODEL FOR L2 Item NUM ANALYSIS -- MODEL 1
summary(HBL2.3)
plot_model(HBL2.3)
plot_model(HBL2.3, type = "pred", terms = c("ItemDifficulty.s", "Overlap.s")) #same in cogadv
ggpredict(L2.1, "L2BackgroundEnvironment.s") %>% plot()

##SELECTED MODEL FOR L2 FREQUENCY-- MODEL 3
plot_model(HBL2.Freq.4)
#no interactions
ggpredict(HBL2.Freq.4, "Freq.s") %>% plot()
