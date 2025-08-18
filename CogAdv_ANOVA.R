library(multcomp)
library(lsmeans)

CogAdv_df <- data.frame(CogAdv_Anova[1:116,])

ggboxplot(
  CogAdv_df, x = "Group", y = "CogAdv", 
  color = "Group", palette = "jco"
)

CogAdvOvr <- lm(CogAdv ~ Group * L1.L2, data = CogAdv_df)
CogAdvAov <- Anova(lm(CogAdv ~ Group * L1.L2, data = CogAdv_df))
CogAdvAov
lsmip(CogAdvOvr,  ~ Group)
lsmeans(CogAdvOvr, pairwise ~ Group)[[2]]
