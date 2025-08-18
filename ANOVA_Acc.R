library(car)
library(rstatix)
library(ggpubr)
library(CGPfunctions)
library(DescTools)
library(emmeans)
Cogs <- data.frame(Cogs)

ggboxplot(
  Cogs, x = "Group", y = "Accuracy", 
  color = "CognateStatus", palette = "jco", facet.by = "L1.L2"
)

CogAov <- lm(Accuracy ~ Group * L1.L2 * CognateStatus, data = Cogs)
Anova(CogAov)
lsmeans(CogAov, pairwise ~ CognateStatus)[[2]]
lsmeans(CogAov, pairwise ~ Group)[[2]]
lsmeans(CogAov, pairwise ~ L1.L2)[[2]]

lsmip(CogAov, CognateStatus ~ Group)
lsmeans(CogAov, pairwise ~ CognateStatus|Group)[[2]]

####
#found significant interaction of group x cognate status. is there a difference
#between cognate status within the groups themselves? if so, HB, BPWA, or both?








#####
pwc %>% filter(gender == "male", risk == "high")
Cogs1 <- Cogs %>% add_xy_position(x = "Group")
pwc.filtered <- pwc %>% filter(gender == "male", risk == "high")
bxp +
  stat_pvalue_manual(
    pwc.filtered, color = "risk", linetype = "risk", hide.ns = TRUE,
    tip.length = 0, step.increase = 0.1, step.group.by = "gender"
  ) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )