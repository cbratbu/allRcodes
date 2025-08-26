library(nnet)
library(emmeans)
library(dplyr)
wab <- read.csv("wabaq.csv")
neural <- read.csv("neural.csv")

neural$ID <- neural$BU_ID
master <- neural %>%
  left_join(wab %>% select(ID, WAB_AQ), by = "ID")

master$membership_louvain <- as.factor(master$membership_louvain)

## WAB
wab_model <- lm(WAB_AQ ~ membership_louvain, data = master)
# Get estimated marginal means for 
emm <- emmeans(wab_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
pairwise_contrasts_wab <- contrast(emm, method = "pairwise", adjust = "BH")
summary(pairwise_contrasts_wab)

##WHOLEBRAIN
wholebrainfc_model <- lm(wholebrain ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
wholebrainfc_emm <- emmeans(wholebrainfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
wholebrainfc_pairwise_contrasts <- contrast(wholebrainfc_emm, method = "pairwise", adjust = "none")
summary(wholebrainfc_pairwise_contrasts)

controlafc_model <- lm(ControlA_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
controlafc_emm <- emmeans(controlafc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
controlafc_pairwise_contrasts <- contrast(controlafc_emm, method = "pairwise", adjust = "none")
summary(controlafc_pairwise_contrasts)

controlbfc_model <- lm(ControlB_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
controlbfc_emm <- emmeans(controlbfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
controlbfc_pairwise_contrasts <- contrast(controlbfc_emm, method = "pairwise", adjust = "none")
summary(controlbfc_pairwise_contrasts)

controlcfc_model <- lm(ControlC_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
controlcfc_emm <- emmeans(controlcfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
controlcfc_pairwise_contrasts <- contrast(controlcfc_emm, method = "pairwise", adjust = "none")
summary(controlcfc_pairwise_contrasts)

audfc_model <- lm(Aud_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
audfc_emm <- emmeans(audfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
audfc_pairwise_contrasts <- contrast(audfc_emm, method = "pairwise", adjust = "none")
summary(audfc_pairwise_contrasts)

defaultafc_model <- lm(DefaultA_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
defaultafc_emm <- emmeans(defaultafc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
defaultafc_pairwise_contrasts <- contrast(defaultafc_emm, method = "pairwise", adjust = "none")
summary(defaultafc_pairwise_contrasts)

defaultbfc_model <- lm(DefaultB_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
defaultbfc_emm <- emmeans(defaultbfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
defaultbfc_pairwise_contrasts <- contrast(defaultbfc_emm, method = "pairwise", adjust = "none")
summary(defaultbfc_pairwise_contrasts)

defaultcfc_model <- lm(DefaultC_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
defaultcfc_emm <- emmeans(defaultcfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
defaultcfc_pairwise_contrasts <- contrast(defaultcfc_emm, method = "pairwise", adjust = "none")
summary(defaultcfc_pairwise_contrasts)

dorsattnafc_model <- lm(DorsAttnA_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
dorsattnafc_emm <- emmeans(dorsattnafc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
dorsattnafc_pairwise_contrasts <- contrast(dorsattnafc_emm, method = "pairwise", adjust = "none")
summary(dorsattnafc_pairwise_contrasts)

dorsattnbfc_model <- lm(DorsAttnB_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
dorsattnbfc_emm <- emmeans(dorsattnbfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
dorsattnbfc_pairwise_contrasts <- contrast(dorsattnbfc_emm, method = "pairwise", adjust = "none")
summary(dorsattnbfc_pairwise_contrasts)

languagefc_model <- lm(Language_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
languagefc_emm <- emmeans(languagefc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
languagefc_pairwise_contrasts <- contrast(languagefc_emm, method = "pairwise", adjust = "none")
summary(languagefc_pairwise_contrasts)

salvenattnafc_model <- lm(SalVenAttnA_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
salvenattnafc_emm <- emmeans(salvenattnafc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
salvenattnafc_pairwise_contrasts <- contrast(salvenattnafc_emm, method = "pairwise", adjust = "none")
summary(salvenattnafc_pairwise_contrasts)

salvenattnbfc_model <- lm(SalVenAttnB_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
salvenattnbfc_emm <- emmeans(salvenattnbfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
salvenattnbfc_pairwise_contrasts <- contrast(salvenattnbfc_emm, method = "pairwise", adjust = "none")
summary(salvenattnbfc_pairwise_contrasts)

sommotafc_model <- lm(SomMotA_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
sommotafc_emm <- emmeans(sommotafc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
sommotafc_pairwise_contrasts <- contrast(sommotafc_emm, method = "pairwise", adjust = "none")
summary(sommotafc_pairwise_contrasts)

sommotbfc_model <- lm(SomMotB_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
sommotbfc_emm <- emmeans(sommotbfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
sommotbfc_pairwise_contrasts <- contrast(sommotbfc_emm, method = "pairwise", adjust = "none")
summary(sommotbfc_pairwise_contrasts)

visualafc_model <- lm(VisualA_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
visualafc_emm <- emmeans(visualafc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
visualafc_pairwise_contrasts <- contrast(visualafc_emm, method = "pairwise", adjust = "none")
summary(visualafc_pairwise_contrasts)

visualbfc_model <- lm(VisualB_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
visualbfc_emm <- emmeans(visualbfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
visualbfc_pairwise_contrasts <- contrast(visualbfc_emm, method = "pairwise", adjust = "none")
summary(visualbfc_pairwise_contrasts)

visualcfc_model <- lm(VisualC_fc ~ membership_louvain + volume_mm3, data = master)
# Get estimated marginal means for 
visualcfc_emm <- emmeans(visualcfc_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
visualcfc_pairwise_contrasts <- contrast(visualcfc_emm, method = "pairwise", adjust = "none")
summary(visualcfc_pairwise_contrasts)

###
##WHOLEBRAIN
wholebrainvol_model <- lm(volume_mm3 ~ membership_louvain, data = master)
# Get estimated marginal means for 
wholebrainvol_emm <- emmeans(wholebrainvol_model, ~ membership_louvain)  # Note: ~ membership_louvain, not wholebrain
wholebrainvol_pairwise_contrasts <- contrast(wholebrainvol_emm, method = "pairwise", adjust = "BH")
summary(wholebrainvol_pairwise_contrasts)


pvals_controla <- summary(controlafc_pairwise_contrasts)$p.value
pvals_controlb <- summary(controlbfc_pairwise_contrasts)$p.value
pvals_controlc <- summary(controlcfc_pairwise_contrasts)$p.value
pvals_aud <- summary(audfc_pairwise_contrasts)$p.value
pvals_defaulta <- summary(defaultafc_pairwise_contrasts)$p.value
pvals_defaultb <- summary(defaultbfc_pairwise_contrasts)$p.value
pvals_defaultc <- summary(defaultcfc_pairwise_contrasts)$p.value
pvals_dorsattna <- summary(dorsattnafc_pairwise_contrasts)$p.value
pvals_dorsattnb <- summary(dorsattnbfc_pairwise_contrasts)$p.value
pvals_language <- summary(languagefc_pairwise_contrasts)$p.value
pvals_salvenattna <- summary(salvenattnafc_pairwise_contrasts)$p.value
pvals_salvenattnb <- summary(salvenattnbfc_pairwise_contrasts)$p.value
pvals_sommota <- summary(sommotafc_pairwise_contrasts)$p.value
pvals_sommotb <- summary(sommotbfc_pairwise_contrasts)$p.value
pvals_visuala <- summary(visualafc_pairwise_contrasts)$p.value
pvals_visualb <- summary(visualbfc_pairwise_contrasts)$p.value
pvals_visualc <- summary(visualcfc_pairwise_contrasts)$p.value
pvals_wholebrain <- summary(wholebrainfc_pairwise_contrasts)$p.value

all_pvals <- c(pvals_controla,pvals_controlb,pvals_controlc,pvals_aud,
               pvals_defaulta,pvals_defaultb,pvals_defaultc,pvals_dorsattna,
               pvals_dorsattnb,pvals_language,pvals_salvenattna,
               pvals_salvenattnb,pvals_sommota,pvals_sommotb,
               pvals_visuala, pvals_visualb, pvals_visualc,pvals_wholebrain)

pvals_fdr <- p.adjust(all_pvals, method = "fdr")
get_tagged_summary <- function(obj, model_name) {
  s <- summary(obj)
  if (nrow(s) > 0) {
    s$model <- model_name
    return(s)
  } else {
    return(NULL)
  }
}

summ_controlafc <- get_tagged_summary(controlafc_pairwise_contrasts, "ControlA_fc")
summ_controlbfc <- get_tagged_summary(controlbfc_pairwise_contrasts, "ControlB_fc")
summ_controlcfc <- get_tagged_summary(controlcfc_pairwise_contrasts, "ControlC_fc")
summ_audfc <- get_tagged_summary(audfc_pairwise_contrasts, "Aud_fc")
summ_defaultafc <- get_tagged_summary(defaultafc_pairwise_contrasts, "DefaultA_fc")
summ_defaultbfc <- get_tagged_summary(defaultbfc_pairwise_contrasts, "DefaultB_fc")
summ_defaultcfc <- get_tagged_summary(defaultcfc_pairwise_contrasts, "DefaultC_fc")
summ_dorsattnafc <- get_tagged_summary(dorsattnafc_pairwise_contrasts, "DorsAttnA_fc")
summ_dorsattnbfc <- get_tagged_summary(dorsattnbfc_pairwise_contrasts, "DorsAttnlB_fc")
summ_languagefc <- get_tagged_summary(languagefc_pairwise_contrasts, "Language_fc")
summ_salvenattnafc <- get_tagged_summary(salvenattnafc_pairwise_contrasts, "SalVenAttnA_fc")
summ_salvenattnbfc <- get_tagged_summary(salvenattnbfc_pairwise_contrasts, "SalVenAttnB_fc")
summ_sommotafc <- get_tagged_summary(sommotafc_pairwise_contrasts, "SomMotA_fc")
summ_sommotbfc <- get_tagged_summary(sommotbfc_pairwise_contrasts, "SomMotB_fc")
summ_visualafc <- get_tagged_summary(visualafc_pairwise_contrasts, "VisualA_fc")
summ_visualbfc <- get_tagged_summary(visualbfc_pairwise_contrasts, "VisualB_fc")
summ_visualcfc <- get_tagged_summary(visualcfc_pairwise_contrasts, "VisualC_fc")
summ_wholebrainfc <- get_tagged_summary(wholebrainfc_pairwise_contrasts, "Wholebrain_fc")

all_results <- do.call(rbind, list(summ_controlafc,summ_controlbfc,summ_controlcfc,
                                   summ_audfc,summ_defaultafc,summ_defaultbfc,
                                   summ_defaultcfc,summ_dorsattnafc,summ_dorsattnbfc,
                                   summ_languagefc,summ_salvenattnafc,summ_salvenattnbfc,
                                   summ_sommotafc,summ_sommotbfc,summ_visualafc,
                                   summ_visualbfc,summ_visualcfc,summ_wholebrainfc))

# Attach corrected p-values
if (length(all_pvals) == nrow(all_results)) {
  all_results$adj.p.value <- pvals_fdr
} else {
  warning("Adjusted p-values do not match number of rows.")
}

#####
library(ggplot2)

ggplot(master, aes(x = factor(membership_louvain), y = wholebrain)) +
  geom_boxplot() +
  labs(x = "Cluster", y = "Whole Brain Functional Connectivity",
       title = "Whole Brain Functional Connectivity by Cluster") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 8),plot.title = element_text(size = 12)) +
  coord_cartesian(ylim = c(-0.2, 1))


write.csv(master,"master_vol_aq_fc.csv")
