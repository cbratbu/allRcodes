library(cowplot)
library(ggeffects)
library(jtools)
library(ordinal)
library(ggeasy)
#first OLS plot
ggpredict_ols <- data.frame(ggpredict(olr_test4, terms = c("Hispanic")))
ggpredict_ols     
ols_clm <- clm(FactorSDOHScore ~ HispanicNHW +SexMale + AGE + 
                 South + Married + FAMSIZE + BMICALC + Smoker,
               link = "logit",data=IPUMS_glm2)
olr_test4
ols_clm
firt_olsplot <- ggplot(ggpredict_ols, aes(x = response.level, y = predicted)) + 
  geom_point(aes(color = x), position =position_dodge(width = 0.5)) + 
  #geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = x), position = position_dodge(width = 0.5), width = 0.3) + 
  theme_minimal()

#second ol ploss -- better
olsplot_original <- as.data.frame(ggpredict(olr_test4, terms = c("HispanicNHW")))
olsplot <- as.data.frame(ggpredict(ols_clm, terms = c("HispanicNHW")))
olsplot
olsplot$Group <- olsplot$x
olsplot$response.level <- (olsplot$response.level) - 1
olsplot2 <- ggplot(olsplot, aes(x= Group, y = predicted, color = Group)) + geom_point(aes(color = Group)) + 
  facet_wrap(~response.level) + ggtitle("SDOH Score") +
  theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12), 
        axis.title = element_text(size = 12), legend.position="bottom",
        axis.text.y = element_text(hjust = 1.5),
        plot.title = element_text(hjust = .5),
        strip.text.x = element_text(
          size = 12, face = "bold"
        )) +
  ylab('Predicted probability of SDOH score') + xlab('') +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = Group),
                    position = position_dodge(width = 0.5), width = 0.1) +
  scale_colour_manual(values = c("non-Hispanic white"="#2D2926", "Hispanic" = "#CC0000")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy=1))
#needs manual change to dot color
olsplot2
###SDOH score and GLM plot
comdif_sdohscore_plot <- plot(ggpredict(glm_test, terms = c("FactorSDOHScore")), colors = "#f00000")
comdif_sdohscore_plot2 <- comdif_sdohscore_plot +
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12)) +
  ylab('Predicted probability of communication difficulties') + xlab('SDOH Score')
comdif_sdohscore_plot2
#etc
cowplot::plot_grid(
  firt_olsplot,
  comdif_sdohscore_plot2,
  nrow = 1,
  rel_widths = c(1.5,1),
  labels = c('A', 'B'))
summary(glm_test)
ggsave('comdiff_SDOHscore.tiff', 
       dpi = 700)
comdif_sdohscore_plot2 + ggtitle('Communication difficulties and SDOH score') +
  theme(plot.title = element_text(hjust = 0.5))





###all plots
#educational attainment
comdif_edu <- plot_summs(edu_nhw, edu_h, 
                           model.names = c("non-Hispanic white", "Hispanic"),
                         exp = TRUE,
                           coefs = c("Intercept" = "(Intercept)", "Educational attainment \n ≥ HS degree/GED" = "Education"),
                           scale = TRUE, colors = c("#2D2926","#CC0000")) +  ggtitle("Educational attainment") +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12)) + ylab('') + xlab('Odds ratio')
comdif_edu

#poverty level
comdif_povertylvl <- plot_summs(povlvl_nhw, povlvl_h, 
                         model.names = c("non-Hispanic white", "Hispanic"),
                         exp = TRUE,
                         coefs = c("Intercept" = "(Intercept)", "Poverty threshold \n Below" = "PovertyLevel"),
                         scale = TRUE, colors = c("#2D2926","#CC0000")) +  ggtitle("Poverty level") +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12)) + ylab('') + xlab('Odds ratio')
comdif_povertylvl

#cit plotss
comdif_cit <- plot_summs(cit_nhw, cit_h, 
                                model.names = c("non-Hispanic white", "Hispanic"),
                                coefs = c("Intercept" = "(Intercept)", "Citizenship status \n Citizen" = "Cit"),
                                scale = TRUE, colors = c("#2D2926","#CC0000")) +  ggtitle("Citizenship status") +
  theme(plot.title = element_text(hjust = 0.5))+ theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12)) + ylab('') + xlab('Odds ratio')
comdif_cit

#delay in care
comdif_del <- plot_summs(del_nhw, del_h, 
                         model.names = c("non-Hispanic white", "Hispanic"),
                         exp = TRUE,
                         coefs = c("Intercept" = "(Intercept)", "Delays in care \n Experienced delays" = "DelayInCare"),
                         scale = TRUE, colors = c("#2D2926","#CC0000")) +  ggtitle("Delays in care") +
  theme(plot.title = element_text(hjust = 0.5)) + theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12)) + ylab('')  + xlab('Odds ratio')
comdif_del

#web use
comdif_web <- plot_summs(web_nhw, web_h, 
                         model.names = c("non-Hispanic white", "Hispanic"),
                         exp = TRUE,
                         coefs = c("Intercept" = "(Intercept)", "Web use \n Accesses web" = "WebUse"),
                         scale = TRUE, colors = c("#2D2926","#CC0000")) +  ggtitle("Web use") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(base_family = 'Microsoft Sans Serif') +
  theme(axis.text = element_text(size = 12)) + theme(axis.title = element_text(size = 12)) + ylab('') + xlab('Odds ratio')
comdif_web

sdoh_legend <- get_legend(comdif_edu)
cowplot::plot_grid(
  comdif_edu +  theme(legend.position="none"),
  comdif_povertylvl +  theme(legend.position="none"),
  comdif_cit +  theme(legend.position="none"),
  comdif_del + theme(legend.position = "none"),
  comdif_web +  theme(legend.position="none"),
  sdoh_legend,
  nrow = 2,
  labels = c('A', 'B', 'C', 'D', 'E', ''))

ggsave('five_sdoh_by_ethnicity.tiff', dpi = 700)
