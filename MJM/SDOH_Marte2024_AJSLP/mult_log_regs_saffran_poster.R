summary(svyglm(LANY_Bin~ SexMale + Education  + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
                  subset = StrokeNWH == 0))

summary(svyglm(LANY_Bin~ SexMale + Education  + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
                   subset = StrokeHisp == 1))
## POVERTY
new_pov_lany_nhw <- svyglm(LANY_Bin~ SexMale  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0)


new_pov_lany_hisp <- svyglm(LANY_Bin~ SexMale  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1)

##WEB USE

new_web_lany_nhw <- svyglm(LANY_Bin~ SexMale  + WebUse + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0)


new_web_lany_hisp <- svyglm(LANY_Bin~ SexMale + Education  + WebUse + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1)
summary(com_diff_pov_hisp)

com_diff_pov_nwh <- svyglm(ComDiff~ SexMale  + Cit + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
                  subset = StrokeNWH == 0)


com_diff_pov_hisp <- svyglm(ComDiff~ SexMale + Education  + Cit + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
                   subset = StrokeHisp == 1)

##SES COMDIF
com_diff_pov_nwh <- svyglm(ComDiff~ SexMale  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0)


com_diff_pov_hisp <- svyglm(ComDiff~ SexMale  + PovertyAdj + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1)

webuse_comdif_nwh <- svyglm(ComDiff~ SexMale  + WebUse + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeNWH == 0)

summary(webuse_comdif_nwh)


webuse_hisp <- svyglm(ComDiff~ SexMale  + WebUse + AGE + South + Married + FAMSIZE + BMICALC, family = quasibinomial, design = svy2,
               subset = StrokeHisp == 1)

summary(webuse_hisp)

#######

lany_subsets <- plot_summs(new_pov_lany_hisp, new_pov_lany_nhw, 
                           model.names = c("Hispanic", "NHW"),
                           coefs = c("Intercept [Poverty >= 400%]" = "(Intercept)", "Poverty <100%" = "PovertyAdj<100%", "Poverty >=100% & <200%" = "PovertyAdj>=100% & <200%", "Poverty >=200% & 400%" = "PovertyAdj>=200% & <400%"),
                           scale = TRUE, colors = c("#CC0000", "#2D2926")) +  ggtitle("Socioeconomic Status and Activity Limitations") +
  theme(plot.title = element_text(hjust = 0.5))
lany_subsets
lany_webuse <- plot_summs(new_web_lany_hisp, new_web_lany_nhw, 
                           model.names = c("Hispanic", "NHW"),
                          coefs = c("Intercept" = "(Intercept)", "Web Use [Yes]" = "WebUse"),
                          scale = TRUE, colors = c("#CC0000", "#2D2926")) +  ggtitle("Web Use and Activity Limitations") +
  theme(plot.title = element_text(hjust = 0.5))

ses_comdiff2 <- plot_summs(com_diff_pov_hisp, com_diff_pov_nwh, 
           model.names = c("Hispanic", "NHW"),
           coefs = c("Intercept [Poverty >= 400%]" = "(Intercept)","Poverty <100%" = "PovertyAdj<100%", "Poverty >=100% & <200%" = "PovertyAdj>=100% & <200%", "Poverty >=200% & 400%" = "PovertyAdj>=200% & <400%"),
           scale = TRUE, colors = c("#CC0000", "#2D2926")) +  ggtitle("Socioeconomic Status and Communication Difficulties") +
  theme(plot.title = element_text(hjust = 0.5))

webuse_comdiff2 <- plot_summs(webuse_hisp, webuse_comdif_nwh, 
                          model.names = c("Hispanic", "NHW"),
                          coefs = c("Intercept" = "(Intercept)","Web Use [Yes]" = "WebUse"),
                          scale = TRUE, colors = c("#CC0000", "#2D2926")) +  ggtitle("Web Use and Communication Difficulties") +
  theme(plot.title = element_text(hjust = 0.5))

webuse_comdiff2
