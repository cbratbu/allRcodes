library(tidyverse)
library(lmerTest)
library(lme4)

data=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/Alldata_Tx_v_NHx_use_ANOVA.csv")
data_long<-gather(data,Test,Accuracy,TR_Pre:PALPA51_Post, factor_key=TRUE)
data_use<-separate(data_long, col = "Test", into=c("Test", "Timepoint"), sep = -5)
data_use$Test=as.factor(data_use$Test)
data_use$Timepoint=as.factor(data_use$Timepoint)
levels(data_use$Timepoint)
levels(data_use$Group)

write.csv(data_use,"Ancova.csv") 
getwd()

m1=lmer(Accuracy~Timepoint*Test*Group+(1|ID)+(1|Group), data=data_use)
summary(m1)
anova(m1)


data=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/TRUT.csv")
data_long<-gather(data,Measure, Accuracy,TR_Pre:UT_Post, factor_key=TRUE) 
data_use<-separate(data_long, col = "Measure", into=c("Test", "Timepoint"), sep ="_")
data_use$Test=as.factor(data_use$Test)
data_use$Timepoint=as.factor(data_use$Timepoint)
levels(data_use$Timepoint)
data_use$Timepoint <- relevel(data_use$Timepoint, ref="Pre")

levels(data_use$Group)

#trained and untrained 
M1=lmer(Accuracy~Timepoint*Group+(1|ID)+(1|Group), data=data_use)
summary(M1)
anova(M1)

#nonlinear 
nM1<-nlmer(Accuracy~Timepoint*Group+(1|ID)+(1|Group))

#standardized assessment 
data=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/ST.csv")
data_long<-gather(data,Measure, Accuracy,LQ_Pre:PALPA51_Post, factor_key=TRUE) 
data_use<-separate(data_long, col = "Measure", into=c("Test", "Timepoint"), sep ="_")
data_use$Test=as.factor(data_use$Test)
data_use$Timepoint=as.factor(data_use$Timepoint)
levels(data_use$Timepoint)
data_use$Timepoint <- relevel(data_use$Timepoint, ref="Pre")

M2=lmer(Accuracy~Timepoint*Group+(1|ID), data=data_use)
summary(M2)
anova(M2)

#BH tasks - accuracy 
data=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/ACC.csv")
data_long<-gather(data,Measure, Accuracy,Acc_RJNN_Pre:Acc_SFV_Post, factor_key=TRUE) 
data_use<-separate(data_long, col = "Measure", into=c("Test", "Timepoint"), sep = -5)
data_use$Test=as.factor(data_use$Test)
data_use$Timepoint=as.factor(data_use$Timepoint)
levels(data_use$Timepoint)
data_use$Timepoint <- relevel(data_use$Timepoint, ref="_Pre")


M3=lmer(Accuracy~Timepoint*Group+(1|ID)+(1|Group), data=data_use)
summary(M3)
anova(M3)

#BH tasks - reaction time 

data=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/RT.csv")
data_long<-gather(data,Measure, Accuracy, RT_RJNN_Pre:RT_SFV_Post, factor_key=TRUE) 
data_use<-separate(data_long, col = "Measure", into=c("Test", "Timepoint"), sep = -5)
data_use$Test=as.factor(data_use$Test)
data_use$Timepoint=as.factor(data_use$Timepoint)
levels(data_use$Timepoint)
data_use$Timepoint <- relevel(data_use$Timepoint, ref="_Pre")

M4=lmer(Accuracy~Timepoint*Group+(1|ID), data=data_use)
summary(M4)
anova(M4)

#checking normality of the data 

 
data=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/Alldata_Tx_v_NHx_use_ANOVA.csv")
shapiro.test(data$TR_Pre)
shapiro.test(data$TR_Post)
shapiro.test((data$UT_Pre))
shapiro.test((data$UT_Post))
shapiro.test((data$Acc_RJNN_Pre))
shapiro.test((data$Acc_RJNP_Pre))
shapiro.test((data$Acc_SJNN_Pre))
shapiro.test((data$Acc_SJNP_Pre))
shapiro.test((data$Acc_PHNN_Pre))
shapiro.test((data$Acc_PHNP_Pre))
shapiro.test((data$Acc_CCJ_Pre))
shapiro.test((data$Acc_SCV_Pre))
shapiro.test((data$Acc_SFV_Pre))
shapiro.test((data$Acc_RJNN_Post))
shapiro.test((data$Acc_RJNP_Post))
shapiro.test((data$Acc_SJNN_Post))
shapiro.test((data$Acc_SJNP_Post))
shapiro.test((data$Acc_PHNN_Post))
shapiro.test((data$Acc_PHNP_Post))
shapiro.test((data$Acc_CCJ_Post))
shapiro.test((data$Acc_SCV_Post))
shapiro.test((data$Acc_SFV_Post))
shapiro.test((data$RT_RJNN_Pre))
shapiro.test((data$RT_RJNP_Pre))
shapiro.test((data$RT_SJNN_Pre))
shapiro.test((data$RT_SJNP_Pre))
shapiro.test((data$RT_PVNN_Pre))
shapiro.test((data$RT_PVNP_Pre))
shapiro.test((data$RT_CCJ_Pre))
shapiro.test((data$RT_SCV_Pre))
shapiro.test((data$RT_SFV_Pre))
shapiro.test((data$RT_RJNN_Post))
shapiro.test((data$RT_RJNP_Post))
shapiro.test((data$RT_SJNN_Post))
shapiro.test((data$RT_SJNP_Post))
shapiro.test((data$RT_PVNN_Post))
shapiro.test((data$RT_PVNP_Post))
shapiro.test((data$RT_CCJ_Post))
shapiro.test((data$RT_SCV_Post))
shapiro.test((data$RT_SFV_Post))
shapiro.test((data$LQ_Pre))
shapiro.test((data$CQ_Pre))
shapiro.test((data$AQ_Pre))
shapiro.test((data$CLQT_Pre))
shapiro.test((data$BNT_Pre))
shapiro.test((data$PAPT_Pre))
shapiro.test((data$NNB_Pre))
shapiro.test((data$PALPA1_Pre))
shapiro.test((data$PALPA51_Pre))
shapiro.test((data$LQ_Post))
shapiro.test((data$CQ_Post))
shapiro.test((data$AQ_Post))
shapiro.test((data$CLQT_Post))
shapiro.test((data$BNT_Post))
shapiro.test((data$PAPT_Post))
shapiro.test((data$NNB_Post))
shapiro.test((data$PALPA1_Post))
shapiro.test((data$PALPA51_Post))



#How do I fit this as a non-linear model? 
library(coin)
d=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/RT.csv")
d$Group=as.factor(d$Group)

RT_change = 
  d %>% 
  select("Group", "RT_RJNN_Change", "RT_RJNP_Change", "RT_SJNN_Change", "RT_SJNP_Change", "RT_PVNN_Change", "RT_PVNP_Change", "RT_CCJ_Change", "RT_SCV_Change", "RT_SFV_Change")

https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/summarise

RT_Change=
group_by("Group")
summarise (mean = mean"RT_RJNN_Change")
  
d=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/RT_TxRvNHx.csv")
d$Group=as.factor(d$Group) 
wilcox.test(d$RT_RJNN_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_RJNP_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_SJNN_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_SJNP_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_PVNN_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_PVNP_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_CCJ_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_SCV_Change ~ Group, paired=FALSE, data=d) 

wilcox.test(d$RT_SFV_Change ~ Group, paired=FALSE, data=d) 


d1=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/ACC_TxRvNHx.csv")
d1$Group=as.factor(d1$Group)
wilcox.test(d1$Acc_RJNN_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_RJNP_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_SJNN_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_SJNP_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_PHNN_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_PHNP_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_CCJ_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_SCV_Change ~ Group, paired=FALSE, data=d1) 
wilcox.test(d1$Acc_SFV_Change ~ Group, paired=FALSE, data=d1) 

d2=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/ST_TxRvNHx.csv")
wilcox.test(d2$LQ_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$CQ_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$AQ_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$CLQT_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$BNT_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$PAPT_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$NNB_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$PALPA1_Change ~ Group,paired=FALSE,data=d2)
wilcox.test(d2$PALPA51_Change ~ Group,paired=FALSE,data=d2)

#ggplot patients versus controls on change values for all of these tests

d3=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/TRUT.csv")
library(coin)
wilcox.test(d3$TR_Change ~ Group,paired=FALSE,data=d3)
wilcox.test(d3$UT_Change ~ Group,paired=FALSE, data=d3)
