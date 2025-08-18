library(lme4)
library(lmerTest)
library(tidyverse)
library(ggeffects)
install.packages("extrafont")
library(extrafont)
Typicalitydata=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/Use for Mixed Models_Item Level/05.23.18/TUM_use.csv") 
# Typicalitydata$item_number=as.factor(Typicalitydata$item_number)
# Typicalitydata$score=as.factor(Typicalitydata$score)
# Typicalitydata$ID = as.factor(Typicalitydata$ID)
# Typicalitydata$Session=as.numeric(Typicalitydata$Session)

Typicalitydata$TypTra=relevel(Typicalitydata$TypTra,"Monitored")
contrasts(Typicalitydata$TypTra)

#centering session
Typicalitydata_centered=Typicalitydata %>% 
  mutate(Sessionc=scale(Session, scale=FALSE))
Typicalitydata_centered$Sessionc<-as.vector(Typicalitydata_centered$Sessionc)
#https://www.theanalysisfactor.com/center-on-the-mean/

#all
#Treatment
M0=glmer(score ~ Session*TypTra+(1|ID)+(1|item),data=Typicalitydata, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))

#did not converge
#M0=glmer(score ~ Sessionc*TypTra+(1|ID)+(1|item),data=Typicalitydata_centered, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))

#M0_noitem=glmer(score ~ Session*TypTra+(1|ID),data=Typicalitydata, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))
#M0_norandom=glm(score ~ Session*TypTra,data=Typicalitydata, family=binomial(link=logit))
#summary(M0_norandom)

#Generalization
Typicalitydata2= Typicalitydata[(Typicalitydata$TypTra == "Atypical_Untrained" | Typicalitydata$TypTra == "Typical_Untrained"),]
M01=glmer(score ~ Session*TypTra+(1|ID)+(1|item),data=Typicalitydata2, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))
summary(M01)

#plot
#Tx
M0data <- ggpredict(M0, terms = c("Session", "TypTra"), ci.lvl= NA)

plot(M0data,colors = "bw")

use<-ggplot(M0data %>% filter(group != 'Atypical_Untrained' & group != 'Typical_Untrained'))+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Monitored","Atypical_Trained","Typical_Trained"),labels=c("Monitored","Atypical Trained","Typical Trained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+
  ylim(0,1.0)+
  xlim(0,15)+
  theme_classic()+theme(legend.position = "none")

all<-ggplot(M0data)+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Monitored","Atypical_Trained","Typical_Trained","Typical_Untrained","Atypical_Untrained"),labels=c("Monitored","Atypical Trained","Typical Trained", "Typical Untrained", "Atypical Untrained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+
  ylim(0,1.0)+
  xlim(0,15)+
  theme_classic()+theme(legend.position = "none")

plot(use)
Fig1a=use+theme(axis.title=element_text(size=12, colour="black"), 
                axis.text=element_text(size = 12, colour = "black"))
                
 

#Gen
M01data <- ggpredict(M01, terms = c("Session", "TypTra"), ci.lvl= NA)
plot(M01data)

use2=ggplot(M01data)+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Atypical_Untrained","Typical_Untrained"),labels=c("Atypical Untrained","Typical Untrained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+xlim(0,15)+ylim(0,1.0)+
  theme_classic()+theme(legend.position = "none")
plot(use2)
Fig2a=use2+theme(axis.title=element_text(size=12, colour="black"), 
                axis.text=element_text(size = 12, colour = "black"))

# Responders

#Typicalitydata_R=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/01.06.17_Revision Analyses/Use for Mixed Models_Item Level/Typicality data_R.csv") 

#TUM_use=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/05.12.18_Revision 2 Analyses/Use for Mixed Models_Item Level/05.23.18/TUM_use.csv") 

Typicalitydata_R=Typicalitydata %>% filter(Treatment.Response %in% c("R"))
Typicalitydata_R$TypTra=as.factor(Typicalitydata_R$TypTra)
Typicalitydata_R$TypTra=relevel(Typicalitydata_R$TypTra,"Monitored")
contrasts(Typicalitydata_R$TypTra)

M02=glmer(score ~ Session*TypTra+(1|ID)+(1|item),data=Typicalitydata_R, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))
summary(M02)

#M02_noitem=glmer(score ~ Session*TypTra+(1|ID),data=Typicalitydata_R, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))
#M02_norandom=glm(score ~ Session*TypTra,data=Typicalitydata_R, family=binomial(link=logit))
#summary(M02_norandom)

Typicalitydata_R2= Typicalitydata_R[(Typicalitydata_R$TypTra == "Atypical_Untrained" | Typicalitydata_R$TypTra == "Typical_Untrained"),]

M03=glmer(score ~ Session*TypTra+(1|ID)+(1|item),data=Typicalitydata_R2, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))

#plot
M02data <- ggpredict(M02, terms = c("Session", "TypTra"), ci.lvl= NA)

use3=ggplot(M02data %>% filter(group != "Atypical_Untrained" & group != "Typical_Untrained"))+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Monitored","Atypical_Trained","Typical_Trained"),labels=c("Monitored","Atypical Trained", "Typical Trained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+xlim(0,15)+ylim(0,1.0)+
  theme_classic()+theme(legend.position="none")

R<-ggplot(M02data)+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Monitored","Atypical_Trained","Typical_Trained", "Typical_Untrained", "Atypical_Untrained"),labels=c("Monitored","Atypical Trained","Typical Trained", "Typical Untrained", "Atypical Untrained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+
  ylim(0,1.0)+
  xlim(0,15)+
  theme_classic()+theme(legend.position="none")
 
Fig1b=use3+theme(axis.title=element_text(size=12, colour="black"), 
                axis.text=element_text(size = 12, colour = "black"))

M03data <- ggpredict(M03, terms = c("Session", "TypTra"), ci.lvl= NA)
plot(M03data)

use4=ggplot(M03data)+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Atypical_Untrained","Typical_Untrained"),labels=c("Atypical Untrained","Typical Untrained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+xlim(0,15)+ylim(0,1.0)+
  theme_classic()+theme(legend.position="none")

Fig2b=use4+theme(axis.title=element_text(size=12, colour="black"), 
                 axis.text=element_text(size = 12, colour = "black"))

# Non-responders
#Typicalitydata_NR=read.csv("R:/KiranLab4/P50 Neurobiology of Recovery of Aphasia/Tx, Gen, Transfer Analyses - Natalie/01.06.17_Revision Analyses/Use for Mixed Models_Item Level/Typicalitydata_NR.csv") 

Typicalitydata_NR=Typicalitydata %>% filter(Treatment.Response %in% c("NR"))
Typicalitydata_NR$TypTra=as.factor(Typicalitydata_NR$TypTra)
Typicalitydata_NR$TypTra=relevel(Typicalitydata_NR$TypTra,"Monitored")
contrasts(Typicalitydata_NR$TypTra)

M04=glmer(score ~ Session*TypTra+(1|ID)+(1|item),data=Typicalitydata_NR, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))

Typicalitydata_NR2= Typicalitydata_NR[(Typicalitydata_NR$TypTra == "Atypical_Untrained" | Typicalitydata_NR$TypTra == "Typical_Untrained"),]
M05=glmer(score ~ Session*TypTra+(1|ID)+(1|item),data=Typicalitydata_NR2, family=binomial(link=logit),control=glmerControl(optimizer="bobyqa",optCtrl = list(maxfun=2e5)))
summary(M05)
#plot
M04data <- ggpredict(M04, terms = c("Session", "TypTra"), ci.lvl= NA)
plot(M04data)

use5=ggplot(M04data %>% filter(group != "Atypical_Untrained" & group != "Typical_Untrained"))+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Monitored","Atypical_Trained","Typical_Trained"),labels=c("Monitored","Atypical Trained", "Typical Trained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+xlim(0,15)+ylim(0,1.0)+
  theme_classic()+theme(legend.position="none")

Fig1c=use5+theme(axis.title=element_text(size=12, colour="black"), 
                 axis.text=element_text(size = 12, colour = "black"))

NR<-ggplot(M04data)+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Monitored","Atypical_Trained","Typical_Trained", "Typical_Untrained", "Atypical_Untrained"),labels=c("Monitored","Atypical Trained","Typical Trained", "Typical Untrained", "Atypical Untrained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+
  ylim(0,1.0)+
  xlim(0,15)+
  theme_classic()+theme(legend.position="none")
plot(NR)

M05data <- ggpredict(M05, terms = c("Session", "TypTra"), ci.lvl= NA)
plot(M05data)

use6=ggplot(M05data)+
  geom_line(aes(x=x,y=predicted,linetype=group))+
  scale_linetype_discrete(name=NULL,breaks=c("Atypical_Untrained","Typical_Untrained"),labels=c("Atypical Untrained","Typical Untrained"))+
  ylab("Predicted Accuracy")+
  xlab("Session")+xlim(0,15)+ylim(0,1.0)+
  theme_classic()+theme(legend.position="none")

Fig2c=use6+theme(axis.title=element_text(size=12, colour="black"), 
                 axis.text=element_text(size = 12, colour = "black"))
