library(tidyverse)
library(lme4)
library(broom)
library(ggpubr)
library(patchwork)
library(effects)
library(emmeans)
library(sjPlot)
library(Hmisc)
library(boot)

procomdata <- read_csv('procomdata_n34.csv',na=c('','NA'))
summary(procomdata)

#factorize variables
procomdata$patient <-factor(procomdata$patient) 
procomdata$item <-factor(procomdata$item) 
procomdata$category <- factor(procomdata$category)
procomdata$set <-factor(procomdata$set,levels=c('3','1','2')) 
procomdata$cond <-factor(procomdata$cond) 
procomdata$lang<-factor(procomdata$lang)
procomdata$l1lang <- factor(procomdata$l1lang)
procomdata$l2lang <- factor(procomdata$l2lang)
procomdata$matchset <- factor(procomdata$matchset)
procomdata$animacy <- factor(procomdata$animacy)
procomdata$sex <- factor(procomdata$sex)

unique(procomdata$item)

#filter out set 2 items since they are not used in ANY of the models
procomdata <- filter(procomdata,set!='2') %>% 
  droplevels()
summary(procomdata)

##Aim 1: Do Tx-Level and individual-specific factors predict naming outcomes?##
#a) in the treated language?

#treated language data, set 1 and set 3 only
treated.overall <- filter(procomdata,cond=='treated')

#make sum-coded set variable and set contrasts
treated.overall$set.sum <- factor(treated.overall$set,levels=c('3','1'))
contrasts(treated.overall$set.sum)<-contr.sum(2)
contrasts(treated.overall$set.sum) <- c(-1,1)
levels(treated.overall$set.sum)
contrasts(treated.overall$set.sum)
summary(treated.overall$set.sum)

#make sum-coded matchset variable and set contrasts
treated.overall$matchset.sum <- factor(treated.overall$matchset,levels=c('0','1'))
contrasts(treated.overall$matchset.sum)<-contr.sum(2)
contrasts(treated.overall$matchset.sum) <- c(-1,1)
levels(treated.overall$matchset.sum)
contrasts(treated.overall$matchset.sum)
summary(treated.overall$matchset.sum)

#Treatment effect model (treatment-level factors only)
modtreated.overall <- glmer(score~session*set.sum*matchset.sum+(session|patient)+(1|item),data=treated.overall,family=binomial(link=logit),
                            control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.overall)

#predictions using only set 1 and set 3 and lang
pred_treated_overall <- augment_columns(modtreated.overall, treated.overall,
                                              type.predict = c("response")) %>%
  rename(pred_prob = .fitted) %>% 
  # calculate overall mean
  group_by(patient, session, set.sum, matchset.sum) %>% 
  summarise(Acc = mean(score), 
            Acc_pred = mean(pred_prob))
summary(pred_treated_overall)

matchset.names <- as_labeller(
  c(`0` = "L2", `1` = 'L1'))

treated.overall$matchset.sum <- factor(untreated.overall$matchset.sum,levels=c('1','2'),
                                         labels=c('L1','L2'))

#figure 1
plottreated.overall <- ggplot(pred_treated_overall, aes(session, Acc, colour = set.sum)) +
  facet_grid(~factor(matchset.sum,levels=c('1','0')),
             labeller=matchset.names)+
  stat_smooth(aes(y=Acc), method="loess") +
  scale_colour_brewer(name='Word Set',limits=c('1','3'),
                      labels=c('Trained','Control'),palette = 'Set2')+
  expand_limits(y=c(0,1))+
  geom_vline(xintercept=c(2,12),linetype='dashed')+
  #ggtitle('Naming Accuracy in the Treated Language')+
  xlab('Session (Probe #)')+
  ylab('Proportion of Correct Words')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5,size=16),
        legend.title=element_text(hjust=.5,size=14),
        strip.text.x=element_text(size=14,color='black'),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))
plottreated.overall

ggsave(filename='Treatment Effect Plot.tiff',plot=plottreated.overall,device='tiff',
       height=4.5,width=6,units='in',dpi=500)

#For individual-specific model in the treated language
treated.set1 <- filter(treated.overall,set=='1') %>% 
  droplevels()

unique(treated.set1$item)

#add bnt z-score

treated.set1$bnt.z <- scale(treated.set1$BNT,center=T,scale=T)

#add age centered
treated.set1$age.c <- scale(treated.set1$age,center=T,scale=T)
treated.set1$mpo.c <- scale(treated.set1$mpo,center=T,scale=T)
treated.set1$edu.c <- scale(treated.set1$edu,center=T,scale=T)

modtreated.individual2 <- glmer(score~session+age.c+edu.c+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                               control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.individual2)

modtreated.individual3 <- glmer(score~session*mpo.c+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                                control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.individual3)

modtreated.individual4 <- glmer(score~session*edu.c+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                                control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.individual4)

modtreated.individual <- glmer(score~session*bnt.z*matchset.sum+age.c+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                                control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.individual)

hist(treated.overall$bnt.z) #use to help visualize values for the effects plot below

#Figure 2
tiff(file = "Treatment Lang BNT Effect.tiff", width = 6.5, height = 4, units='in',res=500)
plottreated.bnt <- plot(predictorEffects(modtreated.individual, ~ session,xlevels=list(bnt.z=c(-1,-.5,0,.5,1))),
                     axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
                               x=list(rug=FALSE)),ylim=c(0,1),
                     lines=list(multiline=TRUE,z.var="matchset.sum"),
                     lattice=list(layout=c(5,1),key.args=list(space='right',border=FALSE,title='Tx Language')),
                     main=NULL,
                     xlab='Session',
                     xaxt = "n")
plottreated.bnt
dev.off()

#b) in the untreated language?

#filter for untreated data only
untreated.overall <- filter(procomdata,cond=='untreated')
untreated.overall$set.sum <- factor(untreated.overall$set,levels=c('3','1'))
contrasts(untreated.overall$set.sum)<-contr.sum(2)
contrasts(untreated.overall$set.sum) <- c(-1,1)
levels(untreated.overall$set.sum)
contrasts(untreated.overall$set.sum)
summary(untreated.overall$set.sum)

untreated.overall$matchset.sum <- factor(untreated.overall$matchset,levels=c('0','1'))
contrasts(untreated.overall$matchset.sum)<-contr.sum(2)
contrasts(untreated.overall$matchset.sum) <- c(-1,1)
levels(untreated.overall$matchset.sum)
contrasts(untreated.overall$matchset.sum)
summary(untreated.overall$matchset.sum)

moduntreated.overall <- glmer(score~session*set.sum*matchset.sum+(session|patient)+(1|item),data=untreated.overall,family=binomial(link=logit),
                            control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(moduntreated.overall)

pred_untreated_overall <- augment_columns(moduntreated.overall, untreated.overall,
                                        type.predict = c("response")) %>%
  rename(pred_prob = .fitted) %>% 
  # calculate overall mean
  group_by(patient,session, set.sum, matchset.sum) %>% 
  summarise(Acc = mean(score), 
            Acc_pred = mean(pred_prob))
summary(pred_untreated_overall)

#figure 3
plotuntreated.overall <- ggplot(pred_untreated_overall, aes(session, Acc, colour = set.sum)) +
  facet_grid(~factor(matchset.sum,levels=c('1','0')),
             labeller=matchset.names)+
  stat_smooth(aes(y=Acc),method='loess')+
  scale_colour_brewer(name='Word Set',limits=c('1','3'),
                      labels=c('Trained','Control'),palette = 'Set2')+
  expand_limits(y=c(0,1))+
  geom_vline(xintercept=c(2,12),linetype='dashed')+
  #ggtitle('Naming Accuracy in the Untreated Language')+
  xlab('Session (Probe #)')+
  ylab('Proportion of Correct Words')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5,size=16),
        legend.title=element_text(hjust=.5,size=14),
        strip.text.x=element_text(size=14,color='black'),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))
plotuntreated.overall

ggsave(filename='Generalization Plot.tiff',plot=plotuntreated.overall,device='tiff',
       height=4.5,width=6,units='in',dpi=500)

untreated.set1 <- filter(untreated.overall,set=='1') %>% 
  droplevels()

unique(untreated.set1$item)

#add bnt z-score

untreated.set1$bnt.z <- scale(untreated.set1$BNT,center=T,scale=T)

#add age centered
untreated.set1$age.c <- scale(untreated.set1$age,center=T,scale=F)

moduntreated.individual <- glmer(score~session*bnt.z*matchset.sum+age.c+(session|patient)+(1|item),data=untreated.set1,family=binomial(link=logit),
                               control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(moduntreated.individual)

moduntreated.individual.2 <- glmer(score~session*bnt.z+age.c+(session|patient)+(1|item),data=untreated.set1,family=binomial(link=logit),
                                 control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(moduntreated.individual.2)

anova(moduntreated.individual,moduntreated.individual.2,method='LRT') #drop `matchset.sum` and use moduntreated.individual

#Figure 4
tiff(file = "Untreated Lang BNT Effect.tiff", width = 6.5, height = 4, units='in',res=500)
plotuntreated.bnt <- plot(predictorEffects(moduntreated.individual.2, ~ session,xlevels=list(bnt.z=c(-1,-.5,0,.5,1))),
                        axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
                                  x=list(rug=FALSE)),ylim=c(0,1),
                        lines=list(multiline=TRUE,z.var="session"),
                        lattice=list(layout=c(5,1)),
                        main=NULL,
                        xlab='Session')
plotuntreated.bnt
dev.off()

tiff(file = "Untreated Lang BNT Effect.tiff", width = 6.5, height = 4, units='in',res=500)
plotuntreated.bnt <- plot(predictorEffects(moduntreated.individual.2, ~ session,xlevels=list(bnt.z=c(-1,-.5,0,.5,1)),label=c('-1 SD', '-0.5 SD','Mean','0.5 SD','1 SD')),
                          axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
                                    x=list(rug=FALSE)),ylim=c(0,1),
                          lines=list(multiline=TRUE,z.var="session"),
                          lattice=list(layout=c(1,1),key.args=list(space='right',border=FALSE,title='BNT Z-Score')),
                          main=NULL,
                          xlab='Session',
                          xaxt = "n")

plotuntreated.bnt
dev.off()

plotuntreated.bnt

#-----------------------------------------------------------------------------#

#patient plots for Appendix C

plottreated.patients <- ggplot(pred_treated_overall, aes(session, Acc, colour = set)) +
  facet_wrap(~patient)+
  stat_smooth(aes(y=Acc_pred), method="loess") +
  scale_colour_brewer(name='Word Set',limits=c('1','3'),
                      labels=c('Trained','Control'),palette = 'Set2')+
  expand_limits(y=c(0,1))+
  ggtitle('Naming Accuracy in the Treated Language')+
  xlab('Session (Probe #)')+
  ylab('Predicted Probability of a Correct Response')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5,size=16),
        legend.title=element_text(hjust=.5,size=14),
        strip.text.x=element_text(size=14,color='black'),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))
plottreated.patients

ggsave(filename='Treated Patient Plots.png',plot=plottreated.patients,device='png',
       height=4.5,width=5,units='in',dpi=500)

plotuntreated.patients <- ggplot(pred_untreated_overall, aes(session, Acc, colour = set.sum)) +
  facet_wrap(~patient)+
  stat_smooth(aes(y=Acc_pred), method="loess") +
  scale_colour_brewer(name='Word Set',limits=c('1','3'),
                      labels=c('Trained','Control'),palette = 'Set2')+
  expand_limits(y=c(0,1))+
  ggtitle('Naming Accuracy in the Untreated Language')+
  xlab('Session (Probe #)')+
  ylab('Predicted Probability of a Correct Response')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5,size=16),
        legend.title=element_text(hjust=.5,size=14),
        strip.text.x=element_text(size=14,color='black'),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14))
plotuntreated.patients

ggsave(filename='Untreated Patient Plots.png',plot=plotuntreated.patients,device='png',
       height=4.5,width=5,units='in',dpi=500)

##Aim 2: Do individual-specific factors predict naming outcomes?##

treated.set1 <- filter(treated.overall,set=='1') #only set 1 items

#transform variables

#Standardize BNT
treated.set1$bnt.z <- scale(treated.set1$BNT,center=T,scale=T)

#Center Age
treated.set1$age.c <- scale(treated.set1$age, center=T, scale=F)

#Standardize Age
treated.set1$age.z <- scale(treated.set1$age,center=T,scale=T)

modtreated.set1 <- glmer(score~session*matchset*bnt.z+age.c+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                            control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.set1)

untreated.set1 <- filter(untreated.overall, set=='1') #only set 1 items

#transform variables

#Standardize BNT
untreated.set1$bnt.z <- scale(untreated.set1$BNT,center=T,scale=T)

#Center Age
untreated.set1$age.c <- scale(untreated.set1$age, center=T, scale=F)

#Standardize Age
untreated.set1$age.z <- scale(untreated.set1$age,center=T,scale=T)

moduntreated.set1 <- glmer(score~session*matchset*bnt.z+age.c+(session|patient)+(1|item),data=untreated.set1,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(moduntreated.set1)

##Aim 3: Do stimulus-level characteristics predict naming tx outcomes?##

stimdata <- read_csv('Probe Acc Data_n34.csv',na=c('','NA'))
stimdata$set <- factor(stimdata$set)
stimdata$cond <- factor(stimdata$cond)
stimdata$lang <- factor(stimdata$lang)
stimdata <- filter(stimdata,set=='trained')
stimdata <- filter(stimdata,cond=='WL')

#Add in transformed variables
stimdata$log.freq <- log10(stimdata$freq)
stimdata$log.density <- log10(stimdata$density+1)

x1 <- mean(stimdata$log.freq,na.rm=T)
y <- sd(stimdata$log.freq,na.rm=T)

x1
x1+y
x1-y
x1+0.5*y
x1-0.5*y

#divide data into Spanish and English sets for descriptive stats by lang
stimdata.eng <- filter(stimdata,lang=='Eng')
stimdata.span <- filter(stimdata,lang=='Span')

#English descriptive statistics for Appendix B
mean(stimdata.eng$log.freq,na.rm=T)
sd(stimdata.eng$log.freq,na.rm=T)
min(stimdata.eng$log.freq,na.rm=T)
max(stimdata.eng$log.freq,na.rm=T)

mean(stimdata.eng$length,na.rm=T)
sd(stimdata.eng$length,na.rm=T)
min(stimdata.eng$length,na.rm=T)
max(stimdata.eng$length,na.rm=T)

mean(stimdata.eng$log.density,na.rm=T)
sd(stimdata.eng$log.density,na.rm=T)
min(stimdata.eng$log.density,na.rm=T)
max(stimdata.eng$log.density,na.rm=T)

#Spanish descriptive statistics for Appendix B
mean(stimdata.span$log.freq,na.rm=T)
sd(stimdata.span$log.freq,na.rm=T)
min(stimdata.span$log.freq,na.rm=T)
max(stimdata.span$log.freq,na.rm=T)

mean(stimdata.span$length,na.rm=T)
sd(stimdata.span$length,na.rm=T)
min(stimdata.span$length,na.rm=T)
max(stimdata.span$length,na.rm=T)

mean(stimdata.span$log.density,na.rm=T)
sd(stimdata.span$log.density,na.rm=T)
min(stimdata.span$log.density,na.rm=T)
max(stimdata.span$log.density,na.rm=T)

unique(stimdata$patient)
df <- tibble(unique(stimdata$item))
write_csv(df,'appendix A data.csv')

ggsave(filename='Psycholinguistic Histograms.png',plot=h.all,device='png',
       height=4.5,width=5,units='in',dpi=500)

g1 <- ggboxplot(stimdata,x='lang',y='log.freq',fill='lang',palette='jco',show.legend=F)+
  stat_compare_means(method='t.test',label='p.format',label.x=1.5,label.y=3)

g1 <- g1+ylab('Log Frequency')+theme(axis.title.y=element_text(size=11),legend.title=element_text(size=14),
                                     legend.text=element_text(size=11),axis.ticks.x=element_blank(),
               axis.text.x=element_blank(),axis.title.x=element_blank())+guides(fill=guide_legend(title='Language'))

g2 <- ggboxplot(stimdata,x='lang',y='length',fill='lang',palette='jco')+
  stat_compare_means(method='t.test',label='p.format',label.x=1.5,label.y=14)

g2 <- g2+ylab('Length (Phonemes)')+theme(axis.title.y=element_text(size=11),legend.position='bottom',axis.ticks.x=element_blank(),
               axis.text.x=element_blank(),axis.title.x=element_blank())+
  guides(fill=guide_legend(title='Language'))

g3 <- ggboxplot(stimdata,x='lang',y='log.density',fill='lang',palette='jco')+
  stat_compare_means(method='t.test',label='p.format',label.x=1.5,label.y=3)

g3 <- g3+ylab('Log Neighborhood Density')+theme(axis.title.y=element_text(size=11),legend.position='none',axis.ticks.x=element_blank(),
               axis.text.x=element_blank(),axis.title.x=element_blank())

#old code before I got ggarrange to work below
#g.all <- g1+g2+g3

#g.all

g.all <- ggarrange(g1,g2,g3,ncol=3,nrow=1,common.legend=T,legend='bottom')

ggsave(filename='Psycholinguistic Boxplots.tiff',plot=g.all,device='tiff',
       height=3,width=6,units='in',dpi=500)

#Modeling for psycholinguistic variables

#Transform variables

#log frequency, subtract min value to make predictions at intercept== least freq word
#treated.set1 <- na.omit(treated.set1)
treated.set1$log.freq <- log10(treated.set1$freq)
treated.set1$log.freq_min <- treated.set1$log.freq-(min(treated.set1$log.freq))

min(treated.set1$log.freq)
range(treated.set1$freq)
range(treated.set1$log.freq_min)

#length, subtract min value to make predictions at intercept== shortest word
treated.set1$length_min <- treated.set1$length-2

#log density, add 1 for log, then subtract min log value to make predication at intercept == 0 neighborhood
treated.set1$log.density <- log10(treated.set1$density+1)
treated.set1$log.density_min <- treated.set1$log.density-(min(treated.set1$log.density))
range(treated.set1$log.density_min)

#Create sum-coded variable for stimulus language
treated.set1$lang.sum <- factor(treated.set1$lang)
contrasts(treated.set1$lang.sum)<-contr.sum(2)
contrasts(treated.set1$lang.sum) <- c(-1,1)
levels(treated.set1$lang.sum)
contrasts(treated.set1$lang.sum)
summary(treated.set1$lang.sum)

#frequency model
modtreated.freq <- glmer(score~session*log.freq*lang.sum+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.freq)

unique(treated.set1$patient)

#length model
modtreated.length <- glmer(score~session*length*lang.sum+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.length)

#density model
modtreated.density <- glmer(score~session*log.density*lang.sum+(session|patient)+(1|item),data=treated.set1,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(modtreated.density)

#Plotting psycholinguistic models

#frequency plot
tiff(file = "Log Frequency Effect.tiff", width = 6.5, height = 4, units='in',res=500)
plot.freq <- plot(predictorEffects(modtreated.freq, ~ session,xlevels=list(log.freq=c(-.05,0.26,0.57,0.88,1.19))),
     axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
               x=list(rug=FALSE)),ylim=c(0,1),
     lines=list(multiline=TRUE,z.var="session"), #what's the difference between lang/session/psych variable
     lattice=list(layout=c(2,1),key.args=list(space='right',border=FALSE,title='Log Frequency')),
     main=NULL,
     xlab='Session',
     xaxt = "n")
dev.off()

#length plot
tiff(file = "Length Effect.tiff", width = 6.5, height = 4, units='in',res=500)
plot.length <- plot(predictorEffects(modtreated.length, ~ session,xlevels=list(length=c(2,4,6,8,10))),
     axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
               x=list(rug=FALSE)),ylim=c(0,1),
     lines=list(multiline=TRUE,z.var="session"), #what's the difference between lang/session/psych variable
     lattice=list(layout=c(2,1),key.args=list(space='right',border=FALSE,title='Length')),
     main=NULL,
     xlab='Session',
     xaxt = "n")
dev.off()

mean(stimdata$length, na.rm=T)

x2 <- mean(stimdata$log.density,na.rm=T)
y2 <- sd(stimdata$log.density,na.rm=T)

x2-y2
x2-0.5*y2
x2
x2+0.5*y2
x2+y2

#density plot
tiff(file = "Log Density Effect.tiff", width = 6.5, height = 4, units='in',res=500)
plot.density <- plot(predictorEffects(modtreated.density, ~ session,xlevels=list(log.density=c(0.14,0.40,0.66,0.92,1.18))),
     axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
               x=list(rug=FALSE)),ylim=c(0,1),
     lines=list(multiline=TRUE,z.var="session"), #what's the difference between lang/session/psych variable
     lattice=list(layout=c(2,1),key.args=list(space='right',border=FALSE,title='Log Density')),
     main=NULL,
     xlab='Session',
     xaxt = "n")
dev.off()

tiff(file='Combined Psycholinguistic Plots.tiff',width=6.5,height=4,units='in',res=500)
par(mfrow=c(3,1))
plot.freq <- plot(predictorEffects(modtreated.freq, ~ session,xlevels=list(log.freq=c(-.05,0.26,0.57,0.88,1.19))),
                  axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
                            x=list(rug=FALSE)),ylim=c(0,1),
                  lines=list(multiline=TRUE,z.var="session"), #what's the difference between lang/session/psych variable
                  lattice=list(layout=c(2,1),key.args=list(space='right',border=FALSE,title='Log Frequency')),
                  main=NULL,
                  xlab='Session',
                  xaxt = "n")
plot.length <- plot(predictorEffects(modtreated.length, ~ session,xlevels=list(length=c(2,4,6,8,10))),
                    axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
                              x=list(rug=FALSE)),ylim=c(0,1),
                    lines=list(multiline=TRUE,z.var="session"), #what's the difference between lang/session/psych variable
                    lattice=list(layout=c(2,1),key.args=list(space='right',border=FALSE,title='Length')),
                    main=NULL,
                    xlab='Session',
                    xaxt = "n")
plot.density <- plot(predictorEffects(modtreated.density, ~ session,xlevels=list(log.density=c(0.14,0.40,0.66,0.92,1.18))),
                     axes=list(grid=TRUE,y=list(type="response",lab="Predicted Probability of Correct Response"),
                               x=list(rug=FALSE)),ylim=c(0,1),
                     lines=list(multiline=TRUE,z.var="session"), #what's the difference between lang/session/psych variable
                     lattice=list(layout=c(2,1),key.args=list(space='right',border=FALSE,title='Log Density')),
                     main=NULL,
                     xlab='Session',
                     xaxt = "n")
dev.off()


#############################OLD##################################
##Post-hoc t-tests for significance testing##
#probably not needed since we are sum-coding#

#clear from plot above, but check if set 1 - set 3 is sig. different over time
emtrends(modtreated.overall,pairwise~set.sum,var='session') #averages over matchset variable; yes, sig. different

#test if accuracy over time is higher in L1 vs. L2
emtrends(modtreated.overall,pairwise~matchset.sum|set.sum,var='session') #yes for set 1, but not for set 3 (amazing result)
