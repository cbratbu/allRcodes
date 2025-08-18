library(tidyverse)

#Load in the data
##Two spreadsheets: 1) All error scores, & 2) All accuracy scores (binary)

errors.all <- read_csv('error data_error scores only.csv',na=c('')) #csv file saved to Code folder on old laptop

#transform wide data into long format
errors.long <- gather(errors.all,timepoint,score,B1_score,B2_score,B3_score,Tx1_score,Tx2_score,
                      Tx3_score,Tx4_score,Tx5_score,Tx6_score,Tx7_score,Tx8_score,
                      Tx9_score,Tx10_score,P1_score,P2_score,P3_score,factor_key=TRUE) 

session.trans=tibble(timepoint=c('B1_score','B2_score','B3_score','Tx1_score','Tx2_score',
                                 'Tx3_score','Tx4_score','Tx5_score','Tx6_score','Tx7_score','Tx8_score',
                                 'Tx9_score','Tx10_score','P1_score','P2_score','P3_score'),
                     session=0:15)

#join together new session and score variables
errors.all=errors.long %>% 
  left_join(session.trans)

#use score column to create error type column
errors.all$type <- ifelse(errors.all$score %in% c('1.5','1'),'no response',
                            ifelse(errors.all$score %in% c('2','2.5'),'neologism',
                                   ifelse(errors.all$score %in% c('3','3.5'),'perseveration',
                                          ifelse(errors.all$score %in% c('4','4.5'),'unrelated',
                                                 ifelse(errors.all$score %in% c('5','5.5'),'circumlocution',
                                                        ifelse(errors.all$score %in% c('6','6.5'),'semantic',
                                                               ifelse(errors.all$score %in% c('7','7.5'),'mixed',
                                                                      ifelse(errors.all$score %in% c('8','8.5'),'phonological',
                                                                             ifelse(errors.all$score %in% c('9', '9.25'),'correct (nontarget lang)',
                                                                                    ifelse(errors.all$score=='9.5','motor',
                                                                                           ifelse(errors.all$score=='10.25','correct (target lang)',
                                                                                                  ifelse(errors.all$score=='10','accent','correct (target lang)'))))))))))))


#factorize variables including score -- errors will be categorical from here on out?
errorscols <- c('patient','code','category','item','set','condition','lang','type')
errors.all[errorscols] <- lapply(errors.all[errorscols],factor)

errors.all #looks good

#Research Question 1: Treatment Results & Baseline Proportions of Errors
#Load in accuracy data
##Accuracy spreadsheet was created from the errors data by converting all error scores back to 1 or 0
accscores.all <- read_csv('error data_acc scores only.csv',na=c('')) #csv file saved to Code folder on old laptop

bntscores <- read_csv('bnt values.csv') #bnt scores were not in original spreadsheet and needed to be added for models
accscores.all <- left_join(accscores.all,bntscores,by=c('patient'='patient','lang'='lang'))

matchvalues <- read_csv('langmatch values.csv')
accscores.all <- left_join(accscores.all,matchvalues,by=c('patient'='patient','condition'='condition'))

#Perform same operation w/ session.trans
accscores.long <- gather(accscores.all,timepoint,score,B1_score,B2_score,B3_score,Tx1_score,Tx2_score,
                                       Tx3_score,Tx4_score,Tx5_score,Tx6_score,Tx7_score,Tx8_score,
                                       Tx9_score,Tx10_score,P1_score,P2_score,P3_score,factor_key=TRUE) 

#join together new session and score variables
accscores.all=accscores.long %>% 
  left_join(session.trans)

#factorize variables
accscorescols <- c('patient','code','category','item','set','condition','lang','langmatch')
accscores.all[accscorescols] <- lapply(accscores.all[accscorescols],factor)
accscores.all$set <- factor(accscores.all$set,levels=c('3','1','2'))

accscores.all #looks good

#RQ1: Treatment results for full sample n=48, all 3 sets, both treated & untreated
##Divide accscores.all into treated and untreated data sets

accscores.treated <- filter(accscores.all,condition=='treated')
accscores.untreated <- filter(accscores.all,condition=='untreated')

#libraries for mixed effects regressions
library(lme4)
library(lmerTest)

#######Need to add in BNT scores and treatment language (because previous tx paper showed these were important predictors)#######
###RQ1a###

#standardize BNT values within each dataset before modeling
accscores.treated$bnt.s <- scale(accscores.treated$bnt,center=T,scale=T)

mod_treatedacc <- glmer(score~session*set+bnt.s+langmatch+(session|patient)+(1|item),data=accscores.treated,family=binomial(link=logit),
                        control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(mod_treatedacc) 

mod_treatedacc.nolangmatch <- glmer(score~session*set+bnt.s+(session|patient)+(1|item),data=accscores.treated,family=binomial(link=logit),
                        control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(mod_treatedacc.nolangmatch) #no significant difference between set 2 and set 3 slopes, set 1 sig at baseline and over time, bnt sig

#test if langmatch is needed
anova(mod_treatedacc.nolangmatch,mod_treatedacc) #langmatch not needed; Chisq (1) =.002, p=.958
#mod_treatedacc.nolangmatch is the winner and this is the model reported in the paper

accscores.untreated$bnt.s <- scale(accscores.untreated$bnt,center=T,scale=T)
mod_untreatedacc <- glmer(score~session*set+bnt.s+langmatch+(session|patient)+(1|item),data=accscores.untreated,family=binomial(link=logit),
                          control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(mod_untreatedacc) #no significant difference between set 2 and set 3 slopes, set 1 sig, session (set 3) not significant

mod_untreatedacc.nolangmatch <- glmer(score~session*set+bnt.s+(session|patient)+(1|item),data=accscores.untreated,family=binomial(link=logit),
                          control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(mod_untreatedacc.nolangmatch)

#test if langmatch is needed again
anova(mod_untreatedacc,mod_untreatedacc.nolangmatch) #langmatch not needed; Chisq(1)=1.21, p=.270
#mod_untreatedacc.nolangmatch is the winner and this is the model reported in the paper


#gather predictions for plotting from model output
library(broom) #uses this package
pred_treated_overall <- augment_columns(mod_treatedacc.nolangmatch, accscores.treated,
                                          type.predict = c("response")) %>%
  rename(pred_prob = .fitted) %>% 
  # calculate overall mean
  group_by(patient,session, set) %>% 
  summarise(Acc = mean(score), 
            Acc_pred = mean(pred_prob))
summary(pred_treated_overall)

pred_treated_overall$set <- factor(pred_treated_overall$set,levels=c('1','2','3'),labels=
                                     c('Trained Words','Semantically Related Words','Control Words'))
pred_treated_overall %>% rename('Word Set'='set')

#figure 2A
plottreated.overall <- ggplot(pred_treated_overall, aes(session, Acc, colour = set, group= set)) +
  #scale_colour_brewer(name='Word Set',limits=c('1','2','3'),
                      #labels=c('Trained Words','Semantically Related Words','Control Words'),palette = 'Set1')+
  stat_smooth(aes(y=Acc,fill=set),method='loess')+
  expand_limits(y=c(0,1))+
  geom_vline(xintercept=c(2,12),linetype='dashed')+
  #ggtitle('Naming Accuracy in the Untreated Language')+
  xlab('Session (Probe #)')+
  ylab('Predicted Proportion of Correct Words')+
  labs(fill='Word Set',colour='Word Set')+
  theme_classic()+
  theme(plot.title=element_text(hjust=.5,size=16),
        legend.title=element_text(hjust=.5,size=14),
        strip.text.x=element_text(size=14,color='black'),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        legend.position='right')+
  guides(fill=guide_legend(title.position='top',title.hjust = 0.5))
plottreated.overall

#ggsave(filename='Fig 3.Total Error Proportions.tiff',plot=error.prop.plot,device='tiff',
       #height=7.5,width=8.2,units='in',dpi=500) #Looks good

pred_untreated_overall <- augment_columns(mod_untreatedacc.nolangmatch, accscores.untreated,
                                        type.predict = c("response")) %>%
  rename(pred_prob = .fitted) %>% 
  # calculate overall mean
  group_by(patient,session, set) %>% 
  summarise(Acc = mean(score), 
            Acc_pred = mean(pred_prob))
summary(pred_untreated_overall)

pred_untreated_overall$set <- factor(pred_untreated_overall$set,levels=c('1','2','3'),labels=
                                     c('Trained Words','Semantically Related Words','Control Words'))

#figure 2B
plotuntreated.overall <- ggplot(pred_untreated_overall, aes(session, Acc, colour = set, group= set)) +
  #scale_colour_brewer(name='Word Set',limits=c('1','2','3'),
  #labels=c('Trained Words','Semantically Related Words','Control Words'),palette = 'Set1')+
  stat_smooth(aes(y=Acc,fill=set),method='loess')+
  expand_limits(y=c(0,1))+
  geom_vline(xintercept=c(2,12),linetype='dashed')+
  #ggtitle('Naming Accuracy in the Untreated Language')+
  xlab('Session (Probe #)')+
  ylab('Predicted Proportion of Correct Words')+
  labs(fill='Word Set Translations',colour='Word Set Translations')+
  theme_classic()+
  theme(plot.title=element_text(hjust=.5,size=16),
        legend.title=element_text(hjust=.5,size=14),
        strip.text.x=element_text(size=14,color='black'),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        legend.position='right')+
  guides(fill=guide_legend(title.position='top',title.hjust = 0.5))
plotuntreated.overall

combined.overallplots<- plot_grid(plottreated.overall, plotuntreated.overall, 
                                  nrow = 2,labels=c('A','B'),hjust=-4.5)
combined.overallplots

#Final Figure 2
ggsave(filename='Fig2.Overall Accuracy.tiff',plot=combined.overallplots,device='tiff',
       height=8.2,width=8.2,units='in',dpi=500)

###RQ1b###
errors.baseline=errors.all %>% 
  filter(session<3) %>% 
  filter(!type %in% c('motor','accent','NA','correct (target lang)')) %>% 
  droplevels() #motor, accent are too few; NAs for patients who have fewer than 16 probes; don't include correctTL

#Reorder so that the levels increase/get closer to correct in target lang
errors.baseline$type <- factor(errors.baseline$type, levels=c('no response','neologism','perseveration',
                                                              'unrelated','circumlocution','semantic',
                                                              'mixed','phonological','correct (nontarget lang)')) 

#Calculate error proportions
errors.prop <- errors.baseline %>%
  ##This groups and counts the occurrences
  group_by(patient,set,condition,type) %>% 
  tally() %>%
  ungroup() %>% 
  ##this groups and calculates the proportion for the groups above without the score.
  group_by(patient,set,condition) %>% 
  mutate(prop = n/sum(n)) %>% 
  complete(type,nesting(patient,set, condition),fill=list(prop=0)) #this line ensures that categories w/ 0 observations are still retained

errors.prop #looks good

write.csv(errors.prop,'error proportions_baseline.csv') #I transposed the error proportions into 6 separate spreadsheets (set x condition) outside of R

#Read in the 6 new spreadsheets
treatedset1.baseprop <- read_csv('treatedset1_baseline proportions.csv')
treatedset2.baseprop <- read_csv('treatedset2_baseline proportions.csv')
treatedset3.baseprop <- read_csv('treatedset3_baseline proportions.csv')
untreatedset1.baseprop <- read_csv('untreatedset1_baseline proportions.csv')
untreatedset2.baseprop <- read_csv('untreatedset2_baseline proportions.csv')
untreatedset3.baseprop <- read_csv('untreatedset3_baseline proportions.csv')

#Fit individual set x condition models to extract patient effect sizes
##Filter treated set data first
accscores.treated.set1 <- filter(accscores.treated,set=='1')
accscores.treated.set2 <- filter(accscores.treated,set=='2')
accscores.treated.set3 <- filter(accscores.treated,set=='3')

##Filter untreated set data next
accscores.untreated.set1 <- filter(accscores.untreated,set=='1')
accscores.untreated.set2 <- filter(accscores.untreated,set=='2')
accscores.untreated.set3 <- filter(accscores.untreated,set=='3')

#Load library for correlation analyses
library(correlation)
#For correlation analyses, data = coefficient data frame created from model, data2 = baseline proportion sheet (x6)
##treated set 1 analyses

##----------Treated set 1 data----------##

###Treated set 1 model for individual effect sizes
mod_treatedset1 <- glmer(score~session+(session|patient)+(1|item),data=accscores.treated.set1,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(mod_treatedset1)

treatedset1.coef <- cbind(coef(mod_treatedset1)$patient) #extract patient effect sizes and create data frame
treatedset1.coef <- treatedset1.coef %>% select(-'(Intercept)') #don't need this, get rid of it
write_csv(treatedset1.coef,'treatedset1_mod coeff.csv') #save for future use

###Treated set 1 correlations
treatedset1.corr.unadjusted <- correlation(data=treatedset1.coef,data2=treatedset1.baseprop,
                                           include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='none') #for uncorrected p-values

treatedset1.corr.adjusted <- correlation(data=treatedset1.coef,data2=treatedset1.baseprop,
                                         include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='fdr') #use FDR correction

treatedset1.corr.both <- data.frame(rbind(treatedset1.corr.unadjusted,treatedset1.corr.adjusted)) #combine both into 1 dataframe
write_csv(treatedset1.corr.both,'treated set 1 correlations.csv')

##-------------Treated set 2 data------------##

###Treated set 2 model for individual effect sizes
mod_treatedset2 <- glmer(score~session+(session|patient)+(1|item),data=accscores.treated.set2,family=binomial(link=logit),
                        control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(mod_treatedset2)

treatedset2.coef <- cbind(coef(mod_treatedset2)$patient)
treatedset2.coef <- treatedset2.coef %>% select(-'(Intercept)')
write_csv(treatedset2.coef,'treatedset2_mod coeff.csv') #save for future use

###Treated set 2 correlations
treatedset2.corr.unadjusted <- correlation(data=treatedset2.coef,data2=treatedset2.baseprop,
            include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='none') #for uncorrected p-values

treatedset2.corr.adjusted <- correlation(data=treatedset2.coef,data2=treatedset2.baseprop,
            include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='fdr') #use FDR correction

treatedset2.corr.both <- data.frame(rbind(treatedset2.corr.unadjusted,treatedset2.corr.adjusted))
write_csv(treatedset2.corr.both,'treated set 2 correlations.csv')

##-------------Treated set 3 data------------##

###Treated set 3 model for individual effect sizes
mod_treatedset3 <- glmer(score~session+(session|patient)+(1|item),data=accscores.treated.set3,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(mod_treatedset3)

treatedset3.coef <- cbind(coef(mod_treatedset3)$patient)
treatedset3.coef <- treatedset3.coef %>% select(-'(Intercept)')
write_csv(treatedset2.coef,'treatedset3_mod coeff.csv') #save for future use

###Treated set 3 correlations
treatedset3.corr.unadjusted <- correlation(data=treatedset3.coef,data2=treatedset3.baseprop,
                                           include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='none') #for uncorrected p-values

treatedset3.corr.adjusted <- correlation(data=treatedset3.coef,data2=treatedset3.baseprop,
                                         include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='fdr') #use FDR correction

treatedset3.corr.both <- data.frame(rbind(treatedset3.corr.unadjusted,treatedset3.corr.adjusted))
write_csv(treatedset3.corr.both,'treated set 3 correlations.csv')

##-------------Untreated set 1 data------------##

###Untreated set 1 model for individual effect sizes
mod_untreatedset1 <- glmer(score~session+(session|patient)+(1|item),data=accscores.untreated.set1,family=binomial(link=logit),
                         control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(mod_untreatedset1)

untreatedset1.coef <- cbind(coef(mod_untreatedset1)$patient)
untreatedset1.coef <- untreatedset1.coef %>% select(-'(Intercept)')
write_csv(untreatedset1.coef,'untreatedset1_mod coeff.csv') #save for future use

###Untreated set 1 correlations
untreatedset1.corr.unadjusted <- correlation(data=untreatedset1.coef,data2=untreatedset1.baseprop,
                                           include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='none') #for uncorrected p-values

untreatedset1.corr.adjusted <- correlation(data=untreatedset1.coef,data2=untreatedset1.baseprop,
                                         include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='fdr') #use FDR correction

untreatedset1.corr.both <- data.frame(rbind(untreatedset1.corr.unadjusted,untreatedset1.corr.adjusted))
write_csv(untreatedset1.corr.both,'untreated set 1 correlations.csv')

##-------------Untreated set 2 data------------##

###Untreated set 2 model for individual effect sizes
mod_untreatedset2 <- glmer(score~session+(session|patient)+(1|item),data=accscores.untreated.set2,family=binomial(link=logit),
                           control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))

summary(mod_untreatedset2)

untreatedset2.coef <- cbind(coef(mod_untreatedset2)$patient)
untreatedset2.coef <- untreatedset2.coef %>% select(-'(Intercept)')
write_csv(untreatedset2.coef,'untreatedset2_mod coeff.csv') #save for future use

###Untreated set 2 correlations
untreatedset2.corr.unadjusted <- correlation(data=untreatedset2.coef,data2=untreatedset2.baseprop,
                                             include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='none') #for uncorrected p-values

untreatedset2.corr.adjusted <- correlation(data=untreatedset2.coef,data2=untreatedset2.baseprop,
                                           include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='fdr') #use FDR correction

untreatedset2.corr.both <- data.frame(rbind(untreatedset2.corr.unadjusted,untreatedset2.corr.adjusted))
write_csv(untreatedset2.corr.both,'untreated set 2 correlations.csv')

##-------------Untreated set 3 data------------##

###Untreated set 3 model for individual effect sizes
mod_untreatedset3 <- glmer(score~session+(1+session|patient)+(1|item),data=accscores.untreated.set3,family=binomial(link=logit))
#strangely does not converge with 'bobyqa'

summary(mod_untreatedset3)

untreatedset3.coef <- cbind(coef(mod_untreatedset3)$patient)
untreatedset3.coef <- untreatedset3.coef %>% select(-'(Intercept)')
write_csv(untreatedset3.coef,'untreatedset3_mod coeff.csv') #save for future use

###Untreated set 3 correlations
untreatedset3.corr.unadjusted <- correlation(data=untreatedset3.coef,data2=untreatedset3.baseprop,
                                             include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='none') #for uncorrected p-values

untreatedset3.corr.adjusted <- correlation(data=untreatedset3.coef,data2=untreatedset3.baseprop,
                                           include_factors = TRUE, method = "spearman",use='complete.obs',p_adjust='fdr') #use FDR correction


untreatedset3.corr.both <- data.frame(rbind(untreatedset3.corr.unadjusted,untreatedset3.corr.adjusted))
write_csv(untreatedset3.corr.both,'untreated set 3 correlations.csv')

#RQ2: Error evolution over course of treatment
errors.prop.overtx=errors.all %>% 
  filter(!type %in% c('motor','accent','NA')) %>% 
  na.omit() %>% 
  droplevels() #motor, accent are too few; NAs for patients who have fewer than 16 probes; don't include correctTL

#Reorder so that the levels increase/get closer to correct in target lang
errors.prop.overtx$type <- factor(errors.prop.overtx$type, levels=c('correct (target lang)','correct (nontarget lang)','phonological',
                                                              'mixed','semantic','circumlocution',
                                                              'unrelated','perseveration','neologism','no response'))

errors.prop.overtx$set <- factor(errors.prop.overtx$set,levels=c('1','2','3'),
                                   labels=c('Trained Words','Semantically Related Words','Control Words'))

errors.prop.overtx$condition <- factor(errors.prop.overtx$condition,levels=c('treated','untreated'),
                                 labels=c('Treated Language','Untreated Language'))

#Calculate error proportions
errors.prop.all <- errors.prop.overtx %>%
  ##This groups and counts the occurrences
  group_by(set,condition,session,type) %>% 
  tally() %>%
  ungroup() %>% 
  ##this groups and calculates the proportion for the groups above without the score.
  group_by(set,session,condition) %>% 
  mutate(prop = n/sum(n)) %>% 
  complete(type,nesting(set,condition,session),fill=list(count=0)) #this line ensures that categories w/ 0 observations are still retained

errors.prop.all

#Use this new dataframe to create the error proportions plot (Figure 3 in manuscript)
errors.prop.all$type <- factor(errors.prop.all$type,levels=c('no response','neologism','perseveration',
                                                             'unrelated','circumlocution','semantic',
                                                             'mixed','phonological','correct (nontarget lang)','correct (target lang)'))

error.prop.plot <- ggplot(errors.prop.all,aes(x=session,y=prop,fill=type))+
  geom_bar(position='stack',stat='identity')+
  labs(x='Session (Probe #)',y='Proportion',fill='Type')+
  facet_wrap(~condition+set)+
  theme_classic()

error.prop.plot

ggsave(filename='Fig 3.Total Error Proportions.tiff',plot=error.prop.plot,device='tiff',
       height=7.5,width=8.2,units='in',dpi=500) #Looks good

##Mixed effects poisson regression for error counts over tx
##Count up errors across patients, sets, and conditions (no item-level data for poisson regression)
poisson.errors <- errors.prop.overtx %>% 
  count(patient,set,condition,session,type,sort=TRUE,.drop=FALSE) #use errors.prop.overtx because it is already cleaned


poisson.errors$type <- factor(poisson.errors$type, levels=c('no response','neologism','perseveration',
                                                              'unrelated','circumlocution','semantic',
                                                              'mixed','phonological','correct (nontarget lang)','correct (target lang)')) 
library(MASS)
library(multcomp)
library(lmtest)

#Create a contrast matrix with 20 terms because each poisson model will have 20 terms

contrast.matrix.intercept <- rbind(
  `intercept:NR`	=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:NEO`	=c(1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:PER`	=c(1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:UNR`	=c(1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:CIR`	=c(1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:SEM`	=c(1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:MIX`	=c(1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:PHO`	=c(1,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0),
  `intercept:CNL`	=c(1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0),
  `intercept:CTL` =c(1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0))

contrast.matrix.slope <- rbind(
  `session:NR`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  `session:NEO`	=c(0,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0),
  `session:PER`	=c(0,1,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0),
  `session:UNR`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0),
  `session:CIR`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0),
  `session:SEM`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0),
  `session:MIX`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0),
  `session:PHO`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0),
  `session:CNL`	=c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0),
  `session:CTL` =c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1)) 

#This function creates a table with the model outputs based on the results of the contrast matrices
table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), 
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
  
}

##----------Treated set 1 data----------##
treatedset1.poisson.errors <- poisson.errors %>% 
  filter(set=='Trained Words' & condition=='Treated Language') %>% 
  droplevels()

###Treated set 1 counts regression
#fit poisson first and then compare to negative binomial
poissonmod_treatedset1 <- glm(n~session*type,data=treatedset1.poisson.errors,family=poisson(link='log'))

#assess conditional mean and variance
mean(poissonmod_treatedset1$fitted.values)
var(poissonmod_treatedset1$fitted.values) #poissonmod is overdispersed

#try negative binomial model
negbinmod_treatedset1 <- glm.nb(n~session*type,data=treatedset1.poisson.errors)

#perform likelihood ratio test
lrtest(poissonmod_treatedset1,negbinmod_treatedset1) #negative binomial preferred

treatedset1.poisson.errors$modelval <- negbinmod_treatedset1$fitted.values #feed the fitted values back into the dataframe
summary(negbinmod_treatedset1)

###Test treated set 1 predictions using contrast matrix
summary(glht(negbinmod_treatedset1, contrast.matrix.slope),test=adjusted("none"))
summary(glht(negbinmod_treatedset1, contrast.matrix.slope),test=adjusted("BH")) #these are the same
summary(glht(negbinmod_treatedset1, contrast.matrix.intercept),test=adjusted("BH")) #these are the same

###Prepare to make individual rate of change plots (Figures 4 & 5)

#Create separate tables for slopes and intercepts
slopes.treatedset1<-table_glht(glht(negbinmod_treatedset1, contrast.matrix.slope,test=adjusted("BH"))) #put slopes in a table
intercepts.treatedset1<-table_glht(glht(negbinmod_treatedset1, contrast.matrix.intercept,test=adjusted("BH"))) #put slopes in a table

#Bind the slope and intercept tables together
slopes.intercepts.treatedset1<-data.frame(rbind(slopes.treatedset1,intercepts.treatedset1))

#Create a new column that labels the response type and whether each row is a slope or intercept
slopes.intercepts.treatedset1<-slopes.intercepts.treatedset1%>% 
  mutate(type=c("NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL","NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL"),
         parameter=c("slope","slope","slope","slope","slope","slope","slope","slope","slope","slope",
                     "intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept"
         ))

slopes.intercepts.treatedset1<-slopes.intercepts.treatedset1 %>% 
  mutate(IRR=exp(as.numeric(Estimate))) #Add in Incidence Rate Ratios (IRR)

slopes.intercepts.treatedset1 <- slopes.intercepts.treatedset1 %>% 
  mutate(Percent=(as.numeric(IRR-1))) #Add in Percentages (i.e., IRR-1 *100%)

#Save the combined table for supplemental material?
write_csv(slopes.intercepts.treatedset1,'treated set 1_contrast matrix results.csv')

#caption<-c("horizontal line reflects predicted improvement in accuracy after one semester of intervention based on the slope estimate")
treatedset1.changeplot<-read.csv("treatedset1_changeplot.csv")
treatedset1.changeplot$type <- factor(treatedset1.changeplot$type,levels=c('NOR','NEO','PER','UNR','CIR',
                                                                           'SEM','MIX','PHO','CNL','CTL'))

#these are predicted values from the model not the raw data!
treatedset1_plot<-ggplot(treatedset1.changeplot,aes(x=intercept,y=slope,label=type,color=type))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax = intercept.err.upper, xmin = intercept.err.lower))+
  #annotate("text", x=.56,y=0,labels=stringr::str_wrap(labels,width=20))+
  labs(x="",y='')+
  #geom_label(aes(fill=factor(type)),size=3,fontface="bold",colour="black",nudge_y = -.005)+
  geom_hline(yintercept=0)+
  ylim(-0.2,0.2)+
  #scale_x_continuous(breaks = c(.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0))+
  theme_classic()+
  theme(legend.position='none',plot.title=element_text(hjust=0.5),
        legend.title=element_text(size=10),plot.caption=(element_text(hjust=0,size=10)))

treatedset1_plot 

ggsave(filename='Fig 4A.Total Evolution of Treated Set 1 Errors.tiff',plot=treatedset1_plot,device='tiff',
       height=5,width=7.5,units='in',dpi=500) #Looks good

##----------Treated set 2 data----------##

treatedset2.poisson.errors <- poisson.errors %>% 
  filter(set=='Semantically Related Words' & condition=='Treated Language') %>% 
  droplevels()

###Treated set 2 counts regression
poissonmod_treatedset2 <- glm(n~session*type,data=treatedset2.poisson.errors,family=poisson(link='log'))

#assess conditional mean and variance
mean(poissonmod_treatedset2$fitted.values)
var(poissonmod_treatedset2$fitted.values) #poissonmod is not overdispersed but let's stick with negbin

negbinmod_treatedset2 <- glm.nb(n~session*type,data=treatedset2.poisson.errors)

#perform likelihood ratio test
lrtest(poissonmod_treatedset2,negbinmod_treatedset2) #negative binomial preferred

treatedset2.poisson.errors$modelval <- negbinmod_treatedset2$fitted.values #feed the fitted values back into the dataframe
summary(negbinmod_treatedset2)

###Test predictions using contrast matrix
summary(glht(negbinmod_treatedset2, contrast.matrix.slope),test=adjusted("none"))
summary(glht(negbinmod_treatedset2, contrast.matrix.slope),test=adjusted("BH"))
summary(glht(negbinmod_treatedset2, contrast.matrix.intercept),test=adjusted("BH"))
#CTL is only significant change afterwards

###Prepare to make individual rate of change plots (Figures 4 & 5)

#Create separate tables for slopes and intercepts
slopes.treatedset2<-table_glht(glht(negbinmod_treatedset2, contrast.matrix.slope,test=adjusted("BH"))) #put slopes in a table
intercepts.treatedset2<-table_glht(glht(negbinmod_treatedset2, contrast.matrix.intercept,test=adjusted("BH"))) #put slopes in a table

#Bind the slope and intercept tables together
slopes.intercepts.treatedset2<-data.frame(rbind(slopes.treatedset2,intercepts.treatedset2))

#Create a new column that labels the response type and whether each row is a slope or intercept
slopes.intercepts.treatedset2<-slopes.intercepts.treatedset2%>% 
  mutate(type=c("NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL","NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL"),
         parameter=c("slope","slope","slope","slope","slope","slope","slope","slope","slope","slope",
                     "intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept"
         ))

slopes.intercepts.treatedset2<-slopes.intercepts.treatedset2 %>% 
  mutate(IRR=exp(as.numeric(Estimate))) #Add in Incidence Rate Ratios (IRR)

slopes.intercepts.treatedset2 <- slopes.intercepts.treatedset2 %>% 
  mutate(Percent=(as.numeric(IRR-1))) #Add in Percentages (i.e., IRR-1 *100%)

#Save the combined table for supplemental material?
write_csv(slopes.intercepts.treatedset2,'treated set 2_contrast matrix results.csv')

treatedset2.changeplot<-read.csv("treatedset2_changeplot.csv")
treatedset2.changeplot$type <- factor(treatedset2.changeplot$type,levels=c('NOR','NEO','PER','UNR','CIR',
                                                                           'SEM','MIX','PHO','CNL','CTL'))

#these are predicted values from the model not the raw data!
treatedset2_plot<-ggplot(treatedset2.changeplot,aes(x=intercept,y=slope,label=type,color=type))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax = intercept.err.upper, xmin = intercept.err.lower))+
  #annotate("text", x=.56,y=0,labels=stringr::str_wrap(labels,width=20))+
  labs(x="",y='Rate of Change in Log Expected Counts')+
  #geom_label(aes(fill=factor(type)),size=3,fontface="bold",colour="black",nudge_y = -.015)+
  geom_hline(yintercept=0)+
  ylim(-0.2,0.2)+
  #scale_x_continuous(breaks = c(.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0))+
  theme_classic()+
  theme(legend.position='none',plot.title=element_text(hjust=0.5),
        legend.title=element_text(size=10),plot.caption=(element_text(hjust=0,size=10)))
treatedset2_plot 

ggsave(filename='Fig 4B.Total Evolution of Treated Set 2 Errors.tiff',plot=treatedset2_plot,device='tiff',
       height=5,width=7.5,units='in',dpi=500) #Looks good

##----------Treated set 3 data----------##
treatedset3.poisson.errors <- poisson.errors %>% 
  filter(set=='Control Words' & condition=='Treated Language') %>% 
  droplevels()

###Treated set 3 counts regression
poissonmod_treatedset3 <- glm(n~session*type,data=treatedset3.poisson.errors,family=poisson(link='log'))

#assess conditional mean and variance
mean(poissonmod_treatedset3$fitted.values)
var(poissonmod_treatedset3$fitted.values) #poissonmod is not overdispersed but let's stick with negbin

negbinmod_treatedset3 <- glm.nb(n~session*type,data=treatedset3.poisson.errors)

#perform likelihood ratio test
lrtest(poissonmod_treatedset3,negbinmod_treatedset3) #negative binomial preferred

treatedset3.poisson.errors$modelval <- negbinmod_treatedset3$fitted.values #feed the fitted values back into the dataframe
summary(negbinmod_treatedset3)

###Test predictions using contrast matrix
summary(glht(negbinmod_treatedset3, contrast.matrix.slope),test=adjusted("none"))
summary(glht(negbinmod_treatedset3, contrast.matrix.slope),test=adjusted("BH"))
summary(glht(negbinmod_treatedset3, contrast.matrix.intercept),test=adjusted("BH"))
#CTL and PER are only significant change afterwards (survive correction)

#Create separate tables for slopes and intercepts
slopes.treatedset3<-table_glht(glht(negbinmod_treatedset3, contrast.matrix.slope,test=adjusted("BH"))) #put slopes in a table
intercepts.treatedset3<-table_glht(glht(negbinmod_treatedset3, contrast.matrix.intercept,test=adjusted("BH"))) #put slopes in a table

slopes.treatedset3

#Bind the slope and intercept tables together
slopes.intercepts.treatedset3<-data.frame(rbind(slopes.treatedset3,intercepts.treatedset3))

#Create a new column that labels the response type and whether each row is a slope or intercept
slopes.intercepts.treatedset3<-slopes.intercepts.treatedset3%>% 
  mutate(type=c("NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL","NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL"),
         parameter=c("slope","slope","slope","slope","slope","slope","slope","slope","slope","slope",
                     "intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept"
         ))

slopes.intercepts.treatedset3<-slopes.intercepts.treatedset3 %>% 
  mutate(IRR=exp(as.numeric(Estimate))) #Add in Incidence Rate Ratios (IRR)

slopes.intercepts.treatedset3 <- slopes.intercepts.treatedset3 %>% 
  mutate(Percent=(as.numeric(IRR-1))) #Add in Percentages (i.e., IRR-1 *100%)

#Save the combined table for supplemental material?
write_csv(slopes.intercepts.treatedset3,'treated set 3_contrast matrix results.csv')

treatedset3.changeplot<-read.csv("treatedset3_changeplot.csv")
treatedset3.changeplot$type <- factor(treatedset3.changeplot$type,levels=c('NOR','NEO','PER','UNR','CIR',
                                                                           'SEM','MIX','PHO','CNL','CTL'))

#these are predicted values from the model not the raw data!
treatedset3_plot<-ggplot(treatedset3.changeplot,aes(x=intercept,y=slope,label=type,color=type))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax = intercept.err.upper, xmin = intercept.err.lower))+
  #annotate("text", x=.56,y=0,labels=stringr::str_wrap(labels,width=20))+
  labs(x="Log Expected Counts at Baseline",y="",color='Type')+
  #geom_label(aes(fill=factor(type)),size=3,fontface="bold",colour="black",nudge_y = -.015)+
  geom_hline(yintercept=0)+
  ylim(-0.2,0.2)+
  #scale_x_continuous(breaks = c(.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0))+
  theme_classic()+
  theme(legend.position='none',plot.title=element_text(hjust=0.5),
        legend.title=element_text(size=10),plot.caption=(element_text(hjust=0,size=10)))

treatedset3_plot
ggsave(filename='Fig 4C.Total Evolution of Treated Set 3 Errors.tiff',plot=treatedset3_plot,device='tiff',
              height=5,width=7.5,units='in',dpi=500)


#cowplot to the rescue; This is the overall code for Fig 4
plot_legend <- get_legend(treatedset3_plot+theme(legend.position='right')) 
plot_legend #can be used later

alltreatedplots <- plot_grid(treatedset1_plot+
                    annotate("text",x=0,y=0.18,label='Trained Words',size=4)+
                    theme(legend.position="none")+labs(x=''),#+theme(plot.margin = unit(c(0,1,0,0), "cm")),
                   treatedset2_plot+
                    annotate("text",x=0,y=0.18,label='Semantically Related Words',size=4)+
                    theme(legend.position="none")+labs(x='',y='Rate of Change'),#+theme(plot.margin = unit(c(0,1,0,0), "cm")),
                   treatedset3_plot+
                    annotate("text",x=0,y=0.18,label='Control Words',size=4)+
                    theme(legend.position="none")+labs(x='Log Expected Counts at Baseline'),#+theme(plot.margin = unit(c(0,1,0,0), "cm")),
                   align = 'v',
                   labels = c("A", "B", "C"),
                   hjust = -4.5,
                   ncol = 1
)

combined.treatedplots<- plot_grid(alltreatedplots, plot_legend, ncol = 2,rel_widths = c(1,0.5))
combined.treatedplots

ggsave(filename='Fig4.Evolution of Errors in Treated Language.tiff',plot=combined.treatedplots,device='tiff',
       height=8.2,width=8.2,units='in',dpi=500)

library(cowplot)

##----------Untreated set 1 data----------##

untreatedset1.poisson.errors <- poisson.errors %>% 
  filter(set=='Trained Words' & condition=='Untreated Language') %>% 
  droplevels()

###Untreated set 1 poisson regression

poissonmod_untreatedset1 <- glm(n~session*type,data=untreatedset1.poisson.errors,family=poisson(link='log'))
summary(poissonmod_untreatedset1)

mean(poissonmod_untreatedset1$fitted.values)
var(poissonmod_untreatedset1$fitted.values) #approx equal but let's use negbinmod

negbinmod_untreatedset1 <- glm.nb(n~session*type,data=untreatedset1.poisson.errors)
lrtest(poissonmod_untreatedset1,negbinmod_untreatedset1) #negbin preferred

###Test predictions using contrast matrix
summary(glht(negbinmod_untreatedset1, contrast.matrix.slope),test=adjusted("none"))
summary(glht(negbinmod_untreatedset1, contrast.matrix.slope),test=adjusted("BH"))
summary(glht(negbinmod_untreatedset1, contrast.matrix.intercept),test=adjusted("BH"))
#CTL, CNL are pos and survive; CIR, SEM, MIX are neg and survive

#Create separate tables for slopes and intercepts
slopes.untreatedset1<-table_glht(glht(negbinmod_untreatedset1, contrast.matrix.slope,test=adjusted('BH'))) #put slopes in a table
intercepts.untreatedset1<-table_glht(glht(negbinmod_untreatedset1, contrast.matrix.intercept,test=adjusted('BH'))) #put slopes in a table

#Bind the slope and intercept tables together
slopes.intercepts.untreatedset1<-data.frame(rbind(slopes.untreatedset1,intercepts.untreatedset1))

#Create a new column that labels the response type and whether each row is a slope or intercept
slopes.intercepts.untreatedset1<-slopes.intercepts.untreatedset1%>% 
  mutate(type=c("NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL","NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL"),
         parameter=c("slope","slope","slope","slope","slope","slope","slope","slope","slope","slope",
                     "intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept"
         ))

slopes.intercepts.untreatedset1<-slopes.intercepts.untreatedset1 %>% 
  mutate(IRR=exp(as.numeric(Estimate))) #Add in Incidence Rate Ratios (IRR)

slopes.intercepts.untreatedset1 <- slopes.intercepts.untreatedset1 %>% 
  mutate(Percent=(as.numeric(IRR-1))) #Add in Percentages (i.e., IRR-1 *100%)

#Save the combined table for supplemental material?
write_csv(slopes.intercepts.untreatedset1,'untreated set 1_contrast matrix results.csv')

untreatedset1.changeplot<-read.csv("untreatedset1_changeplot.csv")
untreatedset1.changeplot$type <- factor(untreatedset1.changeplot$type,levels=c('NOR','NEO','PER','UNR','CIR',
                                                                           'SEM','MIX','PHO','CNL','CTL'))

#these are predicted values from the model not the raw data!
untreatedset1_plot<-ggplot(untreatedset1.changeplot,aes(x=intercept,y=slope,label=type,color=type))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax = intercept.err.upper, xmin = intercept.err.lower))+
  #annotate("text", x=.56,y=0,labels=stringr::str_wrap(labels,width=20))+
  labs(title='Expected Change in Response Types in Untreated Set 1',x="Log Expected Counts at Baseline",y="Rate of Change",color='Type')+
  #geom_label(aes(fill=factor(type)),size=3,fontface="bold",colour="black",nudge_y = -.015)+
  geom_hline(yintercept=0)+
  ylim(-0.2,0.2)+
  scale_x_continuous(breaks = c(-1,0,1))+
  theme_classic()+
  theme(legend.position='none',plot.title=element_text(hjust=0.5),
        legend.title=element_text(size=10),plot.caption=(element_text(hjust=0,size=10)))

untreatedset1_plot

ggsave(filename='Fig 5A.Total Evolution of Untreated Set 1 Errors.tiff',plot=untreatedset1_plot,device='tiff',
       height=5,width=7.5,units='in',dpi=500)

##----------Untreated set 2 data----------##

untreatedset2.poisson.errors <- poisson.errors %>% 
  filter(set=='Semantically Related Words' & condition=='Untreated Language') %>% 
  droplevels()

###Untreated set 2 counts regression
poissonmod_untreatedset2 <- glm(n~session*type,data=untreatedset2.poisson.errors,family=poisson(link='log'))
summary(poissonmod_untreatedset2)

mean(poissonmod_untreatedset2$fitted.values)
var(poissonmod_untreatedset2$fitted.values) #not the same, use negbin

negbinmod_untreatedset2 <- glm.nb(n~session*type,data=untreatedset2.poisson.errors)
lrtest(poissonmod_untreatedset2,negbinmod_untreatedset2) #negbin mod preferred

###Test predictions using contrast matrix
summary(glht(negbinmod_untreatedset2, contrast.matrix.slope),test=adjusted("none"))
summary(glht(negbinmod_untreatedset2, contrast.matrix.slope),test=adjusted("BH"))
summary(glht(negbinmod_untreatedset2, contrast.matrix.intercept),test=adjusted("BH"))
#CTL only thing significant

#Create separate tables for slopes and intercepts
slopes.untreatedset2<-table_glht(glht(negbinmod_untreatedset2, contrast.matrix.slope,test=adjusted("BH"))) #put slopes in a table
intercepts.untreatedset2<-table_glht(glht(negbinmod_untreatedset2, contrast.matrix.intercept,test=adjusted("BH"))) #put slopes in a table

#Bind the slope and intercept tables together
slopes.intercepts.untreatedset2<-data.frame(rbind(slopes.untreatedset2,intercepts.untreatedset2))

#Create a new column that labels the response type and whether each row is a slope or intercept
slopes.intercepts.untreatedset2<-slopes.intercepts.untreatedset2%>% 
  mutate(type=c("NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL","NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL"),
         parameter=c("slope","slope","slope","slope","slope","slope","slope","slope","slope","slope",
                     "intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept"
         ))

slopes.intercepts.untreatedset2<-slopes.intercepts.untreatedset2 %>% 
  mutate(IRR=exp(as.numeric(Estimate))) #Add in Incidence Rate Ratios (IRR)

slopes.intercepts.untreatedset2 <- slopes.intercepts.untreatedset2 %>% 
  mutate(Percent=(as.numeric(IRR-1))) #Add in Percentages (i.e., IRR-1 *100%)

#Save the combined table for supplemental material?
write_csv(slopes.intercepts.untreatedset2,'untreated set 2_contrast matrix results.csv')

untreatedset2.changeplot<-read.csv("untreatedset2_changeplot.csv")
untreatedset2.changeplot$type <- factor(untreatedset2.changeplot$type,levels=c('NOR','NEO','PER','UNR','CIR',
                                                                               'SEM','MIX','PHO','CNL','CTL'))

#these are predicted values from the model not the raw data!
untreatedset2_plot<-ggplot(untreatedset2.changeplot,aes(x=intercept,y=slope,label=type,color=type))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax = intercept.err.upper, xmin = intercept.err.lower))+
  #annotate("text", x=.56,y=0,labels=stringr::str_wrap(labels,width=20))+
  labs(title='Expected Change in Response Types in Untreated Set 2',x="Log Expected Counts at Baseline",y="Rate of Change",color='Type')+
  #geom_label(aes(fill=factor(type)),size=3,fontface="bold",colour="black",nudge_y = -.015)+
  geom_hline(yintercept=0)+
  ylim(-0.2,0.2)+
  scale_x_continuous(breaks = c(-1,0,1))+
  theme_classic()+
  theme(legend.position='none',plot.title=element_text(hjust=0.5),
        legend.title=element_text(size=10),plot.caption=(element_text(hjust=0,size=10)))

untreatedset2_plot

ggsave(filename='Fig 5B.Total Evolution of Untreated Set 2 Errors.tiff',plot=untreatedset2_plot,device='tiff',
       height=5,width=7.5,units='in',dpi=500)

##----------Untreated set 3 data----------##

untreatedset3.poisson.errors <- poisson.errors %>% 
  filter(set=='Control Words' & condition=='Untreated Language') %>% 
  droplevels()

###Untreated set 3 poisson regression

poissonmod_untreatedset3 <- glm(n~session*type,data=untreatedset3.poisson.errors,family=poisson(link='log'))
summary(poissonmod_untreatedset3)

mean(poissonmod_untreatedset3$fitted.values)
var(poissonmod_untreatedset3$fitted.values) #approx same but let's use negbin

negbinmod_untreatedset3 <- glm.nb(n~session*type,data=untreatedset3.poisson.errors)
lrtest(poissonmod_untreatedset3,negbinmod_untreatedset3) #negbin preferred

###Test predictions using contrast matrix
summary(glht(negbinmod_untreatedset3, contrast.matrix.slope),test=adjusted("none"))
summary(glht(negbinmod_untreatedset3, contrast.matrix.slope),test=adjusted("BH"))
summary(glht(negbinmod_untreatedset3, contrast.matrix.intercept),test=adjusted("BH"))
#Nothing survives correction

#Create separate tables for slopes and intercepts
slopes.untreatedset3<-table_glht(glht(negbinmod_untreatedset3, contrast.matrix.slope,test=adjusted("BH"))) #put slopes in a table
intercepts.untreatedset3<-table_glht(glht(negbinmod_untreatedset3, contrast.matrix.intercept,test=adjusted("BH"))) #put slopes in a table

#Bind the slope and intercept tables together
slopes.intercepts.untreatedset3<-data.frame(rbind(slopes.untreatedset3,intercepts.untreatedset3))

#Create a new column that labels the response type and whether each row is a slope or intercept
slopes.intercepts.untreatedset3<-slopes.intercepts.untreatedset3%>% 
  mutate(type=c("NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL","NOR","NEO","PER","UNR","CIR","SEM","MIX","PHO","CNL","CTL"),
         parameter=c("slope","slope","slope","slope","slope","slope","slope","slope","slope","slope",
                     "intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept","intercept"
         ))

slopes.intercepts.untreatedset3<-slopes.intercepts.untreatedset3 %>% 
  mutate(IRR=exp(as.numeric(Estimate))) #Add in Incidence Rate Ratios (IRR)

slopes.intercepts.untreatedset3 <- slopes.intercepts.untreatedset3 %>% 
  mutate(Percent=(as.numeric(IRR-1))) #Add in Percentages (i.e., IRR-1 *100%)

#Save the combined table for supplemental material?
write_csv(slopes.intercepts.untreatedset3,'untreated set 3_contrast matrix results.csv')

untreatedset3.changeplot<-read.csv("untreatedset3_changeplot.csv")
untreatedset3.changeplot$type <- factor(untreatedset3.changeplot$type,levels=c('NOR','NEO','PER','UNR','CIR',
                                                                               'SEM','MIX','PHO','CNL','CTL'))

#these are predicted values from the model not the raw data!
untreatedset3_plot<-ggplot(untreatedset3.changeplot,aes(x=intercept,y=slope,label=type,color=type))+
  geom_point(size=4)+
  geom_errorbarh(aes(xmax = intercept.err.upper, xmin = intercept.err.lower))+
  #annotate("text", x=.56,y=0,labels=stringr::str_wrap(labels,width=20))+
  labs(title='Expected Change in Response Types in Untreated Set 3',x="Log Expected Counts at Baseline",y="Rate of Change",color='Type')+
  #geom_label(aes(fill=factor(type)),size=3,fontface="bold",colour="black",nudge_y = -.015)+
  geom_hline(yintercept=0)+
  ylim(-0.2,0.2)+
  scale_x_continuous(breaks = c(-1,0,1))+
  theme_classic()+
  theme(legend.position='none',plot.title=element_text(hjust=0.5),
        legend.title=element_text(size=10),plot.caption=(element_text(hjust=0,size=10)))

untreatedset3_plot

ggsave(filename='Fig 5C.Total Evolution of Untreated Set 3 Errors.tiff',plot=untreatedset3_plot,device='tiff',
       height=5,width=7.5,units='in',dpi=500)

#Combined plot Figure 5
plot_legend_untreated <- get_legend(untreatedset3_plot+theme(legend.position='right')) 
plot_legend_untreated #can be used later

alluntreatedplots <- plot_grid(untreatedset1_plot+
                               annotate("text",x=0,y=0.18,label='Trained Translations',size=4)+
                               theme(legend.position="none")+labs(title='',x='',y=''),#+theme(plot.margin = unit(c(0,1,0,0), "cm")),
                             untreatedset2_plot+
                               annotate("text",x=0,y=0.18,label='Semantically Related Translations',size=4)+
                               theme(legend.position="none")+labs(title='',x='',y='Rate of Change'),#+theme(plot.margin = unit(c(0,1,0,0), "cm")),
                             untreatedset3_plot+
                               annotate("text",x=0,y=0.18,label='Control Translations',size=4)+
                               theme(legend.position="none")+labs(title='',x='Log Expected Counts at Baseline',y=''),#+theme(plot.margin = unit(c(0,1,0,0), "cm")),
                             align = 'v',
                             labels = c("A", "B", "C"),
                             hjust = -4.5,
                             ncol = 1
)

combined.untreatedplots<- plot_grid(alluntreatedplots, plot_legend_untreated, ncol = 2,rel_widths = c(1,0.5))
combined.untreatedplots

ggsave(filename='Fig5.Evolution of Errors in Unreated Language.tiff',plot=combined.untreatedplots,device='tiff',
       height=8.2,width=8.2,units='in',dpi=500)
