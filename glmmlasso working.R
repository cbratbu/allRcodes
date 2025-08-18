################## First Simple Method ############################################
## Using BIC (or AIC, respectively) to determine the optimal tuning parameter lambda
L1Perf$Accuracy <- as.integer(Accuracy)

lambda <- seq(500,0,by=-5)

family = binomial(link = logit)

BIC_vec<-rep(Inf,length(lambda))

## first fit good starting model
library(MASS);library(nlme)
PQL<-glmmPQL(Accuracy~1,random = list(~1|Patient, ~1|Item) ,family=family,data=L1Perf)
Delta.start<-c(as.numeric(PQL$coef$fixed),rep(0,6),as.numeric(c(t(PQL$coef$random$Patient, PQL$coef$random$Item)))
Q.start<-as.numeric(VarCorr(PQL)[1,1])

for(j in 1:length(lambda))
{
  print(paste("Iteration ", j,sep=""))
  
  glm1 <- try(glmmLasso(Accuracy ~ (L1AQ.s + L2AQ.s +
                          ItemDifficulty.s + Overlap.s +
                          L1Background.s +  
                             L1Use.s + 
                             L1Environment.s + 
                             L2BackgroundEnvironment.s + 
                             L2Use.s)^2, rnd = list(Patient=~1, Item=~1),  
                        family = family, data = L1Perf, lambda=lambda[j],switch.NR=T,final.re=TRUE), silent=TRUE)  
  
  
  if(class(glm1)!="try-error")
  {  
    BIC_vec[j]<-glm1$bic
  }
  
}

opt<-which.min(BIC_vec)
glm1_final <- glmmLasso(Accuracy ~ (L1AQ.s + L2AQ.s +
                                      ItemDifficulty.s + Overlap.s +
                                      L1Background.s +  
                                      L1Use.s + 
                                      L1Environment.s + 
                                      L2BackgroundEnvironment.s + 
                                      L2Use.s)^2, rnd = list(Patient=~1, Item=~1),
family = family, data = L1Perf, lambda=lambda[opt],switch.NR=F,final.re=TRUE)

summary(glm1_final)

glm1_final$bic
