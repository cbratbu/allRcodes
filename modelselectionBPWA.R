L1Freq.1 <- glmer(Accuracy ~ L2AQ.s+ 
                Freq.s +
                L1Background.s + L1Use.s+
                L1Environment.s + L2Use.s +
                L1Use.s + L2BackgroundEnvironment.s +
                L1Background.s:L1Use.s+
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s +
                (1|Patient)+(1|Item),
              data=L1Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1Freq.3)
L1Freq.2 <- glmer(Accuracy ~ L2AQ.s+ 
                    Freq.s + Overlap.s+
                    L1Background.s +
                    L1Environment.s + L2Use.s +
                    L1Use.s + nnL2BackgroundEnvironment.s +
                    L1Background.s:L1Use.s+
                    L1Background.s:L2Use.s+
                    L1Use.s:L1Environment.s +
                    L1Use.s:nnL2BackgroundEnvironment.s+
                    L2Use.s:nnL2BackgroundEnvironment.s+
                    (1|Patient)+(1|Item),
                  data=L1Perf,family=binomial(link=logit),
                  control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L1Freq.1)
L1Freq1.1 <- drop1(L1Freq.1, test = "Chisq")
L1Freq1.1

L2.2 <- glmer(Accuracy ~ L2AQ.s+
                ItemDifficulty.s+
                   Overlap.s+
                   L1Background.s+
                   L1Use.s+
                   L1Environment.s+
                   L2BackgroundEnvironment.s+
                   L2Use.s+
                L1Background.s:L1Use.s+
                L1Background.s:L2Use.s+
                L1Use.s:L1Environment.s+
                Overlap.s:L2BackgroundEnvironment.s+
                (1|Patient)+(1|Item),
              data=L2Perf,family=binomial(link=logit),
              control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
summary(L2.3)

L2.1drop <- drop1(L2.1, test = "Chisq")
L2.1drop

compare_performance(L2.1, L2.2, rank = TRUE)
testDispersion(L2.2)
simulateResiduals(fittedModel = L2.2, plot = T)
vif(L2.2)




L2.3Freq <- glmer(Accuracy ~ L2AQ.s + 
                    Freq.s+
                       L1Background.s +  
                       L1Use.s + 
                       L1Environment.s + 
                       L2BackgroundEnvironment.s + 
                       L2Use.s+
                    L1Background.s:L1Use.s +
                    L1Background.s:L2Use.s +
                    L1Use.s:L1Environment.s+
                    (1|Patient)+(1|Item),
                  data=L2Perf,family=binomial(link=logit),
                  control=glmerControl(optimizer='bobyqa',optCtrl=list(maxfun=2e5)))
L2.1FreqStep <- drop1(L2.1Freq, test = "Chisq")
L2.1FreqStep
summary(L2.3Freq)
compare_performance(L2.1Freq, L2.3Freq, rank = TRUE)
testDispersion(L2.3Freq)
simulateResiduals(fittedModel = L2.3Freq, plot = T)
vif(L2.3Freq)
