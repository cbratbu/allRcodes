IPUMS_glm2$SDOHScore <- 0

#reorienting variables for addition ==1 will add a 1
IPUMS_glm2$CitFlip <- ifelse(IPUMS_glm2$Cit==1, 0,1)
IPUMS_glm2$EduFlip <- ifelse(IPUMS_glm2$Education==1, 0,1)
IPUMS_glm2$WebFlip <- ifelse(IPUMS_glm2$WebUse==1, 0,1)

IPUMS_glm2$SDOHScore <- (IPUMS_glm2$PovertyLevel + IPUMS_glm2$DelayInCare + 
                           IPUMS_glm2$EduFlip + IPUMS_glm2$CitFlip + 
                           IPUMS_glm2$WebFlip)



glm_subset <- subset(IPUMS_glm, StrokeSubset == 1)$SDOHScore
hist(glm_subset)

IPUMS2 %>% left_join(IPUMS,IPUMS_glm)

IPUMS_glm2$SDOHScore <- 0
IPUMS_glm2$SDOHScore <- (IPUMS_glm2$PovertyLevel + IPUMS_glm2$DelayInCare + 
                           IPUMS_glm2$EduFlip + IPUMS_glm2$CitFlip + 
                           IPUMS_glm2$WebFlip)

IPUMS_glm2$FactorSDOHScore <- as.factor(IPUMS_glm2$SDOHScore)
IPUMS_glm2$dichSDOHScore2 <- ifelse(IPUMS_glm2$SDOHScore >= 1, 1, 0)
