#flagging NHWs
IPUMS$NHWFlag <- ifelse(IPUMS$Hispanic==0 & IPUMS$Stroke ==0 & IPUMS$White == 1, 0,NA)

sum(IPUMS$HealthySubset==1 & IPUMS$Hispanic == 1, na.rm=T)

IPUMS$HispNoStrokeFlag <- ifelse(IPUMS$Hispanic==1 & IPUMS$Stroke ==0, 1,0)

IPUMS$HispFlag <- ifelse(IPUMS$Hispanic == 1 & IPUMS$Stroke == 0, 1,NA)

#propensityscorematched comparisons
hispmatch$HispFlag==1 #-≥ yes
hispmatch$StrokeHisp == 1 #-> no

sum(hispmatch$StrokeNWH==0, na.rm = T)
sum(hispmatch$NHWFlag==0, na.rm = T)

hispmatch$NHWFlag <- ifelse(hispmatch$NHWFlag == 0, 1,NA)

sum(hispmatch$NHWFlag==1, na.rm=T)

hispmatch$HispFlag
hispmatch$NHWMerge <- paste(hispmatch$NHWFlag, hispmatch$StrokeNWH, sep="_")
sum(hispmatch$NHWMerge == 0, na.rm=T)
hispmatch$NHWMerge
##merging for nHw
hispmatch$NHWMerge[hispmatch$NHWMerge == "NA_NA"] <- NA
hispmatch$NHWMerge[hispmatch$NHWMerge == "NA_0"] <- 0
hispmatch$NHWMerge[hispmatch$NHWMerge == "0_0"] <- 0
hispmatch$NHWMerge[hispmatch$NHWMerge == "1_NA"] <- 1
hispmatch$NHWMerge[hispmatch$NHWMerge == "0_1"] <- NA
hispmatch$NHWMerge[hispmatch$NHWMerge == "NA_1"] <- NA
hispmatch$NHWMerge[hispmatch$NHWMerge == "1_1"] <- 1
hispmatch$NHWMerge[hispmatch$NHWMerge == "NA_1"] <- 0
hispmatch$NHWMerge[hispmatch$NHWMerge == "1"] <- 1
####

hispmatch$HispMerge <- paste(hispmatch$HispFlag, hispmatch$StrokeHisp, sep="_")
hispmatch$HispMerge[hispmatch$HispMerge == "NA_NA"] <- NA
##1 == Hispanicw htout stroke, 0 == hispanic with stroke
hispmatch$HispMerge[hispmatch$HispMerge == "1_NA"] <- 1
hispmatch$HispMerge[hispmatch$HispMerge == "1_0"] <- 1
hispmatch$HispMerge[hispmatch$HispMerge == "NA_1"] <- 0
hispmatch$HispMerge[hispmatch$HispMerge == "0_1"] <- 0
hispmatch$HispMerge[hispmatch$HispMerge == "0_0"] <- NA
hispmatch$HispMerge[hispmatch$HispMerge == "NA_0"] <- NA
sum(hispmatch$StrokeHisp == 1, na.rm=T)


sum(IPUMS$NHWFlag == 1, na.rm = T)

IPUMS$MergedFlag <- paste(IPUMS$HispFlag, IPUMS$NHWFlag, sep="_")

IPUMS$MergedFlag[IPUMS$MergedFlag == "NA_NA"] <- NA
IPUMS$MergedFlag[IPUMS$MergedFlag == "1_NA"] <- 1
IPUMS$MergedFlag[IPUMS$MergedFlag == "NA_0"] <- 0
IPUMS$MergedFlag[IPUMS$MergedFlag == "0_0"] <- 0

sum(IPUMS$MergedFlag == 0, na.rm = T)
IPUMS$MergedFlag <- as.numeric(IPUMS$MergedFlag)
IPUMS$MergedFlag
IPUMS$HispFlag <- IPUMS$MergedFlag

sum(IPUMS$HispFlag == 1, na.rm=T)

IPUMS$HealthySubset <- ifelse(IPUMS$HispFlag ==0 | IPUMS$HispFlag ==1, "HealthySubset", NA)
IPUMS$HealthySubset <- ifelse(IPUMS$HealthySubset == "HealthySubset", 1, 0)
sum(IPUMS$HealthySubset == "HealthySubset", na.rm = T)
sum(IPUMS$HealthySubset == "HealthySubset", na.rm = T)
#####

IPUMS$StrokeNWH <- ifelse(IPUMS$Hispanic==0 & IPUMS$Stroke ==1 & IPUMS$White == 1, 0,NA)

### stroke group
sum(IPUMS$StrokeNWH==0, na.rm=T)
sum(IPUMS$StrokeHisp == 1, na.rm = T)

###healthy groups


IPUMS$StrokeHisp <- ifelse(IPUMS$Hispanic == 1 & IPUMS$Stroke == 1, 1,NA)
sum(IPUMS$StrokeHisp == 1, na.rm = T)

IPUMS$MergedStrokeFlag <- paste(IPUMS$StrokeHisp, IPUMS$StrokeNWH, sep="_")

IPUMS$MergedStrokeFlag[IPUMS$MergedStrokeFlag == "NA_NA"] <- NA
IPUMS$MergedStrokeFlag[IPUMS$MergedStrokeFlag == "1_NA"] <- 1
IPUMS$MergedStrokeFlag[IPUMS$MergedStrokeFlag == "NA_0"] <- 0
IPUMS$MergedStrokeFlag[IPUMS$MergedStrokeFlag == "0_0"] <- 0

sum(IPUMS$MergedStrokeFlag == "0", na.rm = T)
IPUMS$MergedStrokeFlag <- as.numeric(IPUMS$MergedStrokeFlag)
IPUMS$MergedStrokeFlag
IPUMS$HispStrokeFlag <- IPUMS$MergedStrokeFlag
sum(IPUMS$HispStrokeFlag == 0, na.rm=T)

IPUMS$StrokeSubset <- ifelse(IPUMS$HispStrokeFlag ==0 | IPUMS$HispStrokeFlag ==1, "Stroke", NA)
IPUMS$StrokeSubset <- ifelse(IPUMS$StrokeSubset =="Stroke", 1, 0)
IPUMS$StrokeSubsetGLM <- ifelse(IPUMS$HispStrokeFlag ==0 | IPUMS$HispStrokeFlag ==1, 1, 0)
IPUMS$StrokeHisp[is.na(IPUMS$StrokeHisp)] <- 0
IPUMS$StrokeNWH[is.na(IPUMS$StrokeNWH)] <- 1
### ALL group

IPUMS$AllSubset <- ifelse(IPUMS$HealthySubset == "HealthySubset" | IPUMS$StrokeSubset == "Stroke", 1, 0)
IPUMS$AllSubset[is.na(IPUMS$AllSubset)] <- 0
IPUMS$HealthySubset[is.na(IPUMS$HealthySubset)] <- 0
IPUMS$StrokeSubset[is.na(IPUMS$StrokeSubset)] <- 0

#for GLM
IPUMS_glm[(IPUMS$StrokeSubset==1),]
IPUMS_glm$StrokeSubset[is.na(IPUMS$StrokeSubset)] <- 0
sum(IPUMS_glm$StrokeSubset == 1, na.rm = F)
sum(IPUMS$NHWFlag == 1, na.rm = T)
