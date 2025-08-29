library(tidyverse)
library(tidylog)
library(naniar)
IPUMS <- read.csv("~/Desktop/Projects/Disparities_Hisp_NHI/final_jan_2023_2.csv")

#NHIS recommends dividing the sampling weights by the number of years pooled (2014-2018)
IPUMS$SAMPWEIGHTAdj <- (IPUMS$SAMPWEIGHT)/5

#sex
IPUMS <- IPUMS %>% replace_with_na(replace = list(SEX = c(7,8,9)))
IPUMS$SexMale <- ifelse(IPUMS$SEX == 1,1,0)

#ANY activity limitation
IPUMS <- IPUMS %>% replace_with_na(replace = list(LANY = c(22)))
IPUMS$LANY_Bin <- ifelse(IPUMS$LANY == 10,1,0)

#ANY functional limitation
IPUMS <- IPUMS %>% replace_with_na(replace = list(LANY = c(22)))
IPUMS$LANY_Bin <- ifelse(IPUMS$LANY == 10,1,0)

#done in order of ipums report sheet
#membermember with a working cell phone
IPUMS <- IPUMS %>% replace_with_na(replace = list(TELCEL = c(7,8,9)))
IPUMS$CellPhone <- ifelse(IPUMS$TELCEL == 2,1,0)
#age
IPUMS <- IPUMS %>% replace_with_na(replace = list(AGE = c(997, 998, 999)))

#marital status; 1 = married (any kind) or living with partner
IPUMS <- IPUMS %>% replace_with_na(replace = list(MARSTCOHAB = c(0,9)))
IPUMS$MStLivPtnr <- ifelse(IPUMS$MARSTCOHAB == 1 | IPUMS$MARSTCOHAB == 2 | IPUMS$MARSTCOHAB == 3 | IPUMS$MARSTCOHAB == 7, 1, 0)

#married_better
IPUMS <- IPUMS %>% replace_with_na(replace = list(MARSTAT = c(00,99)))
IPUMS$Married <- ifelse(IPUMS$MARSTAT == 10 | IPUMS$MARSTAT == 11 | IPUMS$MARSTAT == 12 | IPUMS$MARSTAT == 13, 1, 0)


#family size
IPUMS <- IPUMS %>% replace_with_na(replace = list(FAMSIZE = c(98, 99)))

#hispanic ethnicity
IPUMS <- IPUMS %>% replace_with_na(replace = list(HISPETH = c(90, 91, 92, 93, 99)))
IPUMS$Hispanic <- ifelse(IPUMS$HISPETH != 10, 1, 0)

#years in US -- did not do

#citizen
IPUMS <- IPUMS %>% replace_with_na(replace = list(CITIZEN = c(7, 8, 9)))
IPUMS$Cit <- ifelse(IPUMS$CITIZEN == 2, 1, 0)

#race -- we only want to compare to non-hispanic whites, so we just want white versus non-white
IPUMS <- IPUMS %>% replace_with_na(replace = list(RACENEW = c(997, 998, 999)))
IPUMS$White <- ifelse(IPUMS$RACENEW == 100, 1, 0)

#language of interview not looked at

#language spoken not looked at

#educational attainment - HS or higher
IPUMS <- IPUMS %>% replace_with_na(replace = list(EDUC = c(000, 996, 997, 998, 999)))
IPUMS$Education <- ifelse(IPUMS$EDUC == 200 | IPUMS$EDUC == 201 | IPUMS$EDUC == 202 | IPUMS$EDUC == 300 | 
                            IPUMS$EDUC == 301 | IPUMS$EDUC == 302 | IPUMS$EDUC == 303 | IPUMS$EDUC == 400 |
                            IPUMS$EDUC == 500 | IPUMS$EDUC == 501 | IPUMS$EDUC == 502 | IPUMS$EDUC == 503 | IPUMS$EDUC == 504, 1, 0)

#poverty - will not look at yet

#BMI
IPUMS <- IPUMS %>% replace_with_na(replace = list(BMICALC = c(0.0, 996)))

#ED/ER usage; at least one visit in the last year versus no visits
IPUMS <- IPUMS %>% replace_with_na(replace = list(ERYRNO = c(00, 97, 98, 99)))
IPUMS$ERUse <- ifelse(IPUMS$ERYRNO !=10, 1, 0)

#had surgery in past 12 months
IPUMS <- IPUMS %>% replace_with_na(replace = list(SURGERYR = c(0,7,8,9)))
IPUMS$SurgeryYorN12Mos <- ifelse(IPUMS$SURGERYR ==2, 1, 0)

#total number of surgeries over last 12 months -- incomplete as turns into y/n -- see Y or N variable

#office visits -> transformed to y/n
IPUMS <- IPUMS %>% replace_with_na(replace = list(VISITYRNO = c(96,97,98,99)))
IPUMS$VisitYN <- ifelse(IPUMS$VISITYRNO != 00, 1, 0)

#usual place for care
IPUMS <- IPUMS %>% replace_with_na(replace = list(USUALPL = c(0, 7,8,9)))
IPUMS$UsualPlace <- ifelse(IPUMS$USUALPL != 1, 1, 0)

#not looked at type of place

#not looked at same place for routine care & sick care

#not looking what where people are going

#delayed care bc couldn't get an appointment
IPUMS <- IPUMS %>% replace_with_na(replace = list(DELAYAPPT = c(0, 7,8,9)))
IPUMS$DelAppt <- ifelse(IPUMS$DELAYAPPT == 2, 1, 0)

#no usual source of care being -> speak a different language
IPUMS <- IPUMS %>% replace_with_na(replace = list(NOUSLYLANG = c(0, 7,8,9)))
IPUMS$NoCareBcLang <- ifelse(IPUMS$NOUSLYLANG == 2, 1, 0)

#prescribed medication in the last 12 months
IPUMS <- IPUMS %>% replace_with_na(replace = list(PRESMED12MO = c(0, 7, 8, 9)))
IPUMS$RxPres <- ifelse(IPUMS$PRESMED12MO == 2, 1, 0)

#high cholesterol over last 12 months
IPUMS <- IPUMS %>% replace_with_na(replace = list(CHOLHIGHYR = c(0, 7, 8, 9)))
IPUMS$CholDx <- ifelse(IPUMS$CHOLHIGHYR == 2, 1, 0)

#diabetic -- ever?
IPUMS <- IPUMS %>% replace_with_na(replace = list(DIABETICEV = c(0, 7, 8, 9)))
IPUMS$Db <- ifelse(IPUMS$DIABETICEV == 2, 1, 0)


#hypertension
IPUMS <- IPUMS %>% replace_with_na(replace = list(HYPERTENYR = c(0, 7, 8, 9)))
IPUMS$Htn <- ifelse(IPUMS$HYPERTENYR == 2, 1, 0)

#stroke diagnosis
IPUMS <- IPUMS %>% replace_with_na(replace = list(STROKEV = c(0, 7, 8, 9)))
IPUMS$Stroke <- ifelse(IPUMS$STROKEV == 2, 1, 0)

#had chol CHECKED in last 12 months
IPUMS <- IPUMS %>% replace_with_na(replace = list(CHOLCHEK1YR = c(0, 7, 8, 9)))
IPUMS$Chol <- ifelse(IPUMS$CHOLCHEK1YR == 2, 1, 0)

#bp CHECKED in last 12 months
IPUMS <- IPUMS %>% replace_with_na(replace = list(HYPCHEK1YR = c(0, 7, 8, 9)))
IPUMS$BP <- ifelse(IPUMS$HYPCHEK1YR == 2, 1, 0)

#days had 5+ drinks
IPUMS <- IPUMS %>% replace_with_na(replace = list(ALC5UPYR = c(995, 996, 997, 998, 999)))

#smoker status
IPUMS <- IPUMS %>% replace_with_na(replace = list(SMOKESTATUS2 = c(00, 40, 90)))
IPUMS$Smoker <- ifelse(IPUMS$SMOKESTATUS2 == 10 | IPUMS$SMOKESTATUS2 == 11 | IPUMS$SMOKESTATUS2 == 12 | IPUMS$SMOKESTATUS2 == 13, 1, 0)

#hours of sleep
IPUMS <- IPUMS %>% replace_with_na(replace = list(HRSLEEP = c(00, 25, 97, 98, 99, 99)))

#need help wit ADLs due to physical mental or emotional problem
IPUMS <- IPUMS %>% replace_with_na(replace = list(LADL = c(0, 7, 8, 9)))
IPUMS$ADL <- ifelse(IPUMS$LADL == 2, 1, 0)

#need help with INSTRUMENTAL activities, such as chores, business, shopping
IPUMS <- IPUMS %>% replace_with_na(replace = list(LAIADL = c(0, 7, 8, 9)))
IPUMS$LAIADL2 <- ifelse(IPUMS$LAIADL == 2, 1, 0)

#limited activity by chronic condition
IPUMS <- IPUMS %>% replace_with_na(replace = list(CLIMCHRONIC1 = c(3)))
IPUMS$LimByChronDx <- ifelse(IPUMS$CLIMCHRONIC1 == 1,1,0)

#difficulty communicating in their usual language -> y or no
IPUMS <- IPUMS %>% replace_with_na(replace = list(LACOMDIFEGO = c(0,7,8,9)))
IPUMS$ComDiff <- ifelse(IPUMS$LACOMDIFEGO != 1,1,0)

#a speech problem is causing an activity limitation
IPUMS <- IPUMS %>% replace_with_na(replace = list(CLIMSPEECH = c(0,6,7,8,9)))
IPUMS$SpeechProblem <- ifelse(IPUMS$CLIMSPEECH == 2,1,0)

#chronic status of speech limiting problem (>3 mos)
IPUMS <- IPUMS %>% replace_with_na(replace = list(CLIMSPEECHC = c(0, 9)))
IPUMS$SpeechProblemChronicity <- ifelse(IPUMS$CLIMSPEECHC == 2,1,0)

#climspeech months, how many months, excluding for now

#climspeech no, in number of units, also excluding

#climspeech time period, will leave as well

#clim speech in years, will leave

#activity limitation from stroke
IPUMS <- IPUMS %>% replace_with_na(replace = list(CLIMSTROKE = c(0,6,7,8,9)))
IPUMS$ChronStroke <- ifelse(IPUMS$CLIMSTROKE == 2,1,0)

#functional limitations due to strokIPUMS <- IPUMS %>% replace_with_na(replace = list(CLIMSTROKE = c(0,6,7,8,9)))
IPUMS <- IPUMS %>% replace_with_na(replace = list(FLSTROKE = c(0,6,7,8,9)))
IPUMS$FnLimStroke <- ifelse(IPUMS$FLSTROKE == 2,1,0)

#flstroke in years, ignoring

#hopelessness -> y/n
IPUMS <- IPUMS %>% replace_with_na(replace = list(AHOPELESS = c(6,7,8,9)))
IPUMS$Hopelessness <- ifelse(IPUMS$AHOPELESS != 0,1,0)

#sadness -> y/n
IPUMS <- IPUMS %>% replace_with_na(replace = list(ASAD = c(6,7,8,9)))
IPUMS$Sadness <- ifelse(IPUMS$ASAD != 0,1,0)

#worthlessness -> y/n
IPUMS <- IPUMS %>% replace_with_na(replace = list(AWORTHLESS = c(6,7,8,9)))
IPUMS$Worthless <- ifelse(IPUMS$AWORTHLESS != 0,1,0)

IPUMS <- IPUMS %>% replace_with_na(replace = list(CITIZEN = c(7, 8, 9)))

#regional
IPUMS <- IPUMS %>% replace_with_na(replace = list(REGION = c(08,09)))
IPUMS$South <- ifelse(IPUMS$REGION == 03, 1, 0)

#coronary heart disease, ever
IPUMS <- IPUMS %>% replace_with_na(replace = list(CHEARTDIEV = c(0,7,8,9)))
IPUMS$CHD <- ifelse(IPUMS$CHEARTDIEV == 2, 1, 0)

####INTERNET USE
IPUMS <- IPUMS %>% replace_with_na(replace = list(PCRXFILLYR = c(0,7,8,9)))
IPUMS$PCRX <- ifelse(IPUMS$PCRXFILLYR == 2, 1, 0)

#comm via email
IPUMS <- IPUMS %>% replace_with_na(replace = list(PCEMAILHPYR = c(0,7,8,9)))
IPUMS$Email <- ifelse(IPUMS$PCEMAILHPYR== 2, 1, 0)

#web use
IPUMS <- IPUMS %>% replace_with_na(replace = list(WEBUSE = c(0,7,8,9)))
IPUMS$WebUse <- ifelse(IPUMS$WEBUSE== 2, 1, 0)

#stroke knowledge sudden confusion a symptom
IPUMS <- IPUMS %>% replace_with_na(replace = list(SKCONFUSE = c(0,7,8,9)))
IPUMS$Confuse <- ifelse(IPUMS$SKCONFUSE== 2, 1, 0)

#stroke knowledge sudden headache a symptom
IPUMS <- IPUMS %>% replace_with_na(replace = list(SKHEADACHE = c(0,7,8,9)))
IPUMS$Headache <- ifelse(IPUMS$SKHEADACHE== 2, 1, 0)

#stroke knowledge sudden numb face a symptom
IPUMS <- IPUMS %>% replace_with_na(replace = list(SKNUMBFACE = c(0,7,8,9)))
IPUMS$Numb <- ifelse(IPUMS$SKNUMBFACE== 2, 1, 0)

#stroke knowledge sudden trouble seeing a symptom
IPUMS <- IPUMS %>% replace_with_na(replace = list(SKSEEING = c(0,7,8,9)))
IPUMS$Seeing <- ifelse(IPUMS$SKSEEING== 2, 1, 0)

#stroke knowledge sudden trouble seeing a symptom
IPUMS <- IPUMS %>% replace_with_na(replace = list(SKWALKING = c(0,7,8,9)))
IPUMS$Walking <- ifelse(IPUMS$SKWALKING== 2, 1, 0)

#knowledgable proxy
IPUMS <- IPUMS %>% replace_with_na(replace = list(SAPROXYAVAIL = c(0)))
IPUMS$SAPROXYAVAIL <- ifelse(IPUMS$SAPROXYAVAIL== 2, 1, 0)
### POVERTY
IPUMS$PovertyAdj <- ifelse(IPUMS$POVERTY == 11 | IPUMS$POVERTY == 12 | IPUMS$POVERTY == 13 | IPUMS$POVERTY == 14 ,"<100%",IPUMS$POVERTY)
IPUMS$PovertyAdj <- ifelse(IPUMS$POVERTY == "20" | IPUMS$POVERTY == "21" | IPUMS$POVERTY == "22" | IPUMS$POVERTY == "23" | IPUMS$POVERTY == "24" |IPUMS$POVERTY == "25" ,">=100% & <200%",IPUMS$PovertyAdj)
IPUMS$PovertyAdj <- ifelse(IPUMS$POVERTY == "30" | IPUMS$POVERTY == "31" | IPUMS$POVERTY == "32" | IPUMS$POVERTY == "33" | IPUMS$POVERTY == "34",">=200% & <400%",IPUMS$PovertyAdj)
IPUMS$PovertyAdj <- ifelse(IPUMS$POVERTY == "35" | IPUMS$POVERTY == "36" | IPUMS$POVERTY == "37",">=400%",IPUMS$PovertyAdj)
IPUMS <- IPUMS %>% replace_with_na(replace = list(PovertyAdj = c(38,98,99)))
(IPUMS$PovertyAdj)

IPUMS$Poverty <- IPUMS$PovertyAdj
levels(IPUMS$Poverty)
IPUMS$Poverty <- factor(IPUMS$Poverty)
IPUMS$Poverty <- relevel(IPUMS$Poverty, ref = ">=400%")

#saw neurologist
IPUMS <- IPUMS %>% replace_with_na(replace = list(EPILMDYR = c(0,7,8,9)))
IPUMS$EPILMDYR <- ifelse(IPUMS$EPILMDYR== 2, 1, 0)

#delayed cost
IPUMS <- IPUMS %>% replace_with_na(replace = list(DELAYCOST = c(0,7,8,9)))
IPUMS$DELAYCOST <- ifelse(IPUMS$DELAYCOST== 2, 1, 0)

#saw gen practicioner
IPUMS <- IPUMS %>% replace_with_na(replace = list(SAWGEN = c(0,7,8,9)))
IPUMS$SawGen <- ifelse(IPUMS$SAWGEN== 2, 1, 0)

#health status
IPUMS <- IPUMS %>% replace_with_na(replace = list(HEALTH = c(7,8,9)))
IPUMS$HEALTH_Binary <- ifelse(IPUMS$HEALTH== 1 | IPUMS$HEALTH==2 | IPUMS$HEALTH==3, 1, 0)

##new 2023 jan 4
#same place for sick care as routine care
IPUMS <- IPUMS %>% replace_with_na(replace = list(ROUTCARE = c(0, 7,8,9)))
IPUMS$SamePlaceRoutineCare <- ifelse(IPUMS$ROUTCARE == 2, 1, 0)

#change in usual place of care for insurance
IPUMS <- IPUMS %>% replace_with_na(replace = list(CHANGEIN = c(0, 7,8,9)))
IPUMS$ChangedCare <- ifelse(IPUMS$CHANGEIN == 2, 1, 0)

#delay in care due to hours
IPUMS <- IPUMS %>% replace_with_na(replace = list(DELAYHRS = c(0, 7,8,9)))
IPUMS$Deldue2Hrs <- ifelse(IPUMS$DELAYHRS == 2, 1, 0)

#del due to couldnt get through on phone
IPUMS <- IPUMS %>% replace_with_na(replace = list(DELAYPHONE = c(0, 7,8,9)))
IPUMS$DelPhone <- ifelse(IPUMS$DELAYPHONE == 2, 1, 0)

#del due to transportation
IPUMS <- IPUMS %>% replace_with_na(replace = list(DELAYTRANS = c(0, 7,8,9)))
IPUMS$DelTrans <- ifelse(IPUMS$DELAYTRANS == 2, 1, 0)

#needed but couldn't afford care 
IPUMS <- IPUMS %>% replace_with_na(replace = list(YBARCARE = c(0, 7,8,9)))
IPUMS$Notaffordcare <- ifelse(IPUMS$YBARCARE == 2, 1, 0)

#needed but couldn't afford follow up care 
IPUMS <- IPUMS %>% replace_with_na(replace = list(YBARFOLLOW = c(0, 7,8,9)))
IPUMS$AffordFollowup <- ifelse(IPUMS$YBARFOLLOW == 2, 1, 0)


#problems paying medical bills
IPUMS <- IPUMS %>% replace_with_na(replace = list(HIPROBPAYR = c(0, 7,8,9)))
IPUMS$ProbsPaying <- ifelse(IPUMS$HIPROBPAYR == 2, 1, 0)

#medical care satisfication
IPUMS <- IPUMS %>% replace_with_na(replace = list(HCSATIS12M = c(0, 5,7,8,9)))
IPUMS$Satisfaction <- ifelse(IPUMS$HCSATIS12M == 1 & 2, 1, 0)

#medical care satisfication
IPUMS <- IPUMS %>% replace_with_na(replace = list(HINONE = c(7,8,9)))
IPUMS$Insured <- ifelse(IPUMS$HINONE == 2, 1, 0)

#poverty level
IPUMS <- IPUMS %>% replace_with_na(replace = list(POORYN = c(9)))
IPUMS$PovertyLevel <- ifelse(IPUMS$POORYN == 2, 1, 0)

#del due to wait
#del due to couldnt get through on phone
IPUMS <- IPUMS %>% replace_with_na(replace = list(DELAYWAIT = c(0, 7,8,9)))
IPUMS$DelWait <- ifelse(IPUMS$DELAYWAIT == 2, 1, 0)

IPUMS$DelayInCare <- ifelse(IPUMS$DELAYCOST == 1 | IPUMS$Deldue2Hrs == 1 | IPUMS$DelPhone == 1 |
                              IPUMS$DelWait == 1 | IPUMS$DelTrans == 1 | IPUMS$DelAppt == 1, 1, 0)

#SDOH score
IPUMS$SDOHScore <- 0

#reorienting variables for addition ==1 will add a 1
IPUMS$CitFlip <- ifelse(IPUMS$Cit==1, 0,1)
IPUMS$EduFlip <- ifelse(IPUMS$Education==1, 0,1)
IPUMS$WebFlip <- ifelse(IPUMS$WebUse==1, 0,1)

IPUMS$SDOHScore <- (IPUMS$PovertyLevel + IPUMS$DelayInCare + 
                      IPUMS$EduFlip + IPUMS$CitFlip + 
                      IPUMS$WebFlip)
