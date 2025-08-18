library(xlsx)
PMG=read.xlsx(file.choose(),1,header=TRUE)
cor.test(PMG$PMG, PMG$SC, method="spearman")
cor.test(PMG$PMG, PMG$ST, method="spearman")
cor.test(PMG$PMG,PMG$DM, method="spearman")
cor.test(PMG$PMG,PMG$M, method="spearman")
cor.test(PMG$PMG, PMG$DG,method="spearman")
cor.test(PMG$PMG, PMG$GCTI, method="spearman")
cor.test(PMG$PMG, PMG$GCTFM,method="spearman")
cor.test(PMG$PMG, PMG$DSF, method="spearman")
cor.test(PMG$PMG, PMG$DSB,method="spearman")
cor.test(PMG$PMG, PMG$DPT, method="spearman")
cor.test(PMG$PMG, PMG$RCPM,method="spearman")
cor.test(PMG$PMG, PMG$CMS, method="spearman")
cor.test(PMG$PMG, PMG$SRTTL, method="spearman")
cor.test(PMG$PMG, PMG$Block, method="spearman")
cor.test(PMG$PMG, PMG$PAPT..,method="spearman")



PMGM=read.xlsx(file.choose(),1,header=TRUE)
cor.test(PMGM$PMGM, PMGM$SC, method="spearman")
cor.test(PMGM$PMGM, PMGM$ST, method="spearman")
cor.test(PMGM$PMGM,PMGM$DM, method="spearman")
cor.test(PMGM$PMGM,PMGM$M, method="spearman")
cor.test(PMGM$PMGM, PMGM$DG,method="spearman")
cor.test(PMGM$PMGM, PMGM$GCTI, method="spearman")
cor.test(PMGM$PMGM, PMGM$GCTFM,method="spearman")
cor.test(PMGM$PMGM, PMGM$DSF, method="spearman")
cor.test(PMGM$PMGM, PMGM$DSB,method="spearman")
cor.test(PMGM$PMGM, PMGM$DPT, method="spearman")
cor.test(PMGM$PMGM, PMGM$RCPM,method="spearman")
cor.test(PMGM$PMGM, PMGM$CMS, method="spearman")
cor.test(PMGM$PMGM, PMGM$SRTTL, method="spearman")
cor.test(PMGM$PMGM, PMGM$Block, method="spearman")
cor.test(PMGM$PMGM, PMGM$PAPT..,method="spearman")


pPMG=c(0.04866,
       0.02266,
       0.007287,
       0.4111,
       0.04998,
       0.04868,
       0.01357,
       0.0004712,
       0.002926,
       0.3801,
       0.5811,
       0.05162,
       0.4042,
       0.04537,
       0.0007876)
BONF=p.adjust(pPMG,"bonferroni")
BH=p.adjust(pPMG,"BH")
FDR=p.adjust(pPMG,"fdr")
res=cbind(pPMGM, BONF=round(BONF,3),BH=round(BH,3), FDR=round(FDR,3))


pPMGM=c(0.006192,
        0.02549,
        0.0005633,
        0.7821,
        0.03158,
        0.05577,
        0.01337,
        0.0001729,
        0.004909,
        0.5513,
        0.5462,
        0.1716,
        0.7804,
        0.02102,
        9.51E-05
)
BONF=p.adjust(pPMGM,"bonferroni")
BH=p.adjust(pPMGM,"BH")
FDR=p.adjust(pPMGM,"fdr")
res=cbind(pPMGM, BONF=round(BONF,3),BH=round(BH,3), FDR=round(FDR,3))

cor.test(PMG$DM, PMG$GCTFM, method="spearman")
cor.test(PMG$DM, PMG$DSF, method="spearman")
cor.test(PMG$DM, PMG$DSB, method="spearman")
cor.test(PMG$DM, PMG$PAPT.., method="spearman")
cor.test(PMG$GCFTM, PMG$DSF, method="spearman")
cor.test(PMG$GCTFM, PMG$DSB, method="spearman")
cor.test(PMG$GCFTM, PMG$PAPT.., method="spearman")
cor.test(PMG$DSF, PMG$DSB, method="spearman")
cor.test(PMG$DSF, PMG$PAPT.., method="spearman")
cor.test(PMG$DSB,PMG$PAPT.., method="spearman")
cor.test(PMG$GCTFM, PMG$DSF, method="spearman")
cor.test(PMG$GCTFM, PMG$PAPT, method="spearman")

cor.test(PMGM$DM, PMGM$GCTFM, method="spearman")
cor.test(PMGM$DM, PMGM$DSF, method="spearman")
cor.test(PMGM$DM, PMGM$DSB, method="spearman")
cor.test(PMGM$DM, PMGM$PAPT.., method="spearman")
cor.test(PMGM$GCFTM, PMGM$DSF, method="spearman")
cor.test(PMGM$GCTFM, PMGM$DSB, method="spearman")
cor.test(PMGM$GCFTM, PMGM$PAPT.., method="spearman")
cor.test(PMGM$DSF, PMGM$DSB, method="spearman")
cor.test(PMGM$DSF, PMGM$PAPT, method="spearman")
cor.test(PMGM$DM, PMGM$SC, method="spearman")
cor.test(PMGM$SC, PMGM$GCFTM, method="spearman")
cor.test(PMGM$SC, PMGM$DSF, method="spearman")
cor.test(PMGM$SC, PMGM$DSB, method="spearman")
cor.test(PMGM$SC, PMGM$Block, method="spearman")
cor.test(PMGM$SC, PMGM$PAPT.., method="spearman")
cor.test(PMGM$Block, PMGM$DM, method="spearman")
cor.test(PMGM$Block, PMGM$GCTFM, method="spearman")
cor.test(PMGM$Block, PMGM$DSF, method="spearman")
cor.test(PMGM$Block, PMGM$DSB, method="spearman")
cor.test(PMGM$Block, PMGM$PAPT.., method="spearman")
cor.test(PMGM$DSB,PMGM$PAPT.., method="spearman")
cor.test(PMGM$GCTFM, PMGM$DSF, method="spearman")
cor.test(PMGM$GCTFM, PMGM$PAPT, method="spearman")

mcPMG=c(0.08739,
        0.03561,
        0.1242,
        0.08519,
        0.5909,
        0.3407,
        0.02774,
        1.59E-06,
        0.005667,
        0.001377)
BONF=p.adjust(mcPMG,"bonferroni")
BH=p.adjust(mcPMG,"BH")
FDR=p.adjust(mcPMG,"fdr")
res=cbind(mcPMG, BONF=round(BONF,3),BH=round(BH,3), FDR=round(FDR,3))


mcPMGM=c(0.1171,
         0.008467,
         0.09105,
         0.01143,
         0.9511,
         0.8011,
         0.07021,
         0.00001526,
         0.05836,
         0.006421,
         0.06693,
         0.04165,
         0.01809,
         0.02015,
         0.2415,
         0.165,
         0.1332,
         0.367,
         0.04506,
         0.006101)
BONF=p.adjust(mcPMGM,"bonferroni")
BH=p.adjust(mcPMGM,"BH")
FDR=p.adjust(mcPMGM,"fdr")
res=cbind(mcPMGM, BONF=round(BONF,3),BH=round(BH,3), FDR=round(FDR,3))

Cov=read.xlsx(file.choose(),1,header=TRUE)
cor.test(Cov$PMG,Cov$WAB.AQ, method="spearman")
cor.test(Cov$PMG,Cov$AOS.DDK, method="spearman")
cor.test(Cov$PMG,Cov$Semcomp, method="spearman")
cor.test(Cov$PMG,Cov$Phoncomp, method="spearman")
cor.test(Cov$WAB.AQ,Cov$AOS.DDK, method="spearman")
cor.test(Cov$WAB.AQ,Cov$Semcomp, method="spearman")
cor.test(Cov$WAB.AQ,Cov$Phoncomp, method="spearman")
cor.test(Cov$AOS.DDK,Cov$Semcomp, method="spearman")
cor.test(Cov$Semcomp,Cov$Phoncomp, method="spearman")
cor.test(Cov$AOS.DDK,Cov$Phoncomp, method="spearman")

mcCov=c(8.60E-05,
        0.003843,
        9.21E-07,
        4.28E-05,
        0.003597,
        0.001237,
        0.004249,
        0.003871,
        0.01346,
        0.08853)
BONF=p.adjust(mcCov,"bonferroni")
BH=p.adjust(mcCov,"BH")
FDR=p.adjust(mcCov,"fdr")
res=cbind(mcCov, BONF=round(BONF,3),BH=round(BH,3), FDR=round(FDR,3))

Cov=read.xlsx(file.choose(),1,header=TRUE)
cor.test(Cov$PMGM,Cov$WAB.AQ, method="spearman")
cor.test(Cov$PMGM,Cov$AOS.DDK, method="spearman")
cor.test(Cov$PMGM,Cov$Semcomp, method="spearman")
cor.test(Cov$PMGM,Cov$Phoncomp, method="spearman")

mcCov1=c(1.81E-05,
         0.01587,
         4.25E-07,
         0.000894)
BONF=p.adjust(mcCov1,"bonferroni")
BH=p.adjust(mcCov1, "BH")
FDR=p.adjust(mcCov1,"fdr")
res=cbind(mcCov1, BONF=round(BONF,3),BH=round(BH,3), FDR=round(FDR,3))
