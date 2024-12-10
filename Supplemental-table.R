library(dplyr)

rm(list=ls())

load("C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/PARK/Waterhole/input-data/Final_Environment.RData")

# Run Appendix Script
source("Appendix_Script.R", local = knitr::knit_global())

# Appendix: Calculations for change
Exposure.Data$PrcpChangeCF1 <- Exposure.Data$Future.PrcpIn.CF1 - Exposure.Data$Prcp.max.hist
Exposure.Data$PrcpChangeCF2 <- Exposure.Data$Future.PrcpIn.CF2 - Exposure.Data$Prcp.max.hist
Exposure.Data$DrtDurChangeCF1 <- Drought.char$Duration[2] - Drought.char$Duration[1]
Exposure.Data$DrtDurChangeCF2 <- Drought.char$Duration[3] - Drought.char$Duration[1]
Exposure.Data$DrtFreeChangeCF1 <- Drought.char$Drt.Free[2] - Drought.char$Drt.Free[1]
Exposure.Data$DrtFreeChangeCF2 <- Drought.char$Drt.Free[3] - Drought.char$Drt.Free[1]
Exposure.Data$DrtSevChangeCF1 <- Drought.char$Severity[2] - Drought.char$Severity[1]
Exposure.Data$DrtSevChangeCF2 <- Drought.char$Severity[3] - Drought.char$Severity[1]

# Appendix: Seasonal changes
Exposure.Data$TempWinterHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Winter")]
Exposure.Data$DTempWinterCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Winter")]
Exposure.Data$DTempWinterCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Winter")]
Exposure.Data$TempSpringHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Spring")]
Exposure.Data$DTempSpringCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Spring")]
Exposure.Data$DTempSpringCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Spring")]
Exposure.Data$TempSummerHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Summer")]
Exposure.Data$DTempSummerCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Summer")]
Exposure.Data$DTempSummerCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Summer")]
Exposure.Data$TempFallHist <- H_SeasMean$TavgF[which(H_SeasMean$season == "Fall")]
Exposure.Data$DTempFallCF1 <- Season_delta$TavgF[which(Season_delta$CF == CF1.Name & Season_delta$season == "Fall")]
Exposure.Data$DTempFallCF2 <- Season_delta$TavgF[which(Season_delta$CF == CF2.Name & Season_delta$season == "Fall")]

Exposure.Data$PrcpWinterHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Winter")]
Exposure.Data$DPrcpWinterCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Winter")]
Exposure.Data$DPrcpWinterCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Winter")]
Exposure.Data$PrcpSpringHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Spring")]
Exposure.Data$DPrcpSpringCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Spring")]
Exposure.Data$DPrcpSpringCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Spring")]
Exposure.Data$PrcpSummerHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Summer")]
Exposure.Data$DPrcpSummerCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Summer")]
Exposure.Data$DPrcpSummerCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Summer")]
Exposure.Data$PrcpFallHist <- H_SeasMean$PrcpIn[which(H_SeasMean$season == "Fall")]
Exposure.Data$DPrcpFallCF1 <- Season_delta$PrcpIn[which(Season_delta$CF == CF1.Name & Season_delta$season == "Fall")]
Exposure.Data$DPrcpFallCF2 <- Season_delta$PrcpIn[which(Season_delta$CF == CF2.Name & Season_delta$season == "Fall")]

Exposure.Data$AET1 <- mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                       AnnualWB$CF == CFs[1])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)])
Exposure.Data$AET2 <- mean(AnnualWB$sum_aet.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                       AnnualWB$CF == CFs[2])]) - mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)])
Exposure.Data$AET3 <- mean(AnnualWB$sum_aet.in[which(AnnualWB$year<=2012)])

Exposure.Data <- Exposure.Data %>% mutate_if(is.numeric, round, digits=1) #Rounding all variables

