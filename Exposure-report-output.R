library(here); library(ggplot2);  library(data.table); library(lubridate); library(dplyr); library(openxlsx); library(tidyr)


rm(list = ls())


SiteID = "PIRO"  
Hist.data <- "C:/Users/achildress/Documents/RCF_Testing/PIRO-Historical/"
Future.data <- "C:/Users/achildress/Documents/RCF_Testing/PIRO/"
Output.directory <- "C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/Exposure reports FY22/FY23_RSS Exposure Reports/Report-data/"

CF_selected <- "WarmWet_HotDry" #Select your CF by commenting out set you do not wish to use
# CF_selected_updates <- "WarmDry_HotWet" #Select your CF by commenting out set you do not wish to use
##################
##################
#Load Data

load(paste0(Future.data,"input-data/Final_Environment.RData"))
OutDir <- Future.data


if(CF_selected == "WarmWet_HotDry") {
  FutureSubset <- CFs_all[c(1,5)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WW-HD"
  colors2<- colors5[c(1,4)] # Select pair of climate futures - WarmWet/HotDry
  colors3<-c("white",colors2)
  col<- c("darkgray",colors2)  # WarmWet/HotDry
  CFDir = paste0(OutDir,"WarmWet_HotDry/") # for .csv's
} else{
  FutureSubset <- CFs_all[c(4,2)]; CFs = FutureSubset  # Pick pair of climate futures.
  CF_abbreviation <- "WD-HW"
  colors2<- colors5[c(3,2)] # Select pair of climate futures - HotWet/WarmDry
  colors3<-c("white",colors2)
  # col<- c("darkgray","#9A9EE5","#E10720")  # WarmWet/HotDry
  col<- c("darkgray", colors2)  # HotWet/WarmDry
  CFDir = paste0(OutDir,"WarmDry_HotWet/") # for .csv's
}
TableDir = paste0(CFDir,"tables/") # for .csv's
FigDir = paste0(CFDir,"figures/") # for .csv's

D_Annual <- openxlsx::read.xlsx(xlsxFile=paste0(TableDir,SiteID,"_",CF_abbreviation,"_Plot_data.xlsx"),sheet="D_Annual")
recurrence <- read.csv(paste0(TableDir, "precip_recurrence_interval.csv"))
AnnualWB <- read.csv(paste0(TableDir,"WB-Annual.csv")) %>% 
  left_join(WB_GCMs,by="GCM") %>% 
  mutate(sum_d.in = sum_d.mm/ 25.4,
    CF = replace_na(CF,"Historical"),
    CF = factor(CF, levels=c("Historical",CFs)))


#Historical Data
Historical.AnnualMeans <- read.csv(paste0(Hist.data,"Annual-Averages.csv"))
Historical.Regression <- read.csv(paste0(Hist.data,"Regression Table.csv"))
Historical.Anomalies <- read.csv(paste0(Hist.data,"Anomalies-table.csv"))

# Historical
Exposure.Data <- data.frame(SiteID = SiteID)
Exposure.Data$nClim.Tavg.min <- min(Historical.AnnualMeans$tmeanAvg)
Exposure.Data$nClim.Tavg.mean <- mean(Historical.AnnualMeans$tmeanAvg)
Exposure.Data$nClim.Tavg.max <- max(Historical.AnnualMeans$tmeanAvg)
Exposure.Data$nClim.Prcp.min <- min(Historical.AnnualMeans$pptAvg)
Exposure.Data$nClim.Prcp.mean <- mean(Historical.AnnualMeans$pptAvg)
Exposure.Data$nClim.Prcp.max <- max(Historical.AnnualMeans$pptAvg)
Exposure.Data$Tavg.trend <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[9] >0, "increased", "decreased")
Exposure.Data$Prcp.trend <- ifelse(Historical.Regression$YrCoeff.degF.in..100yrs.[12] >0, "increased", "decreased")
Exposure.Data$Tavg.rate.1900 <- Historical.Regression$YrCoeff.degF.in..100yrs.[7]
Exposure.Data$Tavg.rate.1970 <- Historical.Regression$YrCoeff.degF.in..100yrs.[9]
Exposure.Data$Prcp.rate.1900 <- Historical.Regression$YrCoeff.degF.in..100yrs.[10]
Exposure.Data$Prcp.rate.1970 <- Historical.Regression$YrCoeff.degF.in..100yrs.[12]
Exposure.Data$Tavg.Anomalies.1 <- Historical.Anomalies$hist.anomalies.tmean[1]
Exposure.Data$Tavg.Anomalies.2 <- Historical.Anomalies$hist.anomalies.tmean[2]
Exposure.Data$PrcpAbove.Anomalies.1 <- Historical.Anomalies$hist.anomalies.above.prcp[1]
Exposure.Data$PrcpAbove.Anomalies.2 <- Historical.Anomalies$hist.anomalies.above.prcp[2]
Exposure.Data$PrcpBelow.Anomalies.1 <- Historical.Anomalies$hist.anomalies.below.prcp[1]
Exposure.Data$PrcpBelow.Anomalies.2 <- Historical.Anomalies$hist.anomalies.below.prcp[2]
Exposure.Data$Tavg.Anomalies.recent.percent <- Historical.Anomalies$recent.percent.tmean.anomaly[1]
Exposure.Data$PrcpAbove.Anomalies.recent.percent <- Historical.Anomalies$recent.percent.above.prcp.anomaly[1]

# Future
Exposure.Data$Future.DeltaTavg.min <- min(Future_Means$DeltaTavg)
Exposure.Data$Future.DeltaTavg.max <- max(Future_Means$DeltaTavg)
Exposure.Data$Future.DeltaPr.min <- min(Future_Means$DeltaPr*365)
Exposure.Data$Future.DeltaPr.max <- max(Future_Means$DeltaPr*365)
Exposure.Data$Future.DeltaPr.min.percent <- (min(Future_Means$DeltaPr*365))/(BaseMeanPr*365)*100
Exposure.Data$Future.DeltaPr.max.percent <- (max(Future_Means$DeltaPr*365))/(BaseMeanPr*365)*100

Exposure.Data$DeltaTavg.CF1 <- D_Annual$TavgF[2]
Exposure.Data$DeltaTavg.CF2 <- D_Annual$TavgF[3]
Exposure.Data$DeltaPrcp.CF1 <- D_Annual$PrcpIn[2]
Exposure.Data$DeltaPrcp.CF2 <- D_Annual$PrcpIn[3]
Exposure.Data$HI.Dan.CF1 <- D_Annual$HI.Dan[2]
Exposure.Data$HI.Dan.CF2 <- D_Annual$HI.Dan[3]

Exposure.Data$Hist_return50 <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF=="Historical")],1)
Exposure.Data$CF1_return50  <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF==CFs[1])],1)
Exposure.Data$CF2_return50  <- round(recurrence$GEV[which(recurrence$return == 50 & recurrence$CF==CFs[2])],1)
Exposure.Data$CF1_return.year <- as.integer(recurrence %>% filter(CF == CFs[1]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
Exposure.Data$CF2_return.year <- as.integer(recurrence %>% filter(CF == CFs[2]) %>% slice(which.min(abs(GEV - Exposure.Data$Hist_return50))) %>% dplyr::select(return))
Exposure.Data$HistPrecip95 <- HistPrecip95
Exposure.Data$Hist.meanWB<-mean(AnnualWB$sum_d.in[which(AnnualWB$year<=2012)])
Exposure.Data$CF1.WBdelta <- mean(AnnualWB$sum_d.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                          AnnualWB$CF == CFs[1])]) - Exposure.Data$Hist.meanWB
Exposure.Data$CF2.WBdelta <- mean(AnnualWB$sum_d.in[which(AnnualWB$year>=Yr-Range/2 & AnnualWB$year<= Yr+Range/2 & 
                                                            AnnualWB$CF == CFs[2])]) - Exposure.Data$Hist.meanWB

write.csv(Exposure.Data, paste0(Output.directory,"-",SiteID,"-ExposureData.csv"))
