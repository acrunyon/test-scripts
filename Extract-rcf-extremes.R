library(dplyr)
library(plyr)
library(ggplot2)
library(openxlsx)
library(sf)
library(tidyverse);library(flextable);library(ggrepel)
library(gridExtra)
library(grid)
library(ggpubr)
library(SPEI)

rm(list=ls())
Output.folder <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/ClimateMatters/" #directory for output/plots
Data.folder <- "E:/RCF_2024/RCF_opened/"

### Use this chunk if extracting all units
# nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
# # ner_centroids <- nps_centroids |> filter(REGION == "NE")
# conus_centroids <- nps_centroids |> filter(!STATE %in% c("AK","HI","AS","GU", "MP","PR","VI"))
# states <-  st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/State_Shapefile/States_NAD83_Albers.shp')
# states <- st_transform(x=states,crs = "NAD83")
# states <- states |> filter(STATE_ABBR %in% conus_centroids$STATE)
# # states <- states |> filter(STATE_ABBR %in% ner_centroids$STATE)
# states_geometry <- st_geometry(states)
# Parks <- conus_centroids$UNIT_CODE

Parks <- read.csv("C:/Users/arunyon/Downloads/parks.csv") |> pull(UNIT_CODE)

columns <- c("CF","GCM","PrcpIn","OverHotTemp","OverHighQ","Tmax99","OverPrecip95","OverPrecip99",
                     "TmaxF","TminF","TavgF","maxTmaxF","maxPr","park")
Means = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(Means) = columns

# CFs <- "WarmDry_HotWet/" #WarmWet_HotDry/, WarmDry_HotWet/
# CF.abbrev <- "WD-HW" #WW-HD, WD-HW
for (i in 1:length(Parks)){
park.i = Parks[i]
ParkFolder <- paste0(Data.folder,park.i,"/")
load(paste0(ParkFolder, "input-data/Final_Environment.RData"))
head(Gridmet)
head(ALL_FUTURE)
G <- Gridmet
G$CF <- "Historical"

G$OverHotTemp = G$TmaxF > HotTemp
G$OverHighQ = G$TmaxF > HistTmaxHigh
G$Tmax99 = G$TmaxF > HistTmax99
G$HeatConsecutive=(G$Tmax99)*unlist(lapply(rle(G$Tmax99)$lengths, seq_len))
G$OverPrecip95 = G$PrcpIn > HistPrecip95
G$OverPrecip99 = G$PrcpIn > HistPr99

H_annual <- G %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(PrcpIn,OverHotTemp, OverHighQ, Tmax99, OverPrecip95, OverPrecip99), sum)  
Hmeans<-G %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(TmaxF,TminF,TavgF),mean)
H_annual<-merge(H_annual,Hmeans,by=c("CF","GCM","Year"), all=TRUE);rm(Hmeans)
H_annual$park <- park.i


AF <- ALL_FUTURE |> filter(GCM %in% WB_GCMs$GCM) |> left_join(WB_GCMs,by="GCM")
AF$OverHotTemp = AF$TmaxF > HotTemp
AF$OverHighQ = AF$TmaxF > HistTmaxHigh
AF$Tmax99 = AF$TmaxF > HistTmax99
AF$HeatConsecutive=(AF$Tmax99)*unlist(lapply(rle(AF$Tmax99)$lengths, seq_len))
AF$OverPrecip95 = AF$PrcpIn > HistPrecip95
AF$OverPrecip99 = AF$PrcpIn > HistPr99

F_annual <- AF %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(PrcpIn,OverHotTemp, OverHighQ, Tmax99, OverPrecip95, OverPrecip99), sum)  
Fmeans<-AF %>% group_by(CF, GCM, Year) %>%
  summarise_at(vars(TmaxF,TminF,TavgF),mean)
F_annual<-merge(F_annual,Fmeans,by=c("CF","GCM","Year"), all=TRUE);rm(Fmeans)

write.csv(H_annual,paste0(Output.folder,park.i,"_Hist_annual.csv"))
write.csv(F_annual,paste0(Output.folder,park.i,"_Future_annual.csv"))

# For mid-century
#Tmean, Prcp Pr99, Tmax99, maxmax
h <- aggregate(.~CF+GCM,H_annual[,c(1:2,4:12)], mean,na.action = na.pass)
h$maxTmaxF <- max(G$TmaxF[which(G$Year<2013)])
h$maxPr <- max(G$PrcpIn[which(G$Year<2013)])

h <- aggregate(.~CF+GCM,H_annual[,c(1:2,4:12)], mean,na.action = na.pass)
h$maxTmaxF <- max(G$TmaxF[which(G$Year<2013)])
h$maxPr <- max(G$PrcpIn[which(G$Year<2013)])

f <- aggregate(.~CF+GCM,F_annual[,c(1:2,4:12)], mean,na.action = na.pass)
f1 <- setNames(aggregate(cbind(TmaxF, PrcpIn)~CF+GCM,subset(AF,Year>2034 & Year<2066),max),c("CF","GCM","maxTmaxF","maxPr"))
f <- merge(f,f1,by=c("CF","GCM"))
b <- rbind(h,f);b$park <- park.i
Means <- rbind(Means,b)


## SPEI
# Run SPEI and output ts .csv
## Warm Dry-Hot Wet
CFs<-c("Warm Dry","Hot Wet")
MonthlyWB <- read.csv(paste0(Data.folder,park.i,"/WarmDry_HotWet/tables/WB-Monthly.csv")) %>% 
  left_join(WB_GCMs,by="GCM") %>% 
  mutate(CF = replace_na(CF,"Historical"),
         CF = factor(CF, levels=c("Historical",CFs)),
         Date = as.POSIXct(paste(substr(yrmon,1,4),substr(yrmon,5,6),"1",sep="-"),format="%Y-%m-%d"),
         Year = format(Date, "%Y")) %>% 
  arrange(Date)


M1 <- list()
for (i in 1:length(CFs)){
  M = MonthlyWB %>% filter(CF %in% c("Historical",CFs[i])) %>% 
    complete(Date = seq(min(Date), max(Date), by = "1 month"), 
             fill = list(value = NA)) 
  
  tp<-ts(M$sum_p.mm,frequency=12,start=c(SPEI_start,1)); tp[is.na(tp)]<-0
  tpet<-ts(M$sum_pet.mm,frequency=12,start=c(SPEI_start,1)); tpet[is.na(tpet)]<-0
  SPEI<-spei(tp-tpet,SPEI_per,ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
  M$SPEI = SPEI$fitted[1:length(SPEI$fitted)]
  M1[[i]]<-M %>% drop_na()
}
all2<- ldply(M1, data.frame) #convert back to df
all2$SPEI[which(is.infinite(all2$SPEI))]<- -5 #getting some -Inf values that are large jumps, temp fix

# 
# all3<-subset(all2,Month==9) #Because we aggregated drought years as only applying to growing season
#                             # If you are doing for place where winter drought would be important, use following line
all3<-aggregate(cbind(sum_pet.mm,SPEI)~Year+CF,all2,mean);all3$park <- park.i
write.csv(all3,paste0(Output.folder,park.i,"_WarmDry-HotWet_SPEI.csv"))


## Warm Wet - Hot Dry
CFs<-c("Warm Wet","Hot Dry")
MonthlyWB <- read.csv(paste0(Data.folder,park.i,"/WarmWet_HotDry/tables/WB-Monthly.csv")) %>% 
  left_join(WB_GCMs,by="GCM") %>% 
  mutate(CF = replace_na(CF,"Historical"),
         CF = factor(CF, levels=c("Historical",CFs)),
         Date = as.POSIXct(paste(substr(yrmon,1,4),substr(yrmon,5,6),"1",sep="-"),format="%Y-%m-%d"),
         Year = format(Date, "%Y")) %>% 
  arrange(Date)

M1 <- list()
for (i in 1:length(CFs)){
  M = MonthlyWB %>% filter(CF %in% c("Historical",CFs[i])) %>% 
    complete(Date = seq(min(Date), max(Date), by = "1 month"), 
             fill = list(value = NA)) 
  
  tp<-ts(M$sum_p.mm,frequency=12,start=c(SPEI_start,1)); tp[is.na(tp)]<-0
  tpet<-ts(M$sum_pet.mm,frequency=12,start=c(SPEI_start,1)); tpet[is.na(tpet)]<-0
  SPEI<-spei(tp-tpet,SPEI_per,ref.start=c(SPEI_start,1),ref.end=c(SPEI_end,12))
  M$SPEI = SPEI$fitted[1:length(SPEI$fitted)]
  M1[[i]]<-M %>% drop_na()
}
all2<- ldply(M1, data.frame) #convert back to df
all2$SPEI[which(is.infinite(all2$SPEI))]<- -5 #getting some -Inf values that are large jumps, temp fix

# 
# all3<-subset(all2,Month==9) #Because we aggregated drought years as only applying to growing season
#                             # If you are doing for place where winter drought would be important, use following line
all3<-aggregate(cbind(sum_pet.mm,SPEI)~Year+CF,all2,mean);all3$park <- park.i
write.csv(all3,paste0(Output.folder,park.i,"_WarmDry-HotWet_SPEI.csv"))
}
write.csv(Means,paste0(Output.folder,"0_All-park-midCen-means.csv"))

