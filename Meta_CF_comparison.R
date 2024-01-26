library(dplyr)
library(plyr)
library(ggplot2)
library(openxlsx)
library(sf)
library(tidyverse);library(flextable);library(ggrepel)
library(gridExtra)
library(grid)
library(ggpubr)

rm(list=ls())
OutDir <- "C:/Users/arunyon/3D Objects/Local-files/Meta-analysis/Output/" #directory for output/plots
DataDir <- "D:/RCF/RCF_opened/"

nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
conus_centroids <- nps_centroids |> filter(!STATE %in% c("AK","HI","AS","GU", "MP","PR","VI"))
# states <-  st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/State_Shapefile/States_NAD83_Albers.shp')
# states <- st_transform(x=states,crs = "NAD83")
# states <- states |> filter(STATE_ABBR %in% conus_centroids$STATE)
# states_geometry <- st_geometry(states)

Parks <- conus_centroids$UNIT_CODE
# Parks <- subset to parks in NER
# ParkFolders <- paste0(DataDir,Parks,"/")

conus_centroids <- conus_centroids[2] 
conus_centroids <- conus_centroids |> dplyr::rename(park=UNIT_CODE)

# CFs <- "WarmDry_HotWet/" #WarmWet_HotDry/, WarmDry_HotWet/
# CF.abbrev <- "WD-HW" #WW-HD, WD-HW
# 
# MasterTable <- data.frame()
# for (i in 1:length(Parks)){
#   table.name <- paste0(ParkFolders[i],CFs,"tables/",Parks[i],"_",CF.abbrev,"_Plot_data.xlsx")
#   if(!file.exists(table.name)) {
#     next} else {
#       table <- read.xlsx(xlsxFile = table.name,sheet = 1)
#       drought <- read.csv(paste0(ParkFolders[i],CFs,"tables/Drought_characteristics.csv"))
#       table <- merge(table,drought,by="CF")
#       return <- read.csv(paste0(ParkFolders[i],CFs,"tables/precip_recurrence_interval.csv")) |>
#         subset(return==50,select=c(CF,GEV))
#       table <- merge(table,return,by="CF")
#       table$park <- Parks[i]
#       MasterTable <- rbind(MasterTable,table)
#     }
# }
# write.csv(MasterTable,paste0(OutDir,"WD-HW-ALL-MasterTable.csv"),row.names=F,)
# 
# CFs <- "WarmWet_HotDry/" #WarmWet_HotDry/, WarmDry_HotWet/
# CF.abbrev <- "WW-HD" #WW-HD, WD-HW
# 
# MasterTable <- data.frame()
# for (i in 1:length(Parks)){
#   table.name <- paste0(ParkFolders[i],CFs,"tables/",Parks[i],"_",CF.abbrev,"_Plot_data.xlsx")
#   if(!file.exists(table.name)) {
#     next} else {
#       table <- read.xlsx(xlsxFile = table.name,sheet = 1)
#       drought.table <- paste0(ParkFolders[i],CFs,"tables/Drought_characteristics.csv")
#       if(!file.exists(drought.table)) {
#         next} else {
#       drought <- read.csv(drought.table)
#       table <- merge(table,drought,by="CF")
#       return <- read.csv(paste0(ParkFolders[i],CFs,"tables/precip_recurrence_interval.csv")) |>
#         subset(return==50,select=c(CF,GEV))
#       table <- merge(table,return,by="CF")
#       table$park <- Parks[i]
#       MasterTable <- rbind(MasterTable,table)
#         }
#     }
# }
# write.csv(MasterTable,paste0(OutDir,"WW-HD-ALL-MasterTable.csv"),row.names=F,)

WW.HD <- read.csv(paste0(OutDir,"WW-HD-ALL-MasterTable.csv")) |> arrange(park)
WD.HW <- read.csv(paste0(OutDir,"WD-HW-ALL-MasterTable.csv")) |> arrange(park)

head(WW.HD)
all <- rbind(WW.HD,WD.HW) |> arrange(park)
all <- unique(all)

hist <- all |> filter(CF=="Historical") |> select(c(CF, Severity, GEV, park))
ww.hd.fut <- WW.HD |> filter(CF!="Historical") |> 
  select(c(CF, Severity, GEV, park)) |> 
  left_join(hist,by="park") |> 
  mutate(Severity.delta = Severity.x - Severity.y,
         GEV.delta = GEV.x - GEV.y)

ww.hd.CFdelta <- ww.hd.fut |> group_by(park) |> mutate(Severity.CFdelta = Severity.x - Severity.x[CF.x == "Warm Wet"],
                                                       GEV.CFdelta = GEV.x - GEV.x[CF.x == "Warm Wet"]) |> 
  subset(CF.x != "Warm Wet", select=c(park, Severity.CFdelta, GEV.CFdelta)) |> mutate(CF="WW.HD")
 
ww.hd.GEV <- ww.hd.fut |> filter(GEV.delta<=0) |> 
  select(c(CF.x,park,GEV.delta)) |> filter(duplicated(park))

ww.hd.drt <- ww.hd.fut |> filter(Severity.delta>=0) |> 
  select(c(CF.x,park,Severity.delta)) |> filter(duplicated(park))
 
  
wd.hw.fut <- WD.HW |> filter(CF!="Historical") |> 
  select(c(CF, Severity, GEV, park)) |> 
  left_join(hist,by="park") |> 
  mutate(Severity.delta = Severity.x - Severity.y,
         GEV.delta = GEV.x - GEV.y)

wd.hw.CFdelta <- wd.hw.fut |> group_by(park) |> mutate(Severity.CFdelta = Severity.x - Severity.x[CF.x == "Hot Wet"],
                                                       GEV.CFdelta = GEV.x - GEV.x[CF.x == "Hot Wet"]) |> 
  subset(CF.x != "Hot Wet", select=c(park, Severity.CFdelta, GEV.CFdelta)) |> mutate(CF="WD.HW")


wd.hw.GEV <- wd.hw.fut |> filter(GEV.delta<=0) |> 
  select(c(CF.x,park,GEV.delta)) |> filter(duplicated(park))

wd.hw.drt <- wd.hw.fut |> filter(Severity.delta>=0) |> 
  select(c(CF.x,park,Severity.delta)) |> filter(duplicated(park))

#Merge for plotting
park.GEV <- rbind(ww.hd.GEV |> mutate(CF="WW.HD"),wd.hw.GEV |> mutate(CF="WD.HW")) 

park.GEV$CF[duplicated(park.GEV$park)|duplicated(park.GEV$park, fromLast=TRUE)] <- "Both"

park.GEV <- park.GEV %>% 
  group_by(park) %>% 
  slice_max(GEV.delta, n = 1) %>%
  ungroup()

park.GEV <- merge(conus_centroids,park.GEV,by="park",na.rm=TRUE)
GEVpark.coords <- extract(park.GEV, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

park.drt <- rbind(ww.hd.drt |> mutate(CF="WW.HD"),wd.hw.drt |> mutate(CF="WD.HW")) 
park.drt$CF[duplicated(park.drt$park)|duplicated(park.drt$park, fromLast=TRUE)] <- "Both"

park.drt <- park.drt %>% 
  group_by(park) %>% 
  slice_max(Severity.delta, n = 1) %>%
  ungroup()

park.drt <- merge(conus_centroids,park.drt,by="park",na.rm=TRUE)
drtpark.coords <- extract(park.drt, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  # geom_sf(data = park.GEV,color="black", pch=21,size=4) + 
  geom_sf(data=park.GEV,aes(fill=CF,size=-GEV.delta),pch=21) +
  # geom_text_repel(data=park.GEV,aes(label=park)) +
  scale_fill_manual(values=c("gray","darkgreen","red")) +
  # scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = GEVpark.coords, aes(Lat, Lon, label = park), size = 4) +
  geom_label_repel(data = GEVpark.coords, aes(x = Lat, y = Lon, label = park),
                   size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Parks where extreme precip models show decreasing amount") +
  coord_sf()
ggsave("GEV-map.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  # geom_sf(data = park.GEV,color="black", pch=21,size=4) + 
  geom_sf(data=park.drt,aes(fill=CF,size=Severity.delta),pch=21) +
  # geom_text_repel(data=park.GEV,aes(label=park)) +
  scale_fill_manual(values=c("gray","darkgreen","red")) +
  # scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = GEVpark.coords, aes(Lat, Lon, label = park), size = 4) +
  geom_label_repel(data = drtpark.coords, aes(x = Lat, y = Lon, label = park),
                   size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Parks where drought severity models show decreasing amount") +
  coord_sf()
ggsave("GEV-map.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")

#Look at which diagonal have most divergence and most extreme precip and drought
CFdelta <- rbind(ww.hd.CFdelta,wd.hw.CFdelta) |> mutate(abs.GEV = abs(GEV.CFdelta),
                                                        abs.drt = abs(Severity.CFdelta))
GEV.max <- CFdelta %>% 
  group_by(park) %>% 
  slice_max(abs.GEV, n = 1) %>%
  ungroup()

severity.max <- CFdelta %>% 
  group_by(park) %>% 
  slice_max(abs.drt, n = 1) %>%
  ungroup()

GEVmax.centroids <- merge(conus_centroids,GEV.max,by="park",na.rm=TRUE)
GEVmax.coords <- extract(GEVmax.centroids, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  # geom_sf(data = park.GEV,color="black", pch=21,size=4) + 
  geom_sf(data=GEVmax.centroids,aes(fill=CF,size=abs.GEV),pch=21) +
  # geom_text_repel(data=park.GEV,aes(label=park)) +
  scale_fill_manual(values=c("darkgreen","red")) +
  # scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = GEVpark.coords, aes(Lat, Lon, label = park), size = 4) +
  # geom_label_repel(data = GEVmax.coords, aes(x = Lat, y = Lon, label = park),
                   # size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("CF diagonals with most divergence for extreme precip") +
  coord_sf()
ggsave("GEV-map.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")

Drtmax.centroids <- merge(conus_centroids,severity.max,by="park",na.rm=TRUE)
Drtmax.coords <- extract(Drtmax.centroids, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  # geom_sf(data = park.GEV,color="black", pch=21,size=4) + 
  geom_sf(data=Drtmax.centroids,aes(fill=CF,size=abs.drt),pch=21) +
  # geom_text_repel(data=park.GEV,aes(label=park)) +
  scale_fill_manual(values=c("darkgreen","red")) +
  # scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = GEVpark.coords, aes(Lat, Lon, label = park), size = 4) +
  # geom_label_repel(data = GEVmax.coords, aes(x = Lat, y = Lon, label = park),
  # size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("CF diagonals with most divergence for drought severity") +
  coord_sf()
