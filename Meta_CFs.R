library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse);library(flextable);library(ggrepel)

rm(list=ls())
table <- read.csv("C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/MasterTable.csv")
table$select <- gsub("Damp","Dry",table$select)

nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- nps_centroids[2]
nps_centroids <- nps_centroids |> rename(park=UNIT_CODE)

states <- st_read("C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/State_Shapefile/States_NAD83_Albers.shp")
states <- st_transform(x=states,crs = "NAD83")
states <- states |> filter(!STATE_ABBR %in%  c("HI", "AK"))


HD <- table |> filter(select=="Hot Dry") |> select(c(GCM,select,park))
WW <- table |> filter(select=="Warm Wet") |> select(c(GCM,select,park))
WD <- table |> filter(select=="Warm Dry") |> select(c(GCM,select,park))
HW<- table |> filter(select=="Hot Wet") |> select(c(GCM,select,park))

hd.park <- merge(nps_centroids,HD,by="park",na.rm=TRUE)
ww.park <- merge(nps_centroids,WW,by="park",na.rm=TRUE)
hw.park <- merge(nps_centroids,HW,by="park",na.rm=TRUE)
wd.park <- merge(nps_centroids,WD,by="park",na.rm=TRUE)

hd.park.coords <- extract(hd.park, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  geom_sf(data = hd.park,color="black", pch=21,size=4) + 
  geom_sf(data=hd.park,aes(fill=GCM),pch=21,size=4) +
  # geom_text_repel(data=hd.park.coords,aes(label=GCM)) +
  scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = hd.park.coords, aes(Lat, Lon, label = GCM), size = 5) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
                   # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Hot Dry models") +
  coord_sf()
ggsave("HD.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  geom_sf(data = ww.park,color="black", pch=21,size=4) + 
  geom_sf(data=ww.park,aes(fill=GCM),pch=21,size=4) +
  # geom_text_repel(data=hd.park.coords,aes(label=GCM)) +
  scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = hd.park.coords, aes(Lat, Lon, label = GCM), size = 5) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Warm Wet models") +
  coord_sf()
ggsave("WW.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  geom_sf(data = wd.park,color="black", pch=21,size=4) + 
  geom_sf(data=wd.park,aes(fill=GCM),pch=21,size=4) +
  # geom_text_repel(data=hd.park.coords,aes(label=GCM)) +
  scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = hd.park.coords, aes(Lat, Lon, label = GCM), size = 5) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Warm Dry models") +
  coord_sf()
ggsave("WD.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")

ggplot() + 
  geom_sf(data = states,color="black",fill="tan") + 
  geom_sf(data = hw.park,color="black", pch=21,size=4) + 
  geom_sf(data= hw.park,aes(fill=GCM),pch=21,size=4) +
  # geom_text_repel(data=hd.park.coords,aes(label=GCM)) +
  scale_fill_viridis_d(option = "turbo") +
  # geom_label(data = hd.park.coords, aes(Lat, Lon, label = GCM), size = 5) +
  # geom_label_repel(data = hd.park.coords, aes(x = Lat, y = Lon, label = GCM),
  # size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Hot Wet models") +
  coord_sf()
ggsave("HW.png", width = 15, height = 9, path = "C:/Users/arunyon/Downloads")
