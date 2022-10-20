library(sf)
library(ggplot2)
library(tidyverse)
library(rgdal) # used to read world map data
library(rgeos) # to fortify without needing gpclib
library(maptools)
library(scales)
library(ggrepel)
rm(list=ls())

map_data<- "C:/Users/achildress/OneDrive - DOI/CF_management/CF_product_maps/CF_maps/" #Location spatial data stored
OutDir <- "C:/Users/achildress/DOI/CCRP COLLABORATE! - CCRP COLLABORATE!/01 PROJECT Collaboration/Science, Adaptation, Planning/Climate Products/1_Maps and Guides/Climate Products Maps/ClimateMaps_Oct2022/"
regions <- st_read(paste0(map_data,"nps-regions.shp"))
centroids <- st_read(paste0(map_data,"nps_boundary_centroids.shp"))
HI <- st_read(paste0(map_data,"HI/hawaii.shp"))
PRVI <- st_read(paste0(map_data,"PR_VI/PR_VI.shp"))

data <- read.csv("C:/Users/achildress/OneDrive - DOI/CF_management/CF_product_maps/Existing_CF_List_221020.csv")

cf<- centroids %>% left_join(data,by="UNIT_CODE")

regions <- st_transform(x=regions,
                        crs = "NAD83")
cf <- st_transform(x=cf,
                   crs = "NAD83")

HI <- st_transform(x = HI,
                   crs = "NAD83")

PRVI <- st_transform(x = PRVI,
                     crs = "NAD83")
cf_prods <- c("CFs", "GFIP","Both", "Coming in FY23","None")

cf$Combined_2 <- cf$source %>% replace(is.na(.), "None")
cf$Combined_2 <- factor(cf$Combined_2,levels=cf_prods)


cf <- cf %>% filter(STATE != c("AS", "GU"))

cf$col <- "--"
cf$col[which(cf$Combined_2== "Both")]<-"white"
cf$col[which(cf$Combined_2== "CFs")]<-"white"
cf$col[which(cf$Combined_2== "GFIP")]<-"white"
cf$col[which(cf$Combined_2== "Coming in FY23")]<-"white"
cf$col[which(cf$Combined_2== "None")]<-"grey70"


region_geometry <- st_geometry(regions)

ggplot() + 
  geom_sf(data = regions, aes(fill=nps_bounda)) + 
  geom_sf_text(aes(label = regions$nps_bounda, geometry = region_geometry), fun.geometry = st_centroid, colour = "white") +
  ggtitle("NPS Regions")  +
  coord_sf()

cf_geometry <- st_geometry(cf)

ggplot() + 
  geom_point(data = cf, aes(x = Lon, y = Lat)) +
  ggtitle("NPS Regions") +
  coord_sf()

im_region <- regions %>% filter(regions$nps_bounda == "IM")
ak_region <- regions %>% filter(regions$nps_bounda == "AK")
mw_region <- regions %>% filter(regions$nps_bounda == "MW")
nc_region <- regions %>% filter(regions$nps_bounda == "NC")
ne_region <- regions %>% filter(regions$nps_bounda == "NE")
pw_region <- regions %>% filter(regions$nps_bounda == "PW")
se_region <- regions %>% filter(regions$nps_bounda == "SE") 
hi_region <- HI
car_region <- PRVI


im_cf <- cf %>% filter(cf$REGION == "IM")
ak_cf <- cf %>% filter(cf$REGION == "AK")
mw_cf <- cf %>% filter(cf$REGION == "MW")
nc_cf <- cf %>% filter(cf$REGION == "NC")
ne_cf <- cf %>% filter(cf$REGION == "NE")

pw_cf <- cf %>% filter(cf$REGION == "PW") %>% 
  filter(STATE != "MP")

pw_cf <- pw_cf %>% 
  filter(STATE != "HI")

se_cf <- cf %>% filter(cf$REGION == "SE") %>% 
  filter(STATE != c("VI", "PR")) %>% 
  filter(UNIT_CODE != c("SARI", "VIIS", "CHRI"))

hi_cf <- cf %>% filter(cf$REGION == "PW") %>% 
  filter(STATE == "HI")

car_cf <- cf %>% filter(STATE %in% c("VI", "PR"))

geom_text_repel(hjust=0,vjust=0,aes(label=GCM,colour=col))

hi_cf$col <- as.factor(hi_cf$col)



HI_fig <- ggplot() + 
  geom_sf(data = hi_region, colour = "black", fill = "wheat2") + 
  geom_point(data = hi_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = hi_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Hawai'i - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) + 
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"HI_region.pdf"), plot = HI_fig,dpi=300,width=12,height=12)

# PR & USVI 

PRVI_fig <- ggplot() + 
  geom_sf(data = car_region, colour = "black", fill = "wheat2") + 
  geom_point(data = car_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = car_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F ) +
  ggtitle("Puerto Rico / U.S.V.I. - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"PRVI_region.pdf"), plot = PRVI_fig,dpi=300,width=12,height=12)


IM_fig <- ggplot() + 
  geom_sf(data = im_region, colour = "black", fill = "wheat2") + 
  geom_point(data = im_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = im_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Intermountain Region - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()
IM_fig

ggsave(paste0(OutDir,"IM_region.pdf"), plot = IM_fig,dpi=300,width=12,height=12)


AK_fig <- ggplot() + 
  geom_sf(data = ak_region, colour = "black", fill = "wheat2") + 
  geom_point(data = ak_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2), size = 2.5) +
  #geom_text(data = ak_cf, aes(x = Lon, y = Lat, label = UNIT_CODE), size = 3) +
  geom_label_repel(data = ak_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Alaska Region - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"AK_region.pdf"), plot = AK_fig, dpi=300,width=12,height=12)


MW_fig <- ggplot() + 
  geom_sf(data = mw_region, colour = "black", fill = "wheat2") + 
  geom_point(data = mw_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = mw_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Midwest Region - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"MW_region.pdf"), plot = MW_fig,dpi=300,width=8,height=8)


NC_fig <- ggplot() + 
  geom_sf(data = nc_region, colour = "black", fill = "wheat2") + 
  geom_point(data = nc_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = nc_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, 
                   segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99), show.legend=F) +
  ggtitle("National Capital Region - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"NCap_region.pdf"), plot = NC_fig,dpi=300,width=10,height=10)


NE_fig <- ggplot() + 
  geom_sf(data = ne_region, colour = "black", fill = "wheat2") + 
  geom_point(data = ne_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) +
  geom_label_repel(data = ne_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, 
                   segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99), show.legend=F,) +
  ggtitle("Northeast Region - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"NE_region.pdf"), plot = NE_fig, dpi=300,width=10,height=10)

PW_fig <- ggplot() + 
  geom_sf(data = pw_region, colour = "black", fill = "wheat2") + 
  geom_point(data = pw_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) +
  geom_label_repel(data = pw_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, 
                   segment.color = 'grey50',  max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Pacific West Region - Climate Product Production") +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf(xlim = c(-126, -110), ylim = c(32, 50)) 

ggsave(paste0(OutDir,"PW_region.pdf"), plot = PW_fig,dpi=300,width=10,height=10)


SE_fig <- ggplot() + 
  geom_sf(data = se_region, colour = "black", fill = "wheat2") + 
  geom_point(data = se_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2), size = 2.5) + 
  geom_label_repel(data = se_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Southeast Region - Climate Product Production") +
  scale_color_manual(values=c("blue", "red", "magenta3", "chartreuse4","black"),drop=FALSE) +
  scale_fill_manual(values = c("white", "white", "white", "white","grey75"),drop=FALSE) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_shape_manual(values=c(17,15,18,19,3),drop=FALSE) + 
  coord_sf()

ggsave(paste0(OutDir,"SE_region.pdf"), plot = SE_fig,dpi=300,width=10,height=10)






