library(sf)
library(ggplot2)
library(tidyverse)
library(rgdal) # used to read world map data
library(rgeos) # to fortify without needing gpclib
library(maptools)
library(scales)
library(ggrepel)

regions <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\CF_maps\\nps-regions.shp")
cf <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Desktop\\MAPR\\centroid_fix.shp")
HI <-  st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Documents\\GIS\\CF_maps\\HI\\hawaii.shp")
PRVI <- st_read("C:\\Users\\gknowlton\\OneDrive - DOI\\Desktop\\PR_VI\\PR_VI.shp")

regions <- st_transform(x=regions,
                        crs = "NAD83")
cf <- st_transform(x=cf,
                   crs = "NAD83")

HI <- st_transform(x = HI,
                   crs = "NAD83")

PRVI <- st_transform(x = PRVI,
                     crs = "NAD83")

cf$Combined_2 <- cf$Combined_2 %>% replace(is.na(.), "none")

cf <- cf %>% 
  mutate(Combined_2 = replace(Combined_2, Combined_2 == "both", "Both")) %>% 
  mutate(Combined_2 = replace(Combined_2, Combined_2 == "none", "None"))

cf$Combined_2 <- as.factor(cf$Combined_2)


cf <- cf %>% filter(STATE != c("AS", "GU"))

cf$col <- "--"
cf_prods <- c("Both", "None", "CFs", "GFIP")

cf$col[which(cf$Combined_2== "Both")]<-"white"
cf$col[which(cf$Combined_2== "CFs")]<-"white"
cf$col[which(cf$Combined_2== "GFIP")]<-"white"
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
  scale_color_manual(values=c("blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "grey75")) +
  scale_shape_manual(values=c(17,15,3)) + 
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/HI_region.pdf", plot = HI_fig,dpi=300)

# PR & USVI 

PRVI_fig <- ggplot() + 
  geom_sf(data = car_region, colour = "black", fill = "wheat2") + 
  geom_point(data = car_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = car_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F ) +
  ggtitle("Puerto Rico / U.S.V.I. - Climate Product Production") +
  scale_color_manual(values=c("green", "black")) + 
  scale_fill_manual(values = c("white", "grey75")) +
  scale_shape_manual(values=c(15,3)) + 
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/PRVI_region.pdf", plot = PRVI_fig,dpi=300)


IM_fig <- ggplot() + 
  geom_sf(data = im_region, colour = "black", fill = "wheat2") + 
  geom_point(data = im_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = im_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Intermountain Region - Climate Product Production") +
  scale_color_manual(values=c("red", "blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "white", "grey75")) +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/IM_region.pdf", plot = IM_fig,dpi=300)


AK_fig <- ggplot() + 
  geom_sf(data = ak_region, colour = "black", fill = "wheat2") + 
  geom_point(data = ak_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2), size = 2.5) +
  #geom_text(data = ak_cf, aes(x = Lon, y = Lat, label = UNIT_CODE), size = 3) +
  geom_label_repel(data = ak_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Alaska Region - Climate Product Production") +
  scale_colour_manual(values=c("blue", "black")) + 
  scale_fill_manual(values = c("white", "grey75")) +
  scale_shape_manual(values=c(17,3)) + 
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/AK_region.pdf", plot = AK_fig, dpi=300)


MW_fig <- ggplot() + 
  geom_sf(data = mw_region, colour = "black", fill = "wheat2") + 
  geom_point(data = mw_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = mw_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 2,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Midwest Region - Climate Product Production") +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_color_manual(values=c("red", "blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "white", "grey75")) +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/MW_region.pdf", plot = MW_fig,dpi=300)


NC_fig <- ggplot() + 
  geom_sf(data = nc_region, colour = "black", fill = "wheat2") + 
  geom_point(data = nc_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) + 
  geom_label_repel(data = nc_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, 
                   segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99), show.legend=F) +
  ggtitle("National Capital Region - Climate Product Production") +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_color_manual(values=c("red", "blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "white", "grey75")) +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/NCap_region.pdf", plot = NC_fig,dpi=300)


NE_fig <- ggplot() + 
  geom_sf(data = ne_region, colour = "black", fill = "wheat2") + 
  geom_point(data = ne_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) +
  geom_label_repel(data = ne_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, 
                   segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99), show.legend=F,) +
  ggtitle("Northeast Region - Climate Product Production") +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_color_manual(values=c("red", "blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "white", "grey75")) +
  
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/NE_region.pdf", plot = NE_fig, dpi=300)

PW_fig <- ggplot() + 
  geom_sf(data = pw_region, colour = "black", fill = "wheat2") + 
  geom_point(data = pw_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2 ), size = 2.5) +
  geom_label_repel(data = pw_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, 
                   segment.color = 'grey50',  max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Pacific West Region - Climate Product Production") +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_color_manual(values=c("red", "blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "white", "grey75")) +
  coord_sf(xlim = c(-126, -110), ylim = c(32, 50)) 

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/PW_region.pdf", plot = PW_fig,dpi=300)


SE_fig <- ggplot() + 
  geom_sf(data = se_region, colour = "black", fill = "wheat2") + 
  geom_point(data = se_cf, mapping = aes(x = Lon, y = Lat, shape = Combined_2, colour = Combined_2), size = 2.5) + 
  geom_label_repel(data = se_cf, aes(x = Lon, y = Lat, label = UNIT_CODE, fill = Combined_2), size = 3,min.segment.length = 0, segment.color = 'grey50', show.legend=F) +
  ggtitle("Southeast Region - Climate Product Production") +
  labs(colour = "Climate Product(s)", shape = "Climate Product(s)") +
  scale_color_manual(values=c("red", "blue", "green", "black")) + 
  scale_fill_manual(values = c("white", "white", "white", "grey75")) +
  coord_sf()

ggsave("C:/Users/gknowlton/OneDrive - DOI/Documents/One-offs/ClimateProducts/SE_region.pdf", plot = SE_fig,dpi=300)






