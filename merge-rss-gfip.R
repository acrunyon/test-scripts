library(tidyr)
library(dplyr)
library(stars)

centroids <- st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids_2021/nps_boundary_centroids.shp')

RSS_parks<-read.csv("C:/Users/achildress/OneDrive - DOI/CF_management/CF_product_maps/RSS_table_220505.csv")
GFIP_parks<-read.csv("C:/Users/achildress/OneDrive - DOI/CF_management/CF_product_maps/GFIP_table_220505.csv")

combined <- merge(GFIP_parks,RSS_parks,by="UNIT_CODE",all=T)

c <- combined %>% replace(is.na(.), 0) %>% 
  mutate(both = GFIP*CFs) %>% 
  mutate(GFIP = ifelse(both==1,NA,ifelse(GFIP==0,NA,GFIP)),
         CFs = ifelse(both==1,NA,ifelse(CFs==0,NA,CFs)),
         both = ifelse(both==0,NA,both)) %>% 
  gather(source,value,GFIP:both,na.rm=TRUE)

write.csv(c,"C:/Users/achildress/OneDrive - DOI/CF_management/CF_product_maps/combined.csv",row.names=TRUE)
