#This test script reads .nc files sent by K. Hegewich of climate data used in analogs processing
# Can easily make maps for CONUS parks now
# plot coldest tmin, hottest tmax, ann precip
library(ncdf4)
library(stars)
library(dplyr)
library(ggplot2)
library(sf)


st <- read_stars("C:/Users/achildress/Documents/macav2metdata_allMetrics_20402069_rcp85_MIROC-ESM-CHEM.nc") #Specific projection - year mid cen
st
st2 <- read_stars("C:/Users/achildress/Documents/macav2metdata_tasmean_DJF_19712000_historical_20CMIP5ModelMean.nc") #need to read other nc maca file to reset dims b/c not correct in maca data
st_crs(st) = st_crs(st2)
st_dimensions(st) <- st_dimensions(st2)

sth <- read_stars("C:/Users/achildress/Documents/macav2metdata_allMetrics_19712000_historical_MIROC-ESM-CHEM.nc") #historical for comparison
st_crs(sth) = st_crs(st2)
st_dimensions(sth) <- st_dimensions(st2)


nps_boundary <- st_read('C:/Users/achildress/Documents/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == "YELL")
park <- st_transform(park, st_crs(st2))

box = sf::st_bbox(park)

st_crop_delta <- st_crop(st, box) - st_crop(sth,box)


ggplot() + 
  geom_stars(data = st_crop_delta[7], alpha = 0.8) +
  geom_sf(data = park[1], aes(), fill = NA, lwd=2) 
  # scale_fill_viridis(direction=-1, option = scale,
                     # guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) + #mako for WB delta
  labs(title = "title") +
  # theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        plot.title=element_text(size=12,face="bold",hjust=0.5),
        plot.background = element_rect(colour = col, fill=NA, size=5)) 
  labs(fill = metric)
