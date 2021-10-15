library(cft)
library(raster)
packageVersion("cft")

aoi <- rgdal::readOGR(system.file("extdata", "windcave.geojson", package = "cft"))

d <- cftdata(aoi = aoi, area_name = "windcave", parameters = "pr", 
             years = c(2020, 2023), models = "GFDL-ESM2M", scenarios = "rcp85")

df <- cft_df(d, ncores = parallel::detectCores()/2)
df

#CCSM4 works - base
#MIRO-ESM works - 2020:2023
#bcc-csm1-1 pr 2020:23 works

# try w/ new spatial obj
"WICA"

nps_centroids <- sf::st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- sf::st_transform(nps_centroids, sf::st_crs("+proj=longlat +datum=NAD83 +no_defs"))
# nps_centroids <- sf::st_transform(nps_centroids, crs(aoi))

nps_centroids

sc = subset(nps_centroids, UNIT_CODE == "SCBL")
scbuff <- sf::st_buffer(sc, 100)
sc2 <- as(scbuff, "Spatial")

d2 <- cftdata(aoi = sc2, 
                     area_name = "SCBL",
                     years = c(2020,2023),
                     models = "GFDL-ESM2M",
                     parameters = "tasmax",
                     scenarios = "rcp85",
                     ncores = parallel::detectCores() / 2)
# dd<-brick(paste(OutDir,"grca/tasmax_grca_CCSM4_r6i1p1_rcp85_macav2metdata_2006_2007_daily.tif",sep="")) 

df2 <- cft_df(d2, ncores = parallel::detectCores() / 2)
df2

#works with bcc-csm1-1-m
#NANs for GFDL-ESM2G and GFDL-ESM2M
#

nps_boundary <- sf::st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary2018/nps_boundary.shp')
# select park
park = subset(nps_boundary, UNIT_CODE == "SCBL")
Sp_park= as(park, "Spatial")

d3 <- cftdata(aoi = Sp_park, 
              area_name = "SCBL",
              years = c(2020,2023),
              models = "bcc-csm1-1-m",
              parameters = "tasmax",
              scenarios = "rcp85",
              ncores = parallel::detectCores() / 2)
# dd<-brick(paste(OutDir,"grca/tasmax_grca_CCSM4_r6i1p1_rcp85_macav2metdata_2006_2007_daily.tif",sep="")) 

df3 <- cft_df(d3, ncores = parallel::detectCores() / 2)
df3


dir="C:/Users/achildress/AppData/Local/Temp/1/RtmpILtFPY/scbl/"
nc = raster::brick(paste0(dir,"tasmax_scbl_inmcm4_r1i1p1_rcp85_macav2metdata_2020_2023_daily.tif"))
plot(nc)



RNetCDF::read.nc("C:/Users/achildress/Downloads/macav2metdata_hi105_20402069_rcp85_vs_19712000_MRI-CGCM3.nc")


