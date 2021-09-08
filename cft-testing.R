library(cft)
library(raster)
packageVersion("cft")

aoi <- rgdal::readOGR(system.file("extdata", "windcave.geojson", package = "cft"))

d <- cftdata(aoi = aoi, area_name = "windcave", parameters = "pr", 
             years = c(2020, 2023), models = "bcc-csm1-1", scenarios = "rcp85")

df <- cft_df(d, ncores = parallel::detectCores()/2)
df

#CCSM4 works - base
#MIRO-ESM works - 2020:2023
#bcc-csm1-1 pr 2020:23 works

# try w/ new spatial obj
nps_centroids <- sf::st_read('C:/Users/achildress/OneDrive - DOI/Documents/GIS/nps_boundary_centroids/nps_boundary_centroids.shp')
nps_centroids <- sf::st_transform(nps_centroids, sf::st_crs("+proj=longlat +datum=NAD83 +no_defs"))

sc <- filter(nps_centroids, UNIT_CODE == "SCBL")
scbuff <- sf::st_buffer(sc, 500)
sc2 <- as(scbuff, "Spatial")

d <- cftdata(aoi = aoi, 
                     area_name = "SCBL",
                     years = c(2020,2023),
                     models = "bcc-csm1-1-m",
                     parameters = "tasmax",
                     scenarios = "rcp85",
                     ncores = parallel::detectCores() / 2)

df <- cft_df(d, ncores = parallel::detectCores() / 2)
df

#works with bcc-csm1-1-m
#NANs for GFDL-ESM2G and GFDL-ESM2M
#
