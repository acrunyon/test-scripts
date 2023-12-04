CF.folders <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/"

SiteID <- "VALL"

"sessionInfo.txt"

nps_boundary <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == SiteID)
box = sf::st_bbox(park)

http://thredds.northwestknowledge.net:8080/thredds/ncss/ANALOGS/MACAV2/conus_analogs/climate_data/pptannual/macav2metdata_pptannual_20702099_rcp85_bcc-csm1-1.nc?var=pptannual&north=43.64049%20&west=-103.55064&east=-103.33681&south=43.49720&disableLLSubset=on&disableProjSubset=on&horizStride=1&addLatLon=true&accept=netcdf

http://thredds.northwestknowledge.net:8080/thredds/ncss/grid/ANALOGS/MACAV2/conus_analogs/climate_data/maxSWE/macav2metdata_maxSWE_19712000_historical_CMIP5ModelMean.nc/dataset.html

fig <- "C:/Users/arunyon/Downloads/macav2metdata_pptannual_20702099_rcp85_bcc-csm1-1 (1).nc"
# obj <- terra::rast(fig)
obj <-read_ncdf(fig,var="pptannual")
ob<- st_transform(obj,st_crs(park)) 
# crs(obj) <- crs(park)



# ggplot() +
#   geom_spatraster(data = obj) +
#   geom_sf(park[1], mapping = aes(), color = "black", fill = NA) +
#   # Not plotting NAs of the raster
#   scale_fill_continuous(na.value = NA) +
#   labs(fill="pptannual ")

ggplot() +
  geom_stars(data = ob, alpha = 0.8) + 
  geom_sf(park[1], mapping = aes(), color = "black", fill = NA,lwd=1.5) +
  scale_fill_viridis(direction=-1, option = "mako",
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) 
# Not plotting NAs of the raster
# scale_fill_continuous(na.value = NA) +
labs(fill="pptannual ")


# obj_df <- obj |>
#   as.data.frame(xy = TRUE) |> 
#   mutate(x=x-360)

# ggplot() + 
#   geom_raster(data = obj_df ,aes(x = x, y = y,alpha=pptannual), show.legend=FALSE) +
#   # geom_stars(data = data, alpha = 0.8) + 
#   geom_sf(data = park[1], aes(), fill = NA,) 

