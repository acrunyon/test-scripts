library(terra)
library(dplyr)
library(ncdf4)

rm(list=ls())

model = rast("D:/Thresholds/CMIP6-LOCA2_Thresholds_AllModels_grid_CWD/CMIP6-LOCA2_Thresholds_AllModels_grid/CWD/CMIP6-LOCA2_Thresholds_CWD_ACCESS-CM2.ssp245.r1i1p1f1_1950-2100_16thdeg_grid.nc")

nps_boundary <- st_read('./Data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == "WICA")
park = st_transform(park, crs(model))

extract <- extract(model, park) # extracts timeseries data
crp = crop(model, park) # crops spatial object

crp


cmip = ncdf4::nc_open("D:/LOCA2/CMIP6-LOCA2_1950-2100_NPS_monthly.nc")
cmip$dim$UNIT_CODE$vals
cmip$dim$time
cmip$var$tasmin$dim[2]


ncdf4::ncatt_get(cmip, "tasmin", "units")$value

extr = ncdf4::ncvar_get(cmip, "tasmin")
extr[393,,]
length(cmip$dim$UNIT_CODE$vals)


library(tidync)
nc = tidync("D:/LOCA2/CMIP6-LOCA2_1950-2100_NPS_monthly.nc")
nc1 = activate(nc, "tasmin")
nc_filter = hyper_filter(nc1, time = time < 1000)
nc_filter = hyper_filter(nc1, UNIT_CODE = UNIT_CODE == "DETO", ensemble = ensemble == "ACCESS-CM2.ssp245.r1i1p1f1" )

extracted_data <- hyper_tibble(nc_filter)
t = extracted_data %>% select(time)

extr = activate(nc, "tasmin") %>% 
  hyper_filter(UNIT_CODE = UNIT_CODE == "DETO", ensemble = ensemble == "ACCESS-CM2.ssp245.r1i1p1f1" ) %>% 
  hyper_tbl_cube()

extr$mets$tasmin
as.Date(extr$dims$time, origin = "1950-01-01")

df = data.frame("tasmin" = extr$mets$tasmin, "Date" = as.Date(extr$dims$time, origin = "1950-01-01"))

# t = hyper_tibble(extr) %>% select(time)  ## This works bu couldn't get it to work from subsetted object so pulls all dates



