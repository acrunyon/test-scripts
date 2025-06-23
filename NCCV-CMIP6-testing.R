library(terra)
library(sf)
library(dplyr)
library(ncdf4)
library(tidync)
library(tidyr)

rm(list=ls())

################
# extract raw monthly data
nc = tidync("D:/LOCA2/CMIP6-LOCA2_1950-2100_NPS_monthly.nc")

projections = nc %>% activate("ensemble") %>% hyper_tibble() # extracts values of all projections
parks = nc %>% activate("UNIT_CODE") %>% hyper_tibble()
metrics = c("tasmin", "tasmax", "pr", "tas")

for (i in 2:length(metrics)){ # this took a couple of hours to do for all parks
  var = metrics[[i]]
  df = nc %>% activate(metrics[[i]]) %>% hyper_tbl_cube()
  df1 = data.frame(var = df$mets[[metrics[[i]]]])
  df1 = cbind(SiteID = rownames(df1), df1)
  df2 = df1 %>% pivot_longer(!SiteID, names_to = "proj_time", values_to = metrics[[i]]) %>% 
    mutate(GCM = vapply(strsplit(proj_time, "\\."), \(x) paste(x[2:(length(x)-4)], collapse = "."), character(1)), #vapply is fastest way to do this
           date = as.Date(vapply(strsplit(proj_time, "\\."), \(x) paste(tail(x, 3), collapse = "."), character(1)),
                          format = "%Y.%m.%d"))
  if(i ==1){DF = df2} else{
    DF = merge(DF, df2, by=c("SiteID", "proj_time", "GCM", "date"))
  }
}

write.csv(DF, "D:/LOCA2/LOCA2-monthly-all-units.csv")



##################
# extract tresholds for each park unit
thresholds_nc = tidync("E:/LOCA2/CMIP6-LOCA2_Thresholds_1950-2100_NPS_annual.nc")

metrics = thresholds_nc$variable # view variables available
projections = thresholds_nc %>% activate("ensemble") %>% hyper_tibble()
parks = thresholds_nc %>% activate("UNIT_CODE") %>% hyper_tibble()

## tidync works better for extracting values from complex .nc but terra::rast works better for plotting



# maps from thresholds mesh
spatial = terra::rast("E:/LOCA2/spatial/CMIP6-LOCA2_Thresholds_AllModels_grid_CDD/CMIP6-LOCA2_Thresholds_AllModels_grid/CDD/CMIP6-LOCA2_Thresholds_CDD_ACCESS-CM2.ssp245.r1i1p1f1_1950-2100_16thdeg_grid.nc")

terra::plot(spatial$CDD_151) # var_1:151 for each year 1950-2100; use terra::plot to plot


nps_boundary <- st_read('C:/Users/arunyon/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == "WICA")
park = st_transform(park, crs(spatial))


park_df = terra::extract(spatial, park) # extract extracts values and converts to df
park_rast = terra::crop(spatial, park) # crops out shape

mn = mean(park_rast)
plot(mn)
plot(park[1], add=TRUE, col="transparent",border="black",lwd=4)



# Scatterplots

# read csv and subset to ACAD, BAND, CRLA, CHAT, DETO
cmip6 = read.csv("E:/LOCA2/LOCA2-monthly-all-units.csv")
units = c("ACAD", "BAND", "CRLA", "CHAT", "DETO")

c6 = cmip6 %>% filter(SiteID %in% units)

c = c6 %>% mutate(date = as.Date(date,format="%Y-%m-%d"),
                  year = format(date,"%Y"),
                  period = ifelse(year < 2015, "Historical", "Future"))


hist = c %>% filter(year> 1980 & year < 2011) %>% 
  select(SiteID, GCM, date, year, pr, tas, period) %>% 
  group_by(year, SiteID, GCM) %>% mutate(pr_sum = sum(pr)) %>% 
  group_by(SiteID, GCM) %>% summarise(tmean = mean(tas),
                                      precip = mean(pr_sum))

fut = c %>% filter(year> 2034 & year < 2066) %>% 
  select(SiteID, GCM, date, year, pr, tas, period) %>% 
  group_by(year, SiteID, GCM) %>% mutate(pr_sum = sum(pr)) %>% 
  group_by(SiteID, GCM) %>% summarise(tmean = mean(tas),
                                      precip = mean(pr_sum))










# ##### UNUSED CODE
# # from tidync
# nc1 = activate(nc, "tasmin")
# nc_filter = hyper_filter(nc1, time = time < 1000)
# nc_filter = hyper_filter(nc1, UNIT_CODE = UNIT_CODE == "DETO", ensemble = ensemble == "ACCESS-CM2.ssp245.r1i1p1f1" )
# 
# extracted_data <- hyper_tibble(nc_filter)
# t = extracted_data %>% select(time)
# 
# 
# # opening raw ncdf using ncdf4 - works but too complicated to work with
# cmip = ncdf4::nc_open("D:/LOCA2/CMIP6-LOCA2_1950-2100_NPS_monthly.nc")
# cmip$dim$UNIT_CODE$vals
# cmip$dim$time
# cmip$var$tasmin$dim[2]
# 
# ncdf4::ncatt_get(cmip, "tasmin", "units")$value
# 
# extr = ncdf4::ncvar_get(cmip, "tasmin")
# extr[393,,]
# length(cmip$dim$UNIT_CODE$vals)
# 
# 
# # using terra - doesn't work because not spatial objects
# model = rast("D:/Thresholds/CMIP6-LOCA2_Thresholds_AllModels_grid_CWD/CMIP6-LOCA2_Thresholds_AllModels_grid/CWD/CMIP6-LOCA2_Thresholds_CWD_ACCESS-CM2.ssp245.r1i1p1f1_1950-2100_16thdeg_grid.nc")
# 
# nps_boundary <- st_read('./Data/nps_boundary/nps_boundary.shp')
# park <- filter(nps_boundary, UNIT_CODE == "WICA")
# park = st_transform(park, crs(model))
# 
# extract <- extract(model, park) # extracts timeseries data
# crp = crop(model, park) # crops spatial object
# 
# crp
# 
# 
# # using tidync - works really well and what ultimaely went with. this is extra code
# 
# 
# extr = activate(nc, "tasmin") %>% 
#   hyper_filter(UNIT_CODE = UNIT_CODE == "DETO", ensemble = ensemble == "ACCESS-CM2.ssp245.r1i1p1f1" ) %>% 
#   hyper_tbl_cube()
# 
# # extr$mets$tasmin #extracts data
# # as.Date(extr$dims$time, origin = "1950-01-01") #extracts date and sets as date object
# # hyper_dims(nc) #view dimension info
# 
# df = data.frame("tasmin" = extr$mets$tasmin, "Date" = as.Date(extr$dims$time, origin = "1950-01-01"))
# nc1 = nc %>% hyper_tbl_cube() # creates cube of whole object that can extract from
