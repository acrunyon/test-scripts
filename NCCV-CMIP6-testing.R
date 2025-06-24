library(terra)
library(sf)
library(dplyr)
library(ncdf4)
library(tidync)
library(tidyr)

rm(list=ls())

################
# extract raw monthly data
nc = tidync("E:/LOCA2/CMIP6-LOCA2_1950-2100_NPS_monthly.nc")

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
# cmip6 = read.csv("E:/LOCA2/LOCA2-monthly-all-units.csv")
units = c("ACAD", "BAND", "CRLA", "CHAT", "DETO")
# 
# c6 = cmip6 %>% filter(SiteID %in% units)

cmip6 = read.csv("E:/LOCA2/LOCA2-test-set.csv")

cmip6 = cmip6 %>% mutate(date = as.Date(date,format="%Y-%m-%d"),
                  year = format(date,"%Y"),
                  period = ifelse(year < 2015, "Historical", "Future"),
                  pr_in = pr / 25.4 * 30, # mm to inches and multiply by 30 days in month
                  tmean_f = (tas * 9/5) + 32) # Celsius to Fahrenheit


hist = cmip6 %>% filter(year> 1980 & year < 2011) %>% 
  select(SiteID, GCM, date, year, pr_in, tmean_f, period) %>% 
  group_by(year, SiteID, GCM) %>% mutate(pr_in_sum = sum(pr_in)) %>% 
  group_by(SiteID, GCM) %>% summarise(tmean = mean(tmean_f),
                                      precip = mean(pr_in_sum))

fut = cmip6 %>% filter(year> 2034 & year < 2066) %>% 
  select(SiteID, GCM, date, year, pr_in, tmean_f, period) %>% 
  group_by(year, SiteID, GCM) %>% mutate(pr_in_sum = sum(pr_in)) %>% 
  group_by(SiteID, GCM) %>% summarise(tmean = mean(tmean_f),
                                      precip = mean(pr_in_sum))


fut_delta = fut %>% left_join(hist, by = c("SiteID", "GCM")) %>% 
  mutate(tmean_delta = tmean.x - tmean.y,
         precip_delta = precip.x - precip.y) %>% 
  select(c(SiteID, GCM, tmean_delta, precip_delta)) %>% 
  mutate(ssp = stringr::str_sub(GCM, -6, -1))


# No color
library(ggplot2)
library(ggrepel)

site = units[5]

loca_df = fut_delta %>% filter(SiteID == site)

cmip5_park = read.csv(paste0("E:/RCF_2024/RCF_opened/", site, "/input-data/", site, "_Future_Means.csv"))

tmean_range = c(min(loca_df$tmean_delta, cmip5_park$DeltaTavg), max(loca_df$tmean_delta, cmip5_park$DeltaTavg))
precip_range = c(min(loca_df$precip_delta, cmip5_park$DeltaPr*365), max(loca_df$precip_delta, cmip5_park$DeltaPr*365))



local_df %>% ggplot(aes(tmean_delta, precip_delta, xmin = quantile(tmean_delta, 0.25),
             xmax = quantile(tmean_delta, 0.75),
             ymin = quantile(precip_delta, 0.25),
             ymax = quantile(precip_delta, 0.75))) +
  geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  geom_point(aes(color=ssp),size=4) + 
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=16,vjust=0.2),
        plot.title=element_text(size=20,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=16), legend.title=element_text(size=16)) + 
  theme_classic() +
  ###
  labs(title =paste(site," Changes in climate means in 2050 by GCM run (CMIP6)",sep=""), 
       x = "Changes in average temp", # Change
       y = "Changes in precip") + #change
  scale_color_manual(name="Scenarios", values=c("blue", "gold", "red")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  geom_rect(color = "black", alpha=0) +
  geom_hline(aes(yintercept=mean(precip_delta)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(tmean_delta)),linetype=2) #change


ggsave(paste0(site, "_CMIP6_ssps.jpg"), path = "C:/Users/arunyon/OneDrive - DOI/Documents/CFs/CMIP6-transition/", height=9, width=12,bg="white")



loca_plot = loca_df %>% ggplot(aes(tmean_delta, precip_delta, xmin = quantile(tmean_delta, 0.25),
                                      xmax = quantile(tmean_delta, 0.75),
                                      ymin = quantile(precip_delta, 0.25),
                                      ymax = quantile(precip_delta, 0.75))) +
geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=16,vjust=0.2),
        plot.title=element_text(size=20,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=16), legend.title=element_text(size=16)) + 
  theme_classic() +
  ###
  labs(title =paste(site," Changes in climate means in 2050 by GCM run (CMIP6)",sep=""), 
       x = "Changes in average temp", # Change
       y = "Changes in precip") + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) +
geom_hline(aes(yintercept=mean(precip_delta)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(tmean_delta)),linetype=2) + #change
  xlim(tmean_range[1], tmean_range[2]) +
  ylim(precip_range[1], precip_range[2])


# No color
maca_plot = ggplot(cmip5_park, aes(DeltaTavg, DeltaPr*365, xmin=quantile(DeltaTavg, 0.25), 
                       xmax = quantile(DeltaTavg, 0.75),
                       ymin = quantile(DeltaPr*365, 0.25),
                       ymax = quantile(DeltaPr*365, 0.75))) +
geom_text_repel(aes(label=GCM)) +
  geom_point(colour="black",size=4) +
  theme(axis.text=element_text(size=16),
        axis.title.x=element_text(size=16,vjust=-0.2),
        axis.title.y=element_text(size=16,vjust=0.2),
        plot.title=element_text(size=20,face="bold",vjust=2,hjust=0.5),
        legend.text=element_text(size=16), legend.title=element_text(size=16)) + 
  theme_classic() +
  ###
  labs(title =paste(site," Changes in climate means in 2050 by GCM run (CMIP5)",sep=""), 
       x = "Changes in average temp", # Change
       y = "Changes in precip") + #change
  scale_color_manual(name="Scenarios", values=c("black")) +
  # scale_fill_manual(name="Scenarios",values = c("black")) + 
  theme(legend.position="none") +
  geom_rect(color = "black", alpha=0) + 
  geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
  geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) + #change
  xlim(tmean_range[1], tmean_range[2]) +
  ylim(precip_range[1], precip_range[2])


g = gridExtra::grid.arrange(maca_plot, loca_plot,nrow=2)

ggsave(paste0(site, "_CMIP5_vs_CMIP6_scatterplot.jpg"), path = "C:/Users/arunyon/OneDrive - DOI/Documents/CFs/CMIP6-transition/", plot = g, height=9, width=12,bg="white")













# PLAY WITH RAW LOCA2 DATA

precip_nc = tidync("E:/LOCA2/raw/pr.ACCESS-CM2.ssp245.r1i1p1f1.2075-2100.LOCA_16thdeg_v20240915.yearly.nc")

time = precip_nc %>% activate("time")
t = time$transforms$time

spatial = terra::rast("E:/LOCA2/raw/pr.ACCESS-CM2.ssp245.r1i1p1f1.2075-2100.LOCA_16thdeg_v20240915.yearly.nc")

terra::plot(spatial$pr_tavg_tavg_1) # var_1:151 for each year 1950-2100; use terra::plot to plot


precip_mean = terra::rast("E:/LOCA2/raw/pr.ACCESS-CM2.ssp245.r1i1p1f1.2075-2100.LOCA_16thdeg_v20240915.yearly.all_mean.nc")
terra::plot(precip_mean$pr_tavg_tavg_tavg)

x = precip_mean$pr_tavg_tavg_tavg * 86400 * 365  / 25.4 #This is correct it just looks weird because it includes BC and desert MX - WICA test was spot on
plot(x$pr_tavg_tavg_tavg)

park = st_transform(park, crs(x))
park360 = lwgeom::st_wrap_x(park, 0, 360) #need to convert from -180 180 to 0 360 to work with LOCA2 objects



park_df = terra::extract(x, park360) 
park_crop = terra::crop(x, park360)




tmax_test = terra::rast("E:/LOCA2/raw/tasmax.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220413.monthly.nc")






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
