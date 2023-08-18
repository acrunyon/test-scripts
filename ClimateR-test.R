# # Testing ClimateR as temporary subsistute for cft()
# # https://github.com/mikejohnson51/climateR
#  
# remotes::install_github("mikejohnson51/climateR")
# remotes::install_github("mikejohnson51/AOI")
library(climateR)
library(AOI)
library(dplyr)
library(ggplot2)

rm(list=ls())

# Needs aoi to run -- dig back through original cft code at how to create aoi from lat/lon
# AOI<-geocode(location = c("Fort Collins"), pt = TRUE) 
AOI<- aoi_get(list(42.735558, -99.743792,.01,.01)) #Coordiantes of east NIOB location

vars = c("tasmax", "tasmin", "pr", "rhsmax", "rhsmin")
# scens = c("rcp45", "rcp85")

start.time <-Sys.time()
future_all <- data.frame()
for (i in 1:length(vars)){
future1 = getMACA(AOI, 
                 model = 2, varname = vars[i], scenario  = "rcp45",
                 startDate = "2050-01-01", endDate = "2050-01-31")
future2 = getMACA(AOI, 
                  model = 2, varname = vars[i], scenario  = "rcp85",
                  startDate = "2050-01-01", endDate = "2050-01-31")
future<- left_join(future1, future2, by="date")

future_long = future |>  
  tidyr::pivot_longer(-date)
FL <- future_long |> 
  mutate(GCM = gsub("^[^_]*_([^_]+)_.*$", "\\1", future_long$name),
         RCP = sub('.*_', '', future_long$name)) |> 
  rename(!!vars[i]:=value) |> select(-c(name))
if(i==1) { future_all = FL } else {
  future_all = left_join(future_all, FL, by=c("date","GCM","RCP"))
  rm(future_long, FL,future1, future2, future)
  }
}
end.time <- Sys.time()
end.time-start.time

future1 = getMACA(AOI, 
                  model = 2, varname = vars[4], scenario  = "rcp45",
                  startDate = "2050-01-01", endDate = "2050-01-31")



# sub("\\_.*", "", future_long$name) #extracts variable name
# sub('.*_', '', future_long$name) #extracts rcp
# gsub("^[^_]*_([^_]+)_.*$", "\\1", future_long$name) #extracts gcm


ggplot(data = future_long, aes(x = date, y = value, col = name)) + 
  geom_line() + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "UCSB Temperture: January, 2050",
       x = "Date",
       y = "Degree K",
       color = "Model")