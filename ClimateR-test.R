# Testing ClimateR as temporary subsistute for cft()
# https://github.com/mikejohnson51/climateR
 
remotes::install_github("mikejohnson51/climateR")
remotes::install_github("mikejohnson51/AOI")
library(climateR)
library(AOI)



# Needs aoi to run -- dig back through original cft code at how to create aoi from lat/lon
AOI<-geocode(location = c("Fort Collins"), pt = TRUE) 
future = getMACA(AOI, 
                 model = 5, varname = "tasmax", 
                 startDate = "2050-01-01", endDate = "2050-01-31")
#> Spherical geometry (s2) switched off
#> Spherical geometry (s2) switched on
x<-as.data.frame(future)
future_long = future %>% 
  dplyr::select(-source, -lat, -lon) %>% 
  tidyr::pivot_longer(-date) 

ggplot(data = future_long, aes(x = date, y = value, col = name)) + 
  geom_line() + 
  theme_linedraw() + 
  scale_color_brewer(palette = "Dark2") + 
  labs(title = "UCSB Temperture: January, 2050",
       x = "Date",
       y = "Degree K",
       color = "Model")