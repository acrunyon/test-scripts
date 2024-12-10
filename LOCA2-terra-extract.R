# library(ncdf4)
# library(stars)
library(terra)

rm(list=ls())

file <- "C:/Users/arunyon/Downloads/pr.ACCESS-CM2.ssp245.r1i1p1f1.2015-2044.LOCA_16thdeg_v20220519.nc"

# nc <- nc_open(file)
# star <- read_ncdf(file) #returns proxy object because too big

ter <- terra::rast(file)
t <-subset(ter, time(ter) < as.Date("2015-06-15"))

time(t)
t1 <- t[[100]]

v<- values(t1)
df <- terra::as.data.frame(t1, xy = TRUE, na.rm = FALSE) 


