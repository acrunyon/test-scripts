library(terra)
library(dplyr)
library(sf)
library(stringr)
library(tidyverse)

rm(list=ls())


SiteID <- "VALL"
CF.folders <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/"
OutDir <- "C:/Users/arunyon/Downloads/"
tifDir <- "C:/Users/arunyon/Downloads/"
CFs <- c("Warm Wet", "Hot Dry") 
# CFs <- c("Warm Dry", "Hot Wet") 

## Extract CFs from SessionInfo
SessionInfo <- read.table(paste0(CF.folders,SiteID,"/SessionInfo.txt"), sep = "^")

# extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]

CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
CF.GCM <- subset(CF.GCM, CF %in% CFs)
CF1 <- CF.GCM |> filter(CF == CFs[1])
CF2 <- CF.GCM |> filter(CF == CFs[2])

nps_boundary <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == SiteID)
box = sf::st_bbox(park)

# read in files from variable and CF names
# Var
Var = "runoff_ANN"

#Find file name that contains var and CF | historical

# sub("\\..*", "", CF1$GCM) # Extract part before period
# sub('.*\\.', '', CF1$GCM) # Extract part after period

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")

CF1.file <- map(c(sub('.*\\.', '', CF1$GCM), sub("\\..*", "", CF1$GCM)), 
    str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

CF2.file <- map(c(sub('.*\\.', '', CF2$GCM), sub("\\..*", "", CF2$GCM)), 
                str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# Clip raster to park
b <- st_buffer(park[1], 2000)
r <- crop(Hist.rast, b)
plot(r)
plot(park[1],add=T)

# plot

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

