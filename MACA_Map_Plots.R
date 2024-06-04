library(terra)
library(dplyr)
library(sf)
library(stringr)
library(tidyverse)
library(basemaps)
library(rasterVis)
# Link to MACA tifs https://climate.northwestknowledge.net/PATH_TO_TIFS/MACAV2METDATA/TIF/
# WB from Tercek http://screenedcleanedsummaries.s3-website-us-west-2.amazonaws.com/
library(ggplot2)
library(tidyterra)
library(ggthemes)
library(viridis)
library(readxl)
library(gcookbook)
library(lemon)
library(grid)
library(gridExtra)
library(ggpubr)

rm(list=ls())

SiteID <- "MORA"
CF.folders <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/MORA_27"
OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/MORA-maps"
tifDir <- "D:/MACA_tiffs/"
CFs <- c("Warm Wet", "Hot Dry")
cols <- c("#2B83BA", "#D7191C")
# CFs <- c("Warm Dry", "Hot Wet") 
# cols <- c("#ABDDA4","#FDAE61")

## Get CF info

## Extract CFs from SessionInfo
# SessionInfo <- read.table(paste0(CF.folders,SiteID,"/SessionInfo.txt"), sep = "^") #for downloaded centroid CFs
SessionInfo <- read.table(paste0(CF.folders,"/SessionInfo.txt"), sep = "^") #for custom or Koppen CFs

# extract GCMs and CFs from 9th row of SessionInfo - extract between parenthesis and convert to vector
GCMs <- regmatches(SessionInfo[9,], regexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]]; GCMs <- gsub("\\(|\\)", "", GCMs)
GCMs <- strsplit(GCMs,split=", ",fixed=TRUE)[[1]]
CF.list <- tail(regmatches(SessionInfo[9,], gregexpr("\\((.*?)\\)", SessionInfo[9,]))[[1]], 1); CF.list <- gsub("\\(|\\)", "", CF.list)
CF.list <- strsplit(CF.list,split=", ",fixed=TRUE)[[1]]

CF.GCM <- data.frame(CF=CF.list, GCM=GCMs)
CF.GCM <- subset(CF.GCM, CF %in% CFs)
CF1 <- CF.GCM |> filter(CF == CFs[1])
CF2 <- CF.GCM |> filter(CF == CFs[2])

nps_boundary <- sf::st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary/nps_boundary.shp')
park <- filter(nps_boundary, UNIT_CODE == SiteID)
buff <- st_buffer(park[1], 2000)

#Get basemaps
x <- basemap_raster(buff, map_service = "esri", map_type = "world_hillshade") #world_street_map great but need simpler; world_imagery too dark;
x_terr <- rast(x)


# read in files from variable and CF names
#Find file name that contains var and CF | historical

# sub("\\..*", "", CF1$GCM) # Extract part before period
# sub('.*\\.', '', CF1$GCM) # Extract part after period

### For each variable
## Temp
# Var
Var = "tasmean_ANN" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Temperature (°F)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(°F)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "temp" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")

CF1.file <- map(c(sub('.*\\.', '', CF1$GCM), sub("\\..*", "", CF1$GCM)), 
    str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

CF2.file <- map(c(sub('.*\\.', '', CF2$GCM), sub("\\..*", "", CF2$GCM)), 
                str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park

Hist.rast <- crop(Hist.rast, buff)
CF1.rast <- crop(CF1.rast, buff)
CF2.rast <- crop(CF2.rast, buff)

# #Tercek plots code -- clip raster to park and make delta files, convert to in
# 
# Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
# CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
# CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)
# 
# CF1.rast <- CF1.rast.temp - Hist.rast
# CF2.rast <- CF2.rast.temp - Hist.rast
# 
# Hist.rast <- Hist.rast/25.4
# CF1.rast <- CF1.rast/25.4
# CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- ggplot() +
  geom_spatraster_rgb(data = x_terr) +
  geom_spatraster(data = Hist.rast, alpha=.8)+
  geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
  # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
  scale_fill_gradientn(colours = div.pal$Hex) +
  labs(title = "Historical") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        legend.title=element_blank(),
        # plot.title=element_blank(),
        plot.title=element_text(size=12,face="bold",hjust=0.5),
        plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
        plot.margin = unit(c(.5,0,0,0), "cm")) + 
  labs(fill =  paste0(units))
Hist.plot
  
  # Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

map.plot <- function(data, title,xaxis,metric,col){
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = seq.pal$Hex, 
                          limits = c(scale.min, scale.max), oob = scales::squish) +
    labs(title = title, fill=metric) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.title=element_blank(),
          # plot.title=element_blank(),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
    # labs(fill =metric) +
    # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

cf1.plot <- map.plot(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
cf2.plot <- map.plot(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                   face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0(SiteID,"-", long.title,".png"), width = 11, height = 11, path = OutDir,bg="white", a)


### Precip
### For each variable
# Var
Var = "pr_ANN" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Precipitation (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(in/year)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "prec" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")

CF1.file <- map(c(sub('.*\\.', '', CF1$GCM), sub("\\..*", "", CF1$GCM)), 
                str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

CF2.file <- map(c(sub('.*\\.', '', CF2$GCM), sub("\\..*", "", CF2$GCM)), 
                str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park

Hist.rast <- crop(Hist.rast, buff)
CF1.rast <- crop(CF1.rast, buff)
CF2.rast <- crop(CF2.rast, buff)

# #Tercek plots code -- clip raster to park and make delta files, convert to in
# 
# Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
# CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
# CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)
# 
# CF1.rast <- CF1.rast.temp - Hist.rast
# CF2.rast <- CF2.rast.temp - Hist.rast
# 
# Hist.rast <- Hist.rast/25.4
# CF1.rast <- CF1.rast/25.4
# CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- ggplot() +
  geom_spatraster_rgb(data = x_terr) +
  geom_spatraster(data = Hist.rast, alpha=.8)+
  geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
  # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
  scale_fill_gradientn(colours = div.pal$Hex) +
  labs(title = "Historical") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        legend.title=element_blank(),
        # plot.title=element_blank(),
        plot.title=element_text(size=12,face="bold",hjust=0.5),
        plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
        plot.margin = unit(c(.5,0,0,0), "cm")) + 
  labs(fill =  paste0(units))
Hist.plot

# Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

map.plot <- function(data, title,xaxis,metric,col){
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = seq.pal$Hex, 
                         limits = c(scale.min, scale.max), oob = scales::squish) +
    labs(title = title, fill=metric) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.title=element_blank(),
          # plot.title=element_blank(),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

cf1.plot <- map.plot(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
cf2.plot <- map.plot(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                       face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0(SiteID,"-", long.title,".png"), width = 11, height = 11, path = OutDir,bg="white", a)


### Tercek Deficit
### For each variable
# Var
Var = "Deficit" #tasmean_ANN, pr_ANN, Deficit
long.title = "Annual Climatic Water Deficit (in/year)" # Annual Temperature (°F), Annual Precipitation (in/year), Annual Climatic Water Deficit (in/year)
units = "(in/year)" #(°F), (in/year)

#Palettes -- using IPCC palettes -- divergent for hist and sequential for deltas
# temp = temp_div , temp_seq
# prcp = prec_div , prec_seq
# snow = cryo_div , cryo_seq
VarType = "prec" #temp, prec, cryo

inName <- "C:/Users/arunyon/3D Objects/Local-files/IPCC colors for R 2023-01-18/continuous_colormaps.xlsx"
SheetNames <- excel_sheets(inName)    # typ use full path to get sheet names (vs file open)
# SheetNames

# first sheet is only figures of color maps
for(i in 2:length(SheetNames)){
  # cat(str_c("i ->  ", i , "   "))
  d1 <- read_xlsx(inName, sheet = SheetNames[i], col_names = c("R", "G", "B"))
  d1$PalName <- SheetNames[i]
  d1 <- na.omit(d1)
  ifelse(i == 2, PalData <- d1, PalData <- rbind(PalData, d1))
}

# head(PalData)
PalData$Hex <- rgb(PalData$R, PalData$G, PalData$B, maxColorValue = 255)

div.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_div"))
seq.pal <- PalData %>% dplyr::filter(PalName == paste0(VarType, "_seq"))

if(Var=="Deficit") {div.pal$Hex<-rev(div.pal$Hex);seq.pal$Hex<-rev(seq.pal$Hex)}
# 
# # test palettes
# hw_plot <- ggplot(heightweight, aes(x = ageYear, y = heightIn, colour = weightLb)) +
#   geom_point(size = 3)
# hw_plot + scale_color_gradientn(colours = div.pal$Hex)

Hist.file <- str_subset(list.files(path=tifDir, pattern = Var), pattern="historical")

CF1.file <- map(c(sub('.*\\.', '', CF1$GCM), sub("\\..*", "", CF1$GCM)), 
                str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

CF2.file <- map(c(sub('.*\\.', '', CF2$GCM), sub("\\..*", "", CF2$GCM)), 
                str_detect, string = list.files(path=tifDir, pattern = Var)) %>%
  reduce(`&`) %>% 
  magrittr::extract(list.files(path=tifDir, pattern = Var), .)

Hist.rast <- rast(paste0(tifDir, Hist.file)) #Read in raster 
CF1.rast <- rast(paste0(tifDir, CF1.file)) #Read in raster 
CF2.rast <- rast(paste0(tifDir, CF2.file)) 

# Need to flip MACA WB data
# Hist.rast <- terra::flip(rast(paste0(tifDir, Hist.file)), direction="vertical") #Read in raster and flip it
# CF1.rast <- terra::flip(rast(paste0(tifDir, CF1.file)), direction="vertical") #Read in raster and flip it
# CF2.rast <- terra::flip(rast(paste0(tifDir, CF2.file)), direction="vertical") #Read in raster and flip it

# # Clip raster to park
# 
# Hist.rast <- crop(Hist.rast, buff)
# CF1.rast <- crop(CF1.rast, buff)
# CF2.rast <- crop(CF2.rast, buff)

#Tercek plots code -- clip raster to park and make delta files, convert to in

Hist.rast <- crop(project(Hist.rast, crs(buff)), buff)
CF1.rast.temp <- crop(project(CF1.rast, crs(buff)), buff)
CF2.rast.temp <- crop(project(CF2.rast, crs(buff)), buff)

CF1.rast <- CF1.rast.temp - Hist.rast
CF2.rast <- CF2.rast.temp - Hist.rast

Hist.rast <- Hist.rast/25.4
CF1.rast <- CF1.rast/25.4
CF2.rast <- CF2.rast/25.4

# Historical plot
Hist.plot <- ggplot() +
  geom_spatraster_rgb(data = x_terr) +
  geom_spatraster(data = Hist.rast, alpha=.8)+
  geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
  # scale_fill_viridis(direction=-1, option = "viridis", oob = scales::squish) + 
  scale_fill_gradientn(colours = div.pal$Hex) +
  labs(title = "Historical") +
  theme_map() +
  theme(legend.position = "bottom",
        legend.key.width = unit(6, "cm"),
        legend.key.height = unit(.3, "cm"),
        legend.justification = "center",
        legend.title=element_blank(),
        # plot.title=element_blank(),
        plot.title=element_text(size=12,face="bold",hjust=0.5),
        plot.background = element_rect(colour = "gray", fill=NA, linewidth=5),
        plot.margin = unit(c(.5,0,0,0), "cm")) + 
  labs(fill =  paste0(units))
Hist.plot

# Future delta plots
scale.min = min(minmax(CF1.rast)[1],minmax(CF2.rast)[1])
scale.max = max(minmax(CF1.rast)[2],minmax(CF2.rast)[2])

map.plot <- function(data, title,xaxis,metric,col){
  ggplot() +
    geom_spatraster_rgb(data = x_terr) +
    geom_spatraster(data = data, alpha=.8)+
    geom_sf(data = park[1], aes(), fill = NA,colour="black",linewidth=1.5) +
    # scale_fill_viridis(direction=1, option = "mako", 
    #                    limits = c(scale.min, scale.max), oob = scales::squish) + 
    scale_fill_gradientn(colours = seq.pal$Hex, 
                         limits = c(scale.min, scale.max), oob = scales::squish) +
    labs(title = title, fill=metric) +
    theme_map() +
    theme(legend.position = "bottom",
          legend.key.width = unit(6, "cm"),
          legend.key.height = unit(.3, "cm"),
          legend.justification = "center",
          legend.title=element_blank(),
          # plot.title=element_blank(),
          plot.title=element_text(size=12,face="bold",hjust=0.5),
          plot.background = element_rect(colour = col, fill=NA, linewidth=5),
          plot.margin = unit(c(.5,0,0,0), "cm"))  
  # labs(fill =metric) +
  # guides(colour = guide_legend(title.position="top", title.hjust = 0.5))
}

cf1.plot <- map.plot(data=CF1.rast, title=CFs[1],metric=paste0("Change in ",units),col=cols[1])
cf2.plot <- map.plot(data=CF2.rast, title=CFs[2],metric=paste0("Change in ",units),col=cols[2])


# Merge into one plot

maps <- grid_arrange_shared_legend(cf1.plot, cf2.plot,ncol = 2, nrow = 1, position="bottom",
                                   top = textGrob(paste0("Change in ",long.title, " (2040-2069)"),
                                                  gp=gpar(fontface="bold", col="black", fontsize=18)))

hist.fig <- annotate_figure(Hist.plot, top = text_grob(paste0("Historical ", long.title), 
                                                       face = "bold", size = 18))

a <- grid.arrange(hist.fig, maps,nrow = 2,  clip = TRUE)
a

if(grepl("/", long.title, fixed = TRUE)) {long.title=gsub("/", "_", long.title)}
ggsave(paste0(SiteID,"-", long.title,".png"), width = 11, height = 11, path = OutDir,bg="white", a)
