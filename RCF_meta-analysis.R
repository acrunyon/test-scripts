library(dplyr)
library(ggplot2)
library(openxlsx)
library(sf)
library(tidyverse);library(flextable);library(ggrepel)
library(gridExtra)
library(grid)

rm(list=ls())
OutDir <- "C:/Users/arunyon/3D Objects/Local-files/Meta-analysis/" #directory for output/plots
DataDir <- "D:/RCF/RCF_opened/"


nps_centroids <- st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/nps_boundary_centroids/nps_boundary_centroids.shp')
ner_centroids <- nps_centroids |> filter(REGION == "NE")
states <-  st_read('C:/Users/arunyon/3D Objects/Local-files/Git-repos/CCRP_automated_climate_futures/data/general/spatial-data/State_Shapefile/States_NAD83_Albers.shp')
states <- st_transform(x=states,crs = "NAD83")
states <- states |> filter(STATE_ABBR %in% ner_centroids$STATE)
states_geometry <- st_geometry(states)



Parks <- ner_centroids$UNIT_CODE
# Parks <- subset to parks in NER
ParkFolders <- paste0(DataDir,Parks,"/")


CFs <- "WarmDry_HotWet/" #WarmWet_HotDry/, WarmDry_HotWet/
CF.abbrev <- "WD-HW" #WW-HD, WD-HW

MasterTable <- data.frame()
for (i in 1:length(Parks)){
  table.name <- paste0(ParkFolders[i],CFs,"tables/",Parks[i],"_",CF.abbrev,"_Plot_data.xlsx")
  if(!file.exists(table.name)) {
    next} else {
  table <- read.xlsx(xlsxFile = table.name,sheet = 1)
  drought <- read.csv(paste0(ParkFolders[i],CFs,"tables/Drought_characteristics.csv"))
  table <- merge(table,drought,by="CF")
  return <- read.csv(paste0(ParkFolders[i],CFs,"tables/precip_recurrence_interval.csv")) |> 
    subset(return==50,select=c(CF,GEV))
  table <- merge(table,return,by="CF")
  table$park <- Parks[i]
  MasterTable <- rbind(MasterTable,table)
    }
}

MasterTable$CF[which(MasterTable$CF == "Warm Dry")] -> "Warm Damp"
MasterTable$CF<-factor(MasterTable$CF,levels=c("Historical","Warm Damp","Hot Wet"))

MeanTable <- aggregate(.~CF,subset(MasterTable, select=-c(per,park)),mean)
MeanTable$CF<-factor(MeanTable$CF,levels=c("Historical","Warm Damp","Hot Wet"))
# MeanTable <- MasterTable |> select(-park)|> group_by(CF) |> summarise_all(.funs = c(mean="mean"))


## Plotting functions
#Height and width 
Yr=2050
BasePeriod = "1979-2012"
MethodCaption="N"
PlotWidth = 9
PlotHeight = 6

colors5 <-  c("#6EB2D4", "#05689F", "#F6B294", "#CA0020","grey")
colors2<- colors5[c(3,2)] # Select pair of climate futures - HotWet/WarmDry
colors3<-c("white",colors2)
col<- c("darkgray", colors2)  # HotWet/WarmDry

#ggplot theme to control formatting parameters for plots with month on the x-axis
PlotTheme = theme(axis.text=element_text(size=14),    #Text size for axis tick mark labels
                  axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                  axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                  plot.title=element_text(size=20,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                  legend.title=element_text(size=18),                                                                    #Text size of legend category labels
                  legend.text=element_text(size=16),                                                                   #Text size of legend title
                  legend.position = "bottom")  

BarPlotTheme = theme(axis.text.x=element_text(size=16),    #Text size for axis tick mark labels
                     axis.text.y=element_text(size=14),
                     axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                     axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                     plot.title=element_text(size=20,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                     legend.position = "none") 

var_bar_plot <- function(data,var, cols, title, ylab,CFmethod=""){ 
  At<-aggregate(eval(parse(text=var))~CF,data=data,mean); 
  names(At)<-c("CF",var) 
  p<-ggplot(At, aes(x=CF,y=(eval(parse(text=var))),fill=CF)) + 
    geom_bar(stat="identity",position="dodge",colour="black") + 
    BarPlotTheme + 
    # coord_cartesian(ylim=c(0, 40)) + 
    labs(title = title,  
         y = ylab,caption=
           if(MethodCaption == "Y"){CFmethod}, colour = "Climate Future")  + 
    scale_fill_manual(name="",values = cols)  
  # annotate(geom="text", x=Inf, y=-Inf, label=CFmethod,color="black",vjust=-1,hjust=1)
  if(min(eval(parse(text=paste("At$",var,sep=""))))<20) {p = p + coord_cartesian(ylim = c(0, max(eval(parse(text=paste("At$",var,sep=""))))))}  
  else{p= p + coord_cartesian(ylim = c(min(eval(parse(text=paste("At$",var,sep=""))))*.9, max(eval(parse(text=paste("At$",var,sep=""))))))} 
  p 
} 

var_bar_plot(MeanTable, "TavgF", cols=colors3, ylab="(\u00B0F)",
             title=paste0("NER-Average annual temperature (\u00B0F)\n in ", Yr, " vs ", BasePeriod),CFmethod = "Q")
ggsave("TavgF-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable, "TminF", cols=colors3, ylab="(\u00B0F)",
             title=paste0("NER-Average annual minimum temperature (\u00B0F)\n in ", Yr, " vs ", BasePeriod),CFmethod = "Q")
ggsave("TminF-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable, "PrcpIn", cols=colors3, ylab="(\u00B0F)",
             title=paste0("NER-Average annual precipitatoin (inches)\n in ", Yr, " vs ", BasePeriod),CFmethod = "Q")
ggsave("PrcpIn-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable, "UnderColdTemp", cols=colors3, ylab="Days/Yr",
             title=paste0("NER-Average Days/Yr < 32 (\u00B0F) in ", Yr, " vs ", BasePeriod),CFmethod="Q")
ggsave("UnderColdTemp-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable, "PrecipOver1", cols=colors3, ylab="Days/Yr",
             title=paste0("NER-Average Days/Yr Precipitation > 1 in. \nin ", Yr, " vs ", BasePeriod),CFmethod="Q")
ggsave("PrecipOver1-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable, "GrowLen", cols=colors3, ylab="Days/Yr",
             title=paste0("NER-Average annual growing season length \nin ", Yr, " vs ", BasePeriod),CFmethod="Q")
ggsave("GrowLen-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable, "Sp.Frost", cols=colors3, ylab="Days/Yr",
             title=paste0("NER-Average annual spring frost days (Tavg>41 & Tmin<32) \nin ", Yr, " vs ", BasePeriod),CFmethod="Q")
ggsave("Sp.Frost-Annual-bar.png", width = PlotWidth, height = PlotHeight, path = OutDir)

var_bar_plot(MeanTable,"Severity", colors3,  "NER-Average Drought Severity", 
             "Severity (Intensity * Duration)",CFmethod="I") + coord_cartesian(ylim = c(0, min(MeanTable$Severity)))
ggsave("DroughtSeverity-Bar.png", path = OutDir, height=PlotHeight, width=PlotWidth)

var_bar_plot(MeanTable,"GEV", cols=colors3, title="NER-50-year extreme precipitation (1:50) events", 
             ylab="Precipitation (inches/day)",CFmethod="I")
ggsave("50yr-PrecipEvent-bar.png", path=OutDir, width = PlotWidth, height = PlotHeight)

## Maps
MasterTable = rename(MasterTable, UNIT_CODE = park)
NE_Historical <- MasterTable |> filter(CF == "Historical")
NE_WD <- MasterTable |> filter(CF == "Warm Damp") |> left_join(NE_Historical, by = "UNIT_CODE") %>% 
  mutate(TavgF_delta = TavgF.x - TavgF.y,
         TmaxF_delta = TmaxF.x - TmaxF.y) %>% 
  select(UNIT_CODE, ends_with("_delta"))
NE_HW <- MasterTable |>filter(CF == "Hot Wet")|> left_join(NE_Historical, by = "UNIT_CODE") %>% 
  mutate(TavgF_delta = TavgF.x - TavgF.y,
         TmaxF_delta = TmaxF.x - TmaxF.y) %>% 
  select(UNIT_CODE, ends_with("_delta"))

WD_centroids <- ner_centroids |> left_join(NE_WD,by="UNIT_CODE")
HW_centroids <- ner_centroids |> left_join(NE_HW,by="UNIT_CODE")

N <- distinct(ner_centroids, UNIT_CODE,.keep_all = T)
N <- extract(N, geometry, into = c('Lat', 'Lon'), '\\((.*),(.*)\\)', conv = T)

WD_plot <- ggplot() + 
  geom_sf(data = states,color="black",fill="tan")  +
  geom_sf(data = WD_centroids,aes(size=TavgF_delta,fill=TavgF_delta),color="black", pch=21) +
  geom_label_repel(data = N, aes(x = Lat, y = Lon, label = UNIT_CODE),
                   size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Warm Damp") + labs(fill="Temperature Change", size="Temperature Change") +
  coord_sf()+
  theme(legend.position="bottom") +
  scale_fill_steps(low = "grey", high = "red")+
  # scale_fill_binned(type = "viridis") +
  guides(fill = guide_legend(nrow = 1,byrow=TRUE)) 

HW_plot <- ggplot() + 
  geom_sf(data = states,color="black",fill="tan")  +
  geom_sf(data = HW_centroids,aes(size=TavgF_delta,fill=TavgF_delta),color="black", pch=21) +
  geom_label_repel(data = N, aes(x = Lat, y = Lon, label = UNIT_CODE),
                   size = 1.5,min.segment.length = 0, segment.color = 'grey50', max.overlaps = getOption("ggrepel.max.overlaps", default = 99),show.legend=F) +
  ggtitle("Hot Wet") + labs(fill="Temperature Change", size="Temperature Change") +
  coord_sf()+
  theme(legend.position="bottom") +
  scale_fill_steps(low = "grey", high = "navy")+
  guides(fill = guide_legend(nrow = 1,byrow=TRUE)) 

g <- grid.arrange(WD_plot,HW_plot,nrow=1,top = "Average annual temperature (degF)")
ggsave(plot=g,"TempPanel.png", path = OutDir, height=9, width=12,bg="white")

