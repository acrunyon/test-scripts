# Read in BIBE data
#Create df that is 
# Changed units to metric 200414

#setwd(WD_plots)
library(ggplot2)
library(plyr)
library(lubridate)
library(dplyr)
library(forcats)
library(reshape2)
library(zoo)
library(gridExtra)
library(grid)
library(ggrepel)

rm(list=ls())
DataDir <- "C:/Users/arunyon/OneDrive - DOI/CFs_For_Kaylin/MT-DNR-2024/CHCU/input-data/"
load(paste0(DataDir, "Final_Environment.RData"))
OutDir <- "C:/Users/arunyon/Downloads/"

# Threshold percentages for defining Climate futures. Default low/high:  0.25, 0.75
CFLow = 0.25     
CFHigh = 0.75
Range = 30  #Number of years to summarize (should be at least 30)

WB_GCMs
GCM_sub <- WB_GCMs |> filter(CF %in% c("Warm Wet","Hot Dry"))

col.RCP2 = c("blue", "red")
col.RCP3 = c("white","blue", "red")
colors2 <- c("#9A9EE5","#E10720")


########################## TIME SERIES


################### Create climate futures ###################################

#subset by GCM
FM_indiv<-subset(F_annual,GCM %in% GCM_sub$GCM)



################################# TIME-SERIES PLOTS ############################################
# Can do RCP and Individual model plots now, need to discuss quadrant plots

### By RCP
F_annual$emissions[grep("rcp85",F_annual$GCM)] = "RCP 8.5"
F_annual$emissions[grep("rcp45",F_annual$GCM)] = "RCP 4.5"

FM_rcp <- aggregate(cbind(PrcpIn,TavgF)~emissions+Year,F_annual,mean)


a<-ggplot(FM_rcp, aes(x=Year, y=TavgF, group=emissions, colour = emissions)) +
  # geom_ribbon(aes(x=Year,ymin=Tymin,ymax=Tymax), fill="grey",colour="white") +
  geom_line(colour = "black",linewidth=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(emissions), shape = factor(emissions))) +
  # theme(axis.text=element_text(size=20),
  #       axis.text.x=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.title.y=element_text(size=24,vjust=1.0),
  #       plot.title=element_text(size=26,hjust=0),
  #       legend.text=element_text(size=18), legend.title=element_text(size=18),
  #       legend.direction = "vertical",legend.text = element_text(hjust),
  #       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black"),
  #       plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(a)", 
       x = "Year", y = "Temperature (F)") +
  scale_color_manual(name="",values = col.RCP2) +
  scale_fill_manual(name="",values = col.RCP2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FM_indiv$TavgF), max(FM_indiv$TavgF))) +
  guides(color=guide_legend(override.aes = list(size=7)))  
a
b<-ggplot(FM_indiv, aes(x=Year, y=TavgF, group=GCM, colour = GCM)) +
  # geom_ribbon(aes(x=Year,ymin=Tymin,ymax=Tymax), fill="grey",colour="white") +
  geom_line(colour = "black",linewidth=2.5, stat = "identity") +
  geom_line(size = 2, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(GCM), shape = factor(GCM))) +
  # theme(axis.text=element_text(size=20),
  #       axis.text.x=element_blank(),
  #       axis.title.x=element_blank(),
  #       axis.title.y=element_text(size=24,vjust=1.0),
  #       plot.title=element_text(size=26,hjust=0),
  #       legend.text=element_text(size=18), legend.title=element_text(size=18),
  #       legend.direction = "vertical",legend.text = element_text(hjust),
  #       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       panel.background = element_blank(), axis.line = element_line(colour = "black"),
  #       plot.margin = unit(c(0,1,0,1), "cm")) + 
  labs(title = "(a)", 
       x = "Year", y = "Temperature (F)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22)) +
  scale_y_continuous(limits=c(min(FM_indiv$TavgF), max(FM_indiv$TavgF))) +
  guides(color=guide_legend(override.aes = list(size=7)))  
b
