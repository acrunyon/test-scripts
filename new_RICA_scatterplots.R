library(dplyr)
library(ggplot2)

rm(list=ls())

data <- read.csv("C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/ALL_FUTURE_MEANS.csv")
OutDir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/new_scatterplots/"

parks <- unique(data$SiteID)

CFs1 <- c("Warm Wet", "Hot Dry")
CFs2 <- c("Warm Dry", "Hot Wet")

cols1 <- c("#76B8E0", "#D7191C")
cols2 <- c("#FDAE61", "#4DAA40")

for(i in 1:length(parks)){
  park <- parks[i]
  Future_Means <- subset(data,SiteID==park)
  
  ## WARM WET HOT DRY
  dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365))
    dualscatter  + 
    geom_point(colour="black",size=4) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[1])]), y=mean(365*DeltaPr[which(select==CFs1[1])])), shape=21, size=10, stroke=3, colour=cols1[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[2])]), y=mean(365*DeltaPr[which(select==CFs1[2])])), shape=21, size=10, stroke=3, colour=cols1[2]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[1])]), y=mean(365*DeltaPr[which(select==CFs1[1])])), shape=20, size=2,  colour=cols1[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[2])]), y=mean(365*DeltaPr[which(select==CFs1[2])])), shape=20, size=2,  colour=cols1[2]) +
    annotate("text", x = mean(Future_Means$DeltaTavg[which(Future_Means$select==CFs1[1])]), 
             y = mean(365*Future_Means$DeltaPr[which(Future_Means$select==CFs1[1])]), label = "Warm Wet", size=5,vjust=-1,hjust=1) +
    annotate("text", x = mean(Future_Means$DeltaTavg[which(Future_Means$select==CFs1[2])]), 
             y = mean(365*Future_Means$DeltaPr[which(Future_Means$select==CFs1[2])]), label = "Hot Dry", size=5,vjust=-1,hjust=1) +
    theme(axis.text=element_text(size=18),
          axis.title.x=element_text(size=18,vjust=-0.2),
          axis.title.y=element_text(size=18,vjust=0.2),
          plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
          legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
    ###
    labs(title =paste(park," Changes in climate means in 2050 by GCM run",sep=""), 
         x = "Changes in annual average temperature (\u00B0F)", # Change
         y = "Changes in annual average precipitation (in)") + #change
    scale_color_manual(name="Scenarios", values=c("black")) +
    # scale_fill_manual(name="Scenarios",values = c("black")) + 
    theme(legend.position="none") 
  
  ggsave(paste0(park,"WW-HD-scatter.png"), width = 9, height = 6, path = OutDir)
  
  
  ### WARM DRY HOT WET
  dualscatter = ggplot(Future_Means, aes(DeltaTavg, DeltaPr*365))
    dualscatter  + 
    geom_point(colour="black",size=4) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[1])]), y=mean(365*DeltaPr[which(select==CFs2[1])])), shape=21, size=10, stroke=3, colour=cols2[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[2])]), y=mean(365*DeltaPr[which(select==CFs2[2])])), shape=21, size=10, stroke=3, colour=cols2[2]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[1])]), y=mean(365*DeltaPr[which(select==CFs2[1])])), shape=20, size=2,  colour=cols2[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[2])]), y=mean(365*DeltaPr[which(select==CFs2[2])])), shape=20, size=2,  colour=cols2[2]) +
    annotate("text", x = mean(Future_Means$DeltaTavg[which(Future_Means$select==CFs2[1])]), 
             y = mean(365*Future_Means$DeltaPr[which(Future_Means$select==CFs2[1])]), label = "Warm Dry", size=5,vjust=-1,hjust=1) +
    annotate("text", x = mean(Future_Means$DeltaTavg[which(Future_Means$select==CFs2[2])]), 
             y = mean(365*Future_Means$DeltaPr[which(Future_Means$select==CFs2[2])]), label = "Hot wet", size=5,vjust=-1,hjust=1) +
    theme(axis.text=element_text(size=18),
          axis.title.x=element_text(size=18,vjust=-0.2),
          axis.title.y=element_text(size=18,vjust=0.2),
          plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
          legend.text=element_text(size=18), legend.title=element_text(size=16)) + 
    ###
    labs(title =paste(park," Changes in climate means in 2050 by GCM run",sep=""), 
         x = "Changes in annual average temperature (\u00B0F)", # Change
         y = "Changes in annual average precipitation (in)") + #change
    scale_color_manual(name="Scenarios", values=c("black")) +
    # scale_fill_manual(name="Scenarios",values = c("black")) + 
    theme(legend.position="none") 
  
  ggsave(paste0(park,"WD-HW-scatter.png"), width = 9, height = 6, path = OutDir)
}
