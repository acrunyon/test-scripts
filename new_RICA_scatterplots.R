library(dplyr)
library(ggplot2)
library(stringr)

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
  
  Pr25 = as.numeric(quantile(Future_Means$DeltaPr, 0.25))
  Pr75 = as.numeric(quantile(Future_Means$DeltaPr, 0.75))
  Tavg25 = as.numeric(quantile(Future_Means$DeltaTavg, 0.25)) 
  Tavg75 = as.numeric(quantile(Future_Means$DeltaTavg, 0.75))
  
  ## WARM WET HOT DRY
  FM <- Future_Means
  FM$select[which(!FM$select %in% CFs1)] = NA
  dualscatter = ggplot(FM, aes(DeltaTavg, DeltaPr*365,xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
    dualscatter  + 
    geom_point(colour="black",size=4) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[1])]), y=mean(365*DeltaPr[which(select==CFs1[1])])), shape=21, size=10, stroke=3, colour=cols1[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[2])]), y=mean(365*DeltaPr[which(select==CFs1[2])])), shape=21, size=10, stroke=3, colour=cols1[2]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[1])]), y=mean(365*DeltaPr[which(select==CFs1[1])])), shape=20, size=2,  colour=cols1[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs1[2])]), y=mean(365*DeltaPr[which(select==CFs1[2])])), shape=20, size=2,  colour=cols1[2]) +
    ggrepel::geom_text_repel(aes(label=select),size=5) +
      # geom_rect(color = "black", alpha=0) +
      geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
      geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) +
      theme_classic() +
    theme(axis.text=element_text(size=18),
          axis.title.x=element_text(size=18,vjust=-0.2),
          axis.title.y=element_text(size=18,vjust=0.2),
          plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
          legend.position="none") + 
    ###
    labs(title =paste(park," Changes in climate means in 2050 by GCM run",sep=""), 
         x = "Changes in annual average temperature (\u00B0F)", # Change
         y = "Changes in annual average precipitation (in)") + #change
    scale_color_manual(name="Scenarios", values=c("black")) 

  
  ggsave(paste0(park,"WW-HD-scatter.png"), width = 9, height = 6, path = OutDir)
  
  
  ### WARM DRY HOT WET
  FM <- Future_Means
  FM$select[which(!FM$select %in% CFs2)] = NA
  dualscatter = ggplot(FM, aes(DeltaTavg, DeltaPr*365,xmin=Tavg25, xmax=Tavg75, ymin=Pr25*365, ymax=Pr75*365))
    dualscatter  + 
    geom_point(colour="black",size=4) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[1])]), y=mean(365*DeltaPr[which(select==CFs2[1])])), shape=21, size=10, stroke=3, colour=cols2[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[2])]), y=mean(365*DeltaPr[which(select==CFs2[2])])), shape=21, size=10, stroke=3, colour=cols2[2]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[1])]), y=mean(365*DeltaPr[which(select==CFs2[1])])), shape=20, size=2,  colour=cols2[1]) +
    geom_point(aes(x=mean(DeltaTavg[which(select==CFs2[2])]), y=mean(365*DeltaPr[which(select==CFs2[2])])), shape=20, size=2,  colour=cols2[2]) +
    ggrepel::geom_text_repel(aes(label=select),size=5) +
      # geom_rect(color = "black", alpha=0) +
      geom_hline(aes(yintercept=mean(DeltaPr*365)),linetype=2) + #change
      geom_vline(aes(xintercept=mean(DeltaTavg)),linetype=2) +
      theme_classic() +
    theme(axis.text=element_text(size=18),
          axis.title.x=element_text(size=18,vjust=-0.2),
          axis.title.y=element_text(size=18,vjust=0.2),
          plot.title=element_text(size=18,face="bold",vjust=2,hjust=0.5),
          legend.position="none") + 
    ###
    labs(title =paste(park," Changes in climate means in 2050 by GCM run",sep=""), 
         x = "Changes in annual average temperature (\u00B0F)", # Change
         y = "Changes in annual average precipitation (in)") + #change
    scale_color_manual(name="Scenarios", values=c("black")) 
  
  ggsave(paste0(park,"WD-HW-scatter.png"), width = 9, height = 6, path = OutDir)
}

tabledir <- "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/CFnames/"
library(ggpubr)

colpal <- c("#2B83BA", "#D7191C","#FDAE61", "#ABDDA4") #RColorBrewer Spectral
for(i in 1:length(parks)){
  park <- parks[i]
  Future_Means <- subset(data,SiteID==park)
    Selected.CFs<- Future_Means |> tidyr::drop_na(select) |> select(c(select,GCM)) |> 
  arrange(match(select, c("Warm Wet", "Hot Dry", "Warm Dry", "Hot Wet"))) |> rename("Climate Future"=1, "Projection"=2) 
# df <- data.table::data.table("Climate Future" = Selected.CFs$select,"Projection"= Selected.CFs$GCM)

ggtab<- ggtexttable(Selected.CFs, rows=NULL,
            theme = ttheme(
              colnames.style = colnames_style(fill = "white"),
              tbody.style = tbody_style(fill = colpal)))
  png(paste0(tabledir,park, "-CF-table.png"), width = 3.5, height = 1.5, units = 'in', res = 300); plot(ggtab); dev.off()
}

