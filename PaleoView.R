library(dplyr)
library(ggplot2)
library(ncdf4)
library(stars)
library(raster)
library(zoo)
rm(list=ls())

BP5000 <- nc_open("C:/Users/achildress/Documents/PaleoData/maximum_temperature-5000BP-1989AD.nc")
BP5000

BP5 <- read_ncdf("C:/Users/achildress/Documents/PaleoData/maximum_temperature-5000BP-1989AD.nc")
BP5

plot(BP5000$var$`4000BP-3000BP/3109BP`)

r <- raster("C:/Users/achildress/Documents/mean_temperature-5000BP-1989AD.nc",  
            varname = "4000BP-3000BP/3109BP")
plot(raster::flip(r, direction = "y"))


# Plot paleoview data for RUCA
data<- read.csv("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/RUCA/Paleo_Climate/Combined_data.csv")
data$mean <- (data$Area.Mean*9/5)+32

ggplot(data, aes(Year, mean)) + geom_line(stat="identity",size=1) +
 geom_ribbon(aes(Year,ymin=mean-.8,ymax=mean+.8),colour="yellow",alpha=.5,fill="yellow") +
  theme(axis.text=element_text(size=14),    #Text size for axis tick mark labels
                    axis.title.x=element_blank(),               #Text size and alignment for x-axis label
                    axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
                    plot.title=element_text(size=20,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
                    legend.title=element_text(size=18),                                                                    #Text size of legend category labels
                    legend.text=element_text(size=16),                                                                   #Text size of legend title
                    legend.position = "bottom")  +
  labs(x="Year", y="Average temperature",title="Paleoclimate record for Russel Cave National Monument")+
  scale_x_continuous(breaks=c(-10000, -5000, -2500, 0, 1000,2000))


nclim <- read.csv("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/RUCA/Historical-climate/RUCA-Historical/RUCA_nClimGrid.csv")
load("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/RUCA/CFs/RUCA/input-data/Final_Environment.RData")

# bias correction
PV.bc.mean <- data %>% filter(Year > 1895) %>% pull(mean) %>% mean()

nclim.yr <- nclim %>% mutate(Year = format(as.Date(Date,format="%Y-%m-%d"),format="%Y")) %>% 
  group_by(Year) %>% summarise_at(vars(TavgF), mean)
gridmet.yr <- Baseline_all %>% group_by(Year) %>% summarise_at(vars(TavgF), mean)

nclim.grid.mean <- nclim.yr %>% filter(Year <1978 & Year < 2012) %>% pull(TavgF) %>% mean()
grid.bc.mean <- gridmet.yr %>% pull(TavgF) %>% mean()
diff.grid.nclim <-  nclim.grid.mean - grid.bc.mean

nclim.yr <- nclim.yr %>% mutate(TavgF = TavgF + diff.grid.nclim,
                                data = "NOAA")

PV.nclim.mean <- nclim.yr %>% filter(Year < 1950) %>% pull(TavgF) %>% mean()
diff.nclim.PV <- PV.nclim.mean - PV.bc.mean

Paleo.bc<- data %>% mutate(TavgF = mean + diff.nclim.PV) %>% dplyr::select(Year, TavgF) %>% 
  mutate(data = "Paleoclimate")

CFs <- WB_GCMs %>% filter(CF %in% c("Warm Wet", "Hot Dry"))
CF_annual <- F_annual %>% filter(GCM %in% CFs$GCM) %>% mutate(Tavgroll = rollmean(TavgF,k=30,align="center"))
  dplyr::select(Year, TavgF,CF) %>% rename(data=CF)


all.data <- rbind(Paleo.bc,nclim.yr,CF_annual)
all.data$data <- factor(all.data$data, levels = c("Paleoclimate","NOAA","Warm Wet","Hot Dry"))
all.data$Year <- as.numeric(all.data$Year)
cols<- c("grey","black","#6EB2D4", "#CA0020")

ggplot(all.data, aes(Year, TavgF,col=data,fill=data)) + 
  geom_line(aes(x=Year, y=TavgF),size=1.25,colour="black", na.rm=TRUE) +
  geom_line(aes(x=Year, y=TavgF,colour = data), size=.75 ,na.rm=TRUE) +
  # geom_ribbon(aes(Year,ymin=TavgF-.8,ymax=TavgF+.8),colour="yellow",alpha=.5,fill="yellow") +
  theme(axis.text=element_text(size=14),    #Text size for axis tick mark labels
        axis.title.x=element_blank(),               #Text size and alignment for x-axis label
        axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        plot.title=element_text(size=20,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
        legend.title=element_text(size=18),                                                                    #Text size of legend category labels
        legend.text=element_text(size=16),                                                                   #Text size of legend title
        legend.position = "bottom")  +
  labs(x="Year", y="Average temperature",title="Paleoclimate record for Russel Cave National Monument")+
  scale_color_manual(name="Climate Future",values=cols) +
  scale_x_continuous(breaks=c(-10000, -5000, -2500, 0, 1000,2000,2100))


## Paleoonly
ggplot(Paleo.bc, aes(Year, TavgF)) + geom_line(stat="identity",size=1) +
  # geom_ribbon(aes(Year,ymin=mean-.8,ymax=mean+.8),colour="yellow",alpha=.5,fill="yellow") +
  theme(axis.text=element_text(size=14),    #Text size for axis tick mark labels
        axis.title.x=element_blank(),               #Text size and alignment for x-axis label
        axis.title.y=element_text(size=18, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20)),              #Text size and alignment for y-axis label
        plot.title=element_text(size=20,face="bold",hjust=0.5, margin=margin(t=20, r=20, b=20, l=20)),      #Text size and alignment for plot title
        legend.title=element_text(size=18),                                                                    #Text size of legend category labels
        legend.text=element_text(size=16),                                                                   #Text size of legend title
        legend.position = "bottom")  +
  labs(x="Year", y="Average temperature",title="Paleoclimate record for Russel Cave National Monument")+
  scale_x_continuous(breaks=c(-10000, -5000, -2500, 0, 1000,2000))

min(Paleo.bc$TavgF)
max(Paleo.bc$TavgF)




