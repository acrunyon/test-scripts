# library(devtools)
# devtools::install_github("daheelee/timelineS")
# https://github.com/daheelee/timelineS

library(timelineS)
# timelineS(mj_life, main = "Life of Michael Jackson",labels=mj_life$Events)
# 
# mj_life
#Events   Event_Dates

ccsp <- read.csv("C:/Users/arunyon/Downloads/Events.csv",stringsAsFactors = F)
ccsp$Event_Dates<-as.Date(paste(ccsp$Year,ccsp$Month,"01",sep="-"),format="%Y-%m-%d")
timelineS(ccsp,main="National Park Service\n Climate Change Scenario Planning",cex=1.5,labels=ccsp$Events,buffer.days=0,
          scale.font=2,scale.cex=2,label.cex=1.3,label.direction = "downup",label.angle = 90)

dev.copy(device = png, filename = 'C:/Users/arunyon/Downloads/timeline.png', width = 1500, height = 600); dev.off ()

##!! Update using this https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
# More customizable
