# library(devtools)
# devtools::install_github("daheelee/timelineS")
# https://github.com/daheelee/timelineS

library(timelineS)
timelineS(mj_life, main = "Life of Michael Jackson",labels=mj_life$Events)

mj_life
#Events   Event_Dates

ccsp <- read.csv("C:/Users/arunyon/Downloads/Events.csv",stringsAsFactors = F)
ccsp$Event_Dates<-as.Date(paste(ccsp$Year,ccsp$Month,"01",sep="-"),format="%Y-%m-%d")
timelineS(ccsp,main="National Park Service\n Climate Change Scenario Planning",labels=ccsp$Events)



