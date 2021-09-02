#############
## Water Balance plots for BIBE
# Uses BOTH monthly and annual data from WB output. One folder for annual and one for monthly,
# each with a .csv of combined output

library(RColorBrewer)
library(plotrix)
library(zoo)		# for rollmean
library(ggplot2)
library(grid)
library(cowplot)
library(reshape2)
library(plyr)
library(dplyr)
library(data.table)
rm(list=ls())

RColorBrewer::display.brewer.all()

rm(list=ls())

setwd('C:/Users/achildress/Documents/RSS/working/YOSE/')
station<-read.csv("Hetch_Hetchy.csv",header=TRUE) #temp is in F, prcp is in inches

station$DATE<-as.Date(paste(station$Year,station$Month,station$Day,sep="-"),
                      format="%Y-%m-%d")

station$YEAR<-format(station$DATE,"%Y")
station$MONTH<-format(station$DATE,"%m")
aggregate(PRCP~YEAR,station,sum,na.rm=TRUE) #prcp seems to be in inches

# create df that has precip + runoff values for complete time period
head(station)
length(which(is.na(station$PRCP)))/length(station$PRCP)  #5% missing
length(which(is.na(station$TMAX)))/length(station$TMAX)  #2% missing
length(which(is.na(station$TMIN)))/length(station$TMIN)  #2% missing
# Longest NA streaks (makes sure not missing whole years / seasons)
is.na.rle<-rle(is.na(station$PRCP)); tapply(is.na.rle$lengths, is.na.rle$values,max)
is.na.rle<-rle(is.na(station$TMAX)); tapply(is.na.rle$lengths, is.na.rle$values,max)
is.na.rle<-rle(is.na(station$TMIN)); tapply(is.na.rle$lengths, is.na.rle$values,max)

aggregate(Day~Year,station,length)

# Use linear approximation to fill data holes
station$Precip.approx<-na.approx(station$PRCP)
station$TMAX[38757]<-station$TMAX[38756]
station$Tmax.approx<-na.approx(station$TMAX)
station$TMIN[38757]<-station$TMIN[38756]
station$Tmin.approx<-na.approx(station$TMIN)

plot(station$DATE, station$Precip.approx, type = "l", col="blue",axes = FALSE, bty = "n", xlab = "", ylab = "")

by.year<-aggregate(cbind(Tmax.approx,Tmin.approx)~Year,station,mean)
by<-aggregate(cbind(Precip.approx)~Year,station,sum)
by.year<-merge(by.year,by[,c("Precip.approx","Year")],by="Year")
rm(by)
by.year$Tmean<-(by.year$Tmax.approx+by.year$Tmin.approx)/2


plot(by.year$Year,by.year$Tmax.approx,main='Mean Tmax',
     xlab="Year",ylab="Days/Year")
lines(by.year$Year,by.year$Tmax.approx)

plot(by.year$Year,by.year$Tmin.approx,main='Mean Tmin',
     xlab="Year",ylab="Days/Year")
lines(by.year$Year,by.year$Tmin.approx)

plot(by.year$Year,by.year$Tmean,main='Mean Tmean',
     xlab="Year",ylab="Days/Year")
lines(by.year$Year,by.year$Tmean)

plot(by.year$Year,by.year$Precip.approx,main='Mean Precip',
     xlab="Year",ylab="Days/Year")
lines(by.year$Year,by.year$Precip.approx)

by.year<-subset(by.year,Year>=1925)

#-------------------------------------------------#
############  Running average plots   #############
#-------------------------------------------------#

rTmin <- rollmean(by.year$Tmin.approx, 10)
rTmax <- rollmean(by.year$Tmax.approx, 10)
rTmean <- rollmean(by.year$Tmean, 10)
rPpt  <- rollmean(by.year$Precip.approx, 10)

by.year$year<-as.numeric(by.year$Year)

rYr = seq(max(by.year$year) - length(rTmin)+1, max(by.year$year)) 

rDat <- data.frame(cbind(rYr, rTmin, rTmax, rTmean, rPpt))
names(rDat)[1] <- "cYr"
rDat$year <- rDat$cYr
yrAvgs <- merge(rDat, by.year, by="year",all=TRUE)

##ggplot
#Colors for running means
##ggplot theme for all plots
#Theme for all plots
PlotTheme = theme_gray() %+replace% 
  theme(plot.title = element_text(size=18, face='bold', hjust=0.5, vjust=0.5),
        axis.text.y = element_text(size = 16, colour="black"),
        axis.title.y = element_text(size = 18, angle = 90, margin=margin(0,5,0,0)),
        axis.text.x = element_text(size = 16, colour="black"),
        axis.title.x = element_text(size = 18, margin=margin(5,0,0,0)),
        legend.position = "none",
        legend.title = element_text(size=16),
        legend.text = element_text(size=16)
  )

theme_set(PlotTheme)
TitleSize = theme_get()$plot.title$size  ##Needed for cowplot layouts
SiteID<-"YOSE-Station"
dpi=600

RMColors = scale_color_manual(name="", values=c("brown", "black", "#3366FF"))

######################################################
######### Regressions

######################  Periods of Analysis  ######################

beginRefYr = 1925
endRefYr = 1970

BeginYr	= 1925   # is this data file or for plots?
EndYr = 2017
dataEndYr = 2018   # needed for rolling mean plot below.  

p1_start  = beginRefYr
p1_end    = endRefYr
p2_start  = endRefYr
p2_end    = EndYr


yrAvgs$tmaxP1 <- yrAvgs$Tmax.approx
yrAvgs$tmaxP1[which(yrAvgs$year < p1_start | yrAvgs$year > p1_end)] = NA

yrAvgs$tminP1 <- yrAvgs$Tmin.approx
yrAvgs$tminP1[which(yrAvgs$year < p1_start | yrAvgs$year > p1_end)] = NA

yrAvgs$tmeanP1 <- yrAvgs$Tmean
yrAvgs$tmeanP1[which(yrAvgs$year < p1_start | yrAvgs$year > p1_end)] = NA

yrAvgs$pptP1 <- yrAvgs$Precip.approx
yrAvgs$pptP1[which(yrAvgs$year < p1_start | yrAvgs$year > p1_end)] = NA

yrAvgs$tmaxP2 <- yrAvgs$Tmax.approx
yrAvgs$tmaxP2[which(yrAvgs$year < p2_start | yrAvgs$year > p2_end)] = NA

yrAvgs$tminP2 <- yrAvgs$Tmin.approx
yrAvgs$tminP2[which(yrAvgs$year < p2_start | yrAvgs$year > p2_end)] = NA

yrAvgs$tmeanP2 <- yrAvgs$Tmean
yrAvgs$tmeanP2[which(yrAvgs$year < p2_start | yrAvgs$year > p2_end)] = NA

yrAvgs$pptP2 <- yrAvgs$Precip.approx
yrAvgs$pptP2[which(yrAvgs$year < p2_start | yrAvgs$year > p2_end)] = NA

### Add color field
yrAvgs$col<-NA
yrAvgs$col[which(yrAvgs$year>= p1_start & yrAvgs$year<= p1_end )]<-"all"
yrAvgs$col[which(yrAvgs$year>= p2_start & yrAvgs$year<= p2_end )]<-"p2"

# regressions for trends
lmTmax <- lm(yrAvgs$Tmax.approx~yrAvgs$cYr)
lmTmaxP1 <- lm(yrAvgs$tmaxP1~yrAvgs$cYr)
lmTmaxP2 <- lm(yrAvgs$tmaxP2~yrAvgs$cYr)

lmTmin <- lm(yrAvgs$Tmin.approx~yrAvgs$cYr)
lmTminP1 <- lm(yrAvgs$tminP1~yrAvgs$cYr)
lmTminP2 <- lm(yrAvgs$tminP2~yrAvgs$cYr)

lmTmean <- lm(yrAvgs$Tmean~yrAvgs$cYr)
lmTmeanP1 <- lm(yrAvgs$tmeanP1~yrAvgs$cYr)
lmTmeanP2 <- lm(yrAvgs$tmeanP2~yrAvgs$cYr)

lmPpt  <- lm(yrAvgs$Precip.approx~yrAvgs$cYr)		
lmPptP1 <- lm(yrAvgs$pptP1~yrAvgs$cYr)		
lmPptP2 <- lm(yrAvgs$pptP2~yrAvgs$cYr)		


# make table of coefficients
probStar <- function(pVal){
  probStar <- "NS"
  if(pVal < 0.05)probStar <- "*"
  if(pVal < 0.01)probStar <- "**"
  if(pVal < 0.001)probStar <- "***"
  probStar
}

lmMetrics <- function(lmout){
  s <- summary(lmout)
  # equ <- as.character(s$call)
  # eq <- equ[2]
  YrCoeff <- s$coefficients[2,1]
  ses <- coef(s)[,"Std. Error"]   # gets intercept & slope
  seSlope <- ses[2]
  probCoeff <- s$coefficients[2,4]
  probSign <- probStar(probCoeff)
  r2 <- s$r.squared
  data.frame(YrCoeff,seSlope,probCoeff, probSign, r2)
}

regsTmax <-  rbind(lmMetrics(lmTmax), lmMetrics(lmTmaxP1), lmMetrics(lmTmaxP2))
regsTmin <-  rbind(lmMetrics(lmTmin), lmMetrics(lmTminP1), lmMetrics(lmTminP2))
regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP1),lmMetrics(lmTmeanP2))
regsPpt <-   rbind(lmMetrics(lmPpt),  lmMetrics(lmPptP1),  lmMetrics(lmPptP2))

perAll <- paste(min(yrAvgs$cYr), max(yrAvgs$cYr), sep="-")
per1 <- paste(p1_start, p1_end, sep="-")
per2 <- paste(p2_start, p2_end, sep="-")
Period <- rep(c(perAll, per1, per2), 4)

lmTable <- cbind( Var=rep(c("Tmax", "Tmin", "Tmean", "Precip"),each=3), Period, rbind(regsTmax, regsTmin, regsTmean, regsPpt))

lmTable$YrCoeff <- lmTable$YrCoeff * 100   # convert to degF(in)/100-yrs
lmTable$seSlope <- lmTable$seSlope * 100
#add units to YrCoeff field
colnames(lmTable) <- c("Var", "Period", "YrCoeff(degF(in)/100yrs)", "seSlope", "probCoeff", "probSign", "r2")

print(lmTable, row.names = F)

write.csv(lmTable, "Station- Regression Table test .csv", row.names=FALSE)


#### OverTmax95
d1 <- ggplot(aes(cYr, Tmax.approx), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmax, ~({}^o*F)))) + xlab("") + ggtitle("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE,colour="blue")+
  geom_line(aes(cYr, rTmax), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  theme(axis.text.y = element_text(size = 20, colour="black"),legend.position="right") +
  scale_colour_manual("", values = c("brown","black", "blue"),
                      labels=c("10-yr running mean","Annual means","Regression trend")) +
  guides(colour = guide_legend(keywidth=4))
d1

d2 <- ggplot(aes(cYr, Tmin.approx), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmin, ~({}^o*F)))) + xlab("") + ggtitle("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE,colour="blue")+
  geom_line(aes(cYr, rTmin), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  theme(axis.text.y = element_text(size = 20, colour="black"),legend.position="right") +
  scale_colour_manual("", values = c("brown","black", "blue"),
                      labels=c("10-yr running mean","Annual means","Regression trend")) +
  guides(colour = guide_legend(keywidth=4))
d2

d3 <- ggplot(aes(cYr, Tmean), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab(expression(paste(Tmean, ~({}^o*F)))) + xlab("") + ggtitle("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE,colour="blue")+
  geom_line(aes(cYr, rTmean), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  theme(axis.text.x=element_blank(),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  theme(axis.text.y = element_text(size = 20, colour="black"),legend.position="right") +
  scale_colour_manual("", values = c("brown","black", "blue"),
                      labels=c("10-yr running mean","Annual means","Regression trend")) +
  guides(colour = guide_legend(keywidth=4))
d3

d4 <- ggplot(aes(cYr, Precip.approx), data=yrAvgs) + geom_line(na.rm=TRUE) + geom_point(na.rm=TRUE) +
  ylab("Precip (in)") + xlab("") + ggtitle("") +
  # geom_text(aes(x=1895, y=350, label = "C")) +
  geom_smooth(method="lm", na.rm=TRUE,colour="blue")+
  geom_line(aes(cYr, rPpt), size=1.5, colour="brown", na.rm=TRUE) + 
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  theme(axis.text.x=element_text(size = 20, colour="black"),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  theme(axis.text.y = element_text(size = 20, colour="black"),legend.position="right") +
  scale_colour_manual("", values = c("brown","black", "blue"),
                      labels=c("10-yr running mean","Annual means","Regression trend")) +
  guides(colour = guide_legend(keywidth=4))
d4


library(gridExtra)
grid.arrange(d1,d2,d3,d4, nrow=4)

g <- arrangeGrob(d1,d2,d3,d4, nrow=4)
ggsave("Trend-lines_panel.png", g,width = 6.5, height = 8.5,dpi=600)

####### OverPr95

d1<- ggplot(yrAvgs, aes(colour=col)) +		#  attach only data frame in ggplot call
  geom_smooth(method = lm, aes(cYr, Tmax.approx,colour="blue"), na.rm=TRUE,se=FALSE) +
  geom_line(aes(cYr, Tmax.approx), na.rm=TRUE,colour="black") + geom_point(aes(cYr, Tmax.approx), na.rm=TRUE,colour="black") +
  xlab("") + ylab(expression(paste(Tmax, ~({}^o*F)))) + ggtitle("") +
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  #	geom_line(aes(cYr, rTmax), colour = 'red', size=1)}  # rolling mean 
  geom_smooth(method = lm, aes(cYr, tmaxP2,colour="dark green"), na.rm=TRUE,se=FALSE) +
  scale_colour_manual("Start year", values = c("blue", "dark green"),
                      labels=c("1925","1970")) +
  theme(legend.text=element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  guides(colour = guide_legend(keywidth=4))
d1

d2<- ggplot(yrAvgs, aes(colour=col)) +		#  attach only data frame in ggplot call
  geom_smooth(method = lm, aes(cYr, Tmin.approx,colour="blue"), na.rm=TRUE,se=FALSE) +
  geom_line(aes(cYr, Tmin.approx), na.rm=TRUE,colour="black") + geom_point(aes(cYr, Tmin.approx), na.rm=TRUE,colour="black") +
  xlab("") + ylab(expression(paste(Tmin, ~({}^o*F)))) + ggtitle("") +
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  #	geom_line(aes(cYr, rTmax), colour = 'red', size=1)}  # rolling mean 
  geom_smooth(method = lm, aes(cYr, tminP2,colour="dark green"), na.rm=TRUE,se=FALSE) +
  scale_colour_manual("Start year", values = c("blue", "dark green"),
                      labels=c("1925","1970")) +
  theme(legend.text=element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  guides(colour = guide_legend(keywidth=4))
d2

d3<- ggplot(yrAvgs, aes(colour=col)) +		#  attach only data frame in ggplot call
  geom_smooth(method = lm, aes(cYr, Tmean,colour="blue"), na.rm=TRUE,se=FALSE) +
  geom_line(aes(cYr, Tmean), na.rm=TRUE,colour="black") + geom_point(aes(cYr, Tmean), na.rm=TRUE,colour="black") +
  xlab("") + ylab(expression(paste(Tmean, ~({}^o*F)))) + ggtitle("") +
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  #	geom_line(aes(cYr, rTmax), colour = 'red', size=1)}  # rolling mean 
  geom_smooth(method = lm, aes(cYr, tmeanP2,colour="dark green"), na.rm=TRUE,se=FALSE) +
  scale_colour_manual("Start year", values = c("blue", "dark green"),
                      labels=c("1925","1970")) +
  theme(legend.text=element_blank(),
        axis.text.x = element_blank(),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  guides(colour = guide_legend(keywidth=4))
d3


d4<- ggplot(yrAvgs, aes(colour=col)) +		#  attach only data frame in ggplot call
  geom_smooth(method = lm, aes(cYr, Precip.approx,colour="blue"), na.rm=TRUE,se=FALSE) +
  geom_line(aes(cYr, Precip.approx), na.rm=TRUE,colour="black") + geom_point(aes(cYr, Precip.approx), na.rm=TRUE,colour="black") +
  xlab("") + ylab("Precip (in)") + ggtitle("") +
  scale_x_continuous(breaks=c(1940, 1960, 1980, 2000)) +
  #	geom_line(aes(cYr, rTmax), colour = 'red', size=1)}  # rolling mean 
  geom_smooth(method = lm, aes(cYr, pptP2,colour="dark green"), na.rm=TRUE,se=FALSE) +
  scale_colour_manual("Start year", values = c("blue", "dark green"),
                      labels=c("1925","1970")) +
  theme(legend.text=element_text(size=16),legend.position="bottom",
        axis.text.x = element_text(size = 20, colour="black"),
        axis.title.x=element_text(size=20,colour="black"), axis.text.y = element_text(size = 20, colour="black"),
        axis.title.y = element_text(size = 20, angle = 90),plot.title=element_text(size=24,hjust=0)) +
  guides(colour = guide_legend(keywidth=4))
d4

grid.arrange(d1,d2,d3,d4, nrow=4)

g <- arrangeGrob(d1,d2,d3,d4, nrow=4)
ggsave("Regression_panel.png", g,width = 6.5, height = 8.5,dpi=600)




