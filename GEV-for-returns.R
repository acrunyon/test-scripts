# install.packages("extRemes")
library(extRemes)

rm(list=ls())
load("C:/Users/achildress/Documents/RCF_Testing/VAFO/input-data/Final_Environment.RData")
WB_GCMs <- WB_GCMs %>% rowwise() %>% mutate(CF = ifelse(split$PrcpMean>0.5, gsub("Dry","Damp",CF),CF))


FutureSubset <- CFs_all[c(1,5)]; CFs = FutureSubset  # Pick pair of climate futures.
# WB_GCMs <- subset(WB_GCMs, CF %in% CFs)

colors2<- c("slateblue2", "red3") # Select pair of climate futures - WarmWet/HotDry
#colors2<- c("#F3D3CB","#12045C")  # Select pair of climate futures - HotWet/WarmDry

colors3<-c("white",colors2)
col<- c("darkgray",colors2)  # WarmWet/HotDry


##### Testing return period / exceedance probability calculations
exceedance <- function(df, var) { #var name must be in paren
  DF<-df
  DF<-DF[order(-DF[,var]),]
  DF$rank<-seq(1:nrow(DF))
  DF$return<- (nrow(DF)+1)/(DF$rank)
  DF$EP <- 1/DF$return
  DF
}

############### Analysis on MACA data###########################
# Historical data
# Annual max and plot for cF1
Base_max<-aggregate(PrcpIn~Year,Baseline_all,max)

ggplot(Base_max,aes(x=Year,y=PrcpIn)) + geom_point() 

Base_exceedance <-exceedance(Base_max, "PrcpIn")

#lognorm regression
regression<-lm(PrcpIn~log(return),data=Base_exceedance)
Base_exceedance$modeled<-predict(regression)
# GEV regression
regression_GEV = fevd(x = Base_exceedance$PrcpIn, type = "GEV")
plot(regression_GEV)


GEV_return <- function(location, scale, shape, T){
  location+(scale/shape)*(((-log(1-1/T))^-shape)-1)
} # Based on G(z) = 1-(1/T) https://www.dataanalysisclassroom.com/lesson60/

Base_exceedance$GEV <- GEV_return(location = regression_GEV$results$par[[1]], 
           scale = regression_GEV$results$par[[2]], 
           shape = regression_GEV$results$par[[3]],
           T = Base_exceedance$return)

Base_exceedance %>%
ggplot(aes(x=reorder(as.numeric(Year),PrcpIn),y=PrcpIn)) + geom_point(size=4) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
    labs(title = paste(SiteID, " Max annual precipiptation",sep=""),
         x = "Year", y = "Precipitation (inches/day)") +
  # geom_point(aes(x=Year,y=modeled),shape=21,colour="red", size = 2) +
  geom_point(aes(x=Year,y=GEV),shape=22,colour="blue", size = 4)

# mean(Base_exceedance$modeled)-mean(Base_exceedance$PrcpIn)
mean(Base_exceedance$GEV)-mean(Base_exceedance$PrcpIn)

max100base<-data.frame(return=seq(1,100,1))
max100base$modeled<-predict(regression,newdata=max100base) #lognormal
max100base$GEV = GEV_return(location = regression_GEV$results$par[[1]], 
                                scale = regression_GEV$results$par[[2]], 
                                shape = regression_GEV$results$par[[3]],
                                T = max100base$return)
max100base$GCM<-"Historical"

ggplot()+
  geom_point(max100base,mapping=aes(x=return,y=GEV),shape=22,colour="blue", size = 4) +
  geom_point(Base_exceedance, mapping=aes(x=return,y=PrcpIn),size=4) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " precipitation return events",sep=""),
       x = "Return", y = "Precipitation (inches/day)") 

return50base<-data.frame(return=50)
return50base$modeled<-predict(regression,newdata=return50base)
return50base$GEV = GEV_return(location = regression_GEV$results$par[[1]], 
                  scale = regression_GEV$results$par[[2]], 
                  shape = regression_GEV$results$par[[3]],
                  T = return50base$return)
return50base$GCM<-"Historical"

### CFs future 
AF<-merge(ALL_FUTURE,Future_Means[,c("GCM","CF")],by="GCM")
Future_subset <- subset(AF, GCM %in% WB_GCMs$GCM & CF %in% CFs)
Future_split <- aggregate(PrcpIn~Year+CF+GCM,Future_subset,max)

# 
ggplot(Future_split,aes(x=as.numeric(Year),y=PrcpIn,group=CF, colour = CF)) +  geom_line(size = 1.5, stat = "identity") +
  geom_point(colour= "black", size=4, aes(fill = factor(CF), shape = factor(CF))) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " Max annual precip by GCM",sep=""),
       x = "Year", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors2) +
  scale_fill_manual(name="",values = colors2) +
  scale_shape_manual(name="",values = c(21,22))

Future_GCM<-split(Future_split,Future_split$GCM)

future_exceedance <- list()
for (i in 1:length(Future_GCM)){
  fe <- exceedance(Future_GCM[[i]],"PrcpIn")
  fe$GCM = Future_GCM[[i]]$GCM
  future_exceedance[[i]] <- fe
}

future_exceedance<-ldply(future_exceedance,data.frame)


#modeled results
Future_GCM <- split(future_exceedance,future_exceedance$GCM)
max100future <- data.frame()
return50future <- data.frame()

for (i in 1:length(Future_GCM)){
  gcm = unique(Future_GCM[[i]]$GCM)
  regression = lm(PrcpIn~log(return),data=Future_GCM[[i]])
  regression_GEV = fevd(x = Future_GCM[[i]]$PrcpIn, type = "GEV")
  Future_GCM[[i]]$modeled = predict(regression)
  mf <- data.frame(return=seq(1,100,1))
  mf$modeled<-predict(regression,newdata=mf)
  mf$GEV <- GEV_return(location = regression_GEV$results$par[[1]], 
                                    scale = regression_GEV$results$par[[2]], 
                                    shape = regression_GEV$results$par[[3]],
                                    T = mf$return)
  mf$GCM <- gcm
  max100future <- rbind(max100future,mf)
  
  rf<-data.frame(return=50)
  rf$modeled<-predict(regression,newdata=rf)
  rf$GEV <- GEV_return(location = regression_GEV$results$par[[1]], 
                       scale = regression_GEV$results$par[[2]], 
                       shape = regression_GEV$results$par[[3]],
                       T = rf$return)
  rf$GCM <- gcm
  return50future <- rbind(return50future, rf)
  rm(mf,rf)
}

######################################################

####bar plot of returns

#bind the return intv data together
return50base$CF <- "Historical"
return50future <- merge(return50future, WB_GCMs, by="GCM")
allreturns<-rbind(return50base, return50future)
allreturns$CF<-factor(allreturns$CF, levels=c("Historical",CFs))

#Bar graph 50-year return int for a 24-hour event
var_bar_plot(allreturns,"GEV", cols=colors3, title="Extreme precipitation 50-year recurrence interval", 
             ylab="Precipitation (inches/day)")
ggsave("Bar-50yrRecurrence.png", path=FigDir, width = PlotWidth, height = PlotHeight)


######line plot of return int regressions

#bind the regressions lines into one df
max100base$CF <- "Historical"
max100future <- merge(max100future, WB_GCMs, by="GCM")
allregressions<-rbind(max100base, max100future)
allregressions$CF<-factor(allregressions$CF, levels=c("Historical",CFs))

#all observed in one df
Base_exceedance
Base_exceedance<- add_column(Base_exceedance, CF = "Historical", .after = "Year")
exceedance <- rbind(Base_exceedance[,c("CF","PrcpIn","return")],future_exceedance[,c("CF","PrcpIn","return")])

#line plots of regressions
ggplot() +
  # allregressions, aes(x=return, y=GEV, group=CF, colour = CF)) +
  # geom_line(allregressions, mapping=aes(x=return, y=GEV), size = 2, colour="black",stat="identity") +
  # geom_line(allregressions, mapping=aes(x=return, y=GEV),size = 1.5) 
  geom_point(allregressions, mapping=aes(x=return, y=GEV, fill=factor(CF),shape=factor(CF)),colour= "black", size=4) +
  geom_point(exceedance, mapping=aes(x=return, y=PrcpIn, fill=factor(CF),shape=factor(CF)),colour= "black", size=6) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) + 
    labs(title = paste(SiteID, " - Recurrence intervals for 24-hour precipitation totals",sep=""),
       x = "Recurrence interval (year)", y = "Precipitation (inches/day)") +
  scale_color_manual(name="",values = colors3) +
  scale_fill_manual(name="",values = colors3) +
  scale_shape_manual(name="",values = c(21,22,23))
ggsave("line-recurrence-interval-curve.png", path=FigDir, width = PlotWidth, height = PlotHeight)

write.csv(allregressions, paste0(TableDir,"precip_recurrence_interval.csv"),row.names = FALSE)

ggplot()+
  geom_point(max100base,mapping=aes(x=return,y=GEV),shape=22,colour="blue", size = 4) +
  geom_point(Base_exceedance, mapping=aes(x=return,y=PrcpIn),size=4) +
  PlotTheme + theme(axis.title.x=element_text(size=24, vjust=0.5,  margin=margin(t=20, r=20, b=20, l=20))) +
  labs(title = paste(SiteID, " precipitation return events",sep=""),
       x = "Return", y = "Precipitation (inches/day)") 