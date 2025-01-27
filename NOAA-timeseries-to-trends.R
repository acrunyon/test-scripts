library(dplyr)
library(ggplot2)
rm(list=ls())

data <- read.csv("C:/Users/arunyon/OneDrive - DOI/AKR-CFs/NCEI_NWAK Borough.csv")


head(data)
#new df that drops top 3 rows and renames create new dat object

df <- data[5:nrow(data),1:2] |> rename("value"=1, "anomaly"=2) |> 
  tibble::rownames_to_column(var="yrmon") |> mutate(year = stringr::str_sub(yrmon,1,4),
                                                    month = stringr::str_sub(yrmon,5,6)) |> 
  mutate_if(is.character,as.numeric)

yrAvgs <- aggregate(value~year,df,mean)
yrAvgs$tmeanP2 = if_else(yrAvgs$year<1970,NA,yrAvgs$value)

lmTmean <- lm(value~year,yrAvgs)
lmTmeanP2 <- lm(tmeanP2~year,yrAvgs)


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


regsTmean <- rbind(lmMetrics(lmTmean),lmMetrics(lmTmeanP2))


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

yrAvgs  |> 
  ggplot() + geom_line(aes(year, value), na.rm=TRUE) + geom_point(aes(year, value), na.rm=TRUE) +
  ylab(expression(paste("Avg Temperature", ~({}^o*F)))) + xlab("") +
  labs(title = "Northwest Arctic Borough Average Annual Temperature") +
  # geom_text(aes(x=1895, y= 13.5, label = "B")) +
  geom_smooth(aes(year, value),se=F, method="lm", na.rm=TRUE,linetype=if(summary(lmTmean)$coefficients[2,4]<0.05) {
    1
  } else{2}) +
  # geom_line(aes(Year, rTmean), colour = 'brown', size=1) +
  scale_x_continuous(breaks=c(1920, 1940, 1960, 1980, 2000, 2020)) +
  geom_smooth(method = lm,se=F, aes(year, tmeanP2), na.rm=TRUE,colour="brown",linetype=if(summary(lmTmeanP2)$coefficients[2,4]<0.05){
    1
  } else{2}) 

ggsave("NW-Arctic-Tmean.png",bg="white",width=12,height=6)
