library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())

WBData <- read.csv("C:/Users/achildress/Documents/RCF_Testing/VAFO-0/input-data/VAFO_water_balance_historical.csv")

WBData <- WBData %>% mutate(ppt.in = rain.in + accumswe.in,
                            Month =  format(as.Date(Date,format="%Y-%m-%d"),"%m"),
                            Year =  format(as.Date(Date,format="%Y-%m-%d"),"%Y"))

WBMonthly <- WBData %>% group_by(GCM,Year,Month) %>% 
  summarise(AET.in = sum(AET.in),
            PET.in = sum(PET.in),
            Ppt.in = sum(ppt.in)) %>% 
  group_by(GCM,Month) %>% 
  summarise(AET.in = mean(AET.in),
            PET.in = mean(PET.in),
            Ppt.in = mean(Ppt.in)) %>% 
  mutate(Deficit = PET.in - AET.in,
         Utilization = AET.in,
         Surplus = Ppt.in - AET.in)
WBMonthly

WBMonthlyLong <- WBMonthly %>% select(.,-c("Deficit","Surplus","Utilization")) %>% 
  gather(Variable, water, -c(GCM, Month)) 
WBMonthlyLong$Variable <- factor(WBMonthlyLong$Variable,levels = c("Ppt.in","PET.in","AET.in"))

ggplot(WBMonthly) +
  geom_ribbon(aes(Month, ymin = PET.in, ymax=Ppt.in,fill="Surplus/Runoff",group="GCM"),linetype = 0, alpha=1) +
  geom_ribbon(aes(Month, ymin = AET.in, ymax=PET.in,fill="Deficit",group="GCM"),linetype = 0,alpha=1) +
  geom_ribbon(aes(Month, ymin = 0, ymax=AET.in,fill="Utilization",group="GCM"),linetype = 0,alpha=1) +
  geom_line(data = WBMonthlyLong, aes(x=Month, y = water, group=Variable, linetype = Variable), size = 1.5, stat = "identity",colour="black") +
  scale_fill_manual("",
                    values=c('Surplus/Runoff'="cornflowerblue",'Utilization'="palegreen3",'Deficit'="brown1"))


## Hung up on where 'storage' comes from in example plot

WBMonthlyLong <- WBMonthly %>% mutate(Deficit = -Deficit, Surplus = -Surplus) %>% 
  select(.,-c("PET.in","Ppt.in","Utilization")) %>% gather(Variable, water, -c(GCM, Month)) 

ggplot(WBMonthlyLong) +
  geom_bar(aes(x=Month, y=water,fill=Variable),position="stack", stat="identity")
  




  


