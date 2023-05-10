## Testing time to threshold
library(tidyr)
library(dplyr)
library(ggplot2)

rm(list=ls())

#How ID what threshold is?

load("C:/Users/achildress/Documents/RCF_Testing/VALL/input-data/Final_Environment.RData")

TempThreshold <- quantile(Baseline_all$TmaxF,0.999) + 10 #10 degF greater than historical 99th

#Subset Future_all to WB_GCMs

#Calculate annual max tmax values

#Plot ts of annual max Tmax values with vertical lines at TempThreshold, colored as CFs and text showing # of yrs until crossed

