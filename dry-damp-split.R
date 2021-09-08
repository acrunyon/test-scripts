# create loop to open init_parsed.RData files and create summary tables for each

rm(list=ls())

data.dir<-"C:/Users/achildress/Documents/RCF_Testing/RData-files/"

files <- list.files(path=data.dir, pattern="_Final_Environment.RData") 
parks <- substr(files, 1, 4)

for (n in 1:length(parks)){
  park = parks[n]
  load(paste(data.dir,files[n],sep=""))
  assign(paste0(park,"_Future_Means"), Future_Means)
  rm(list=setdiff(ls(), c(ls(pattern="\\_Future_Means"),"data.dir","files","parks","park","n")))
  # Print progress
  if (n == length(parks)) cat(paste0(round(n / length(parks) * 100), '% completed',': Done'))
  else cat(paste0(round(n / length(parks) * 100), '% completed'),"\r")
  flush.console()
  Sys.sleep(.05)
}

### Code differentiating between damp/dry
mean(APIS_Future_Means$DeltaPr)
mean(CACO_Future_Means$DeltaPr)
mean(MACA_Future_Means$DeltaPr)

mean(MACA_Future_Means$DeltaPr[which(MACA_Future_Means$CF == "Warm Dry")])
mean(CACO_Future_Means$DeltaPr[which(CACO_Future_Means$CF == "Hot Damp")])

