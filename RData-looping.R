# create loop to open init_parsed.RData files and create summary tables for each

rm(list=ls())

data.dir<-"C:/Users/achildress/Documents/RCF_Testing/RData-files/"

files <- list.files(path=data.dir, pattern="_init_parsed.RData") 
parks <- substr(files, 1, 4)

start.time = Sys.time()

for (n in 1:length(parks)){
  park = parks[n]
  load(paste(data.dir,files[n],sep=""))
  Future_all$Date = strptime(Future_all$Date, "%Y-%m-%d")
  Future_all$yr = Future_all$Date$year + 1900
  Future_all = subset(Future_all, yr >= 2040 - (30/2) & yr <= (2040 + (30/2)))
  Future_Means = data.frame(aggregate(cbind(Future_all$PrecipCustom, Future_all$TmaxCustom, Future_all$TminCustom)
                                      ~ Future_all$GCM, Future_all, mean,na.rm=F))   # , Future_all$Wind
  names(Future_Means) = c("GCM", "PrecipCustom", "TmaxCustom", "TminCustom")    # , "Wind"
  Future_Means$TavgCustom = (Future_Means$TmaxCustom + Future_Means$TminCustom)/2
  Future_Means$PrecipCustom = Future_Means$PrecipCustom *365
  assign(paste0(park,"_Future_Means"), Future_Means)
  # Print progress
  if (n == length(parks)) cat(paste0(round(n / length(parks) * 100), '% completed',': Done'))
  else cat(paste0(round(n / length(parks) * 100), '% completed'),"\r")
  flush.console()
  Sys.sleep(.05)
}

end.time = Sys.time()
run.time = end.time-start.time
run.time

### Code differentiating between damp/dry



#41 secs for 3 parks, would take 91 mins for 400 parks



