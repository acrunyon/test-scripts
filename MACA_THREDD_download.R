#######################################################################################
# Script to download daily gridmet data
# Aggregated data available here: http://thredds.northwestknowledge.net:8080/thredds/reacch_climate_MET_aggregated_catalog.html
#######################################################################################

# dir.create('./data/park-specific/parsed-data/Gridmet') # Create folder to store .nc files from GridMET

# data.dir <- "./data/park-specific/parsed-data/Gridmet/"

data.dir <- "C:/Users/achildress/Documents/RCF_Testing/MACA/"

Lat <- 41.83476
Lon = -103.707
SiteID <- "SCBL"
box = c(Lat+.5,Lat-.5,Lon+.5,Lon-.5)

## Download data
#Variable and scenario names corresponding to MACA data directory structure
vars = c("pr", "tasmax", "tasmin")
scens = c("rcp45", "rcp85")
longVar = c("precipitation", "daily_maximum_temperature", "daily_minimum_temperature")

#Variable names for output tables
VarNames = c("PrecipCustom", "TmaxCustom", "TminCustom")

# GCMs to be extracted
GCMs = c('bcc-csm1-1_r1i1p1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
         'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
         'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
         'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

#Date ranges to be extracted
Future_StartYear = 2006   #2006-2099
Future_EndYear = 2099   #2006-2099
Hist_StartYear = 1950     #1950-2005
Hist_EndYear = 2005      #1950-2005

Remove_files = "Y" # "N"       #Removes all climate data files saved in directory

#Set working directory where files will download
"http://thredds.northwestknowledge.net:8080/thredds/ncss/agg_macav2metdata_pr_BNU-ESM_r1i1p1_rcp45_2006_2099_CONUS_daily.nc?var=precipitation&north=41.84&west=-103.71&east=-103.70&south=41.83&disableProjSubset=on&horizStride=1&time_start=2006-01-01T00%3A00%3A00Z&time_end=2099-12-31T00%3A00%3A00Z&timeStride=1&addLatLon=true&accept=netcdf"
                                                                                                                                                              
x=paste0("http://thredds.northwestknowledge.net:8080/thredds/ncss/agg_macav2metdata_",vars[1],"_",GCMs[1],"_",scens[1],"_2006_2099_CONUS_daily.nc?var=",longVar[1],"&north=",box[1],"&west=",box[4],"&east=",box[3],"&south=",box[2],"&disableProjSubset=on&horizStride=1&time_start=2006-01-01T00%3A00%3A00Z&time_end=2099-12-31T00%3A00%3A00Z&timeStride=1&addLatLon=true&accept=netcdf")
download.file(url=x, destfile=paste0(data.dir,vars[1],".nc"),method="auto",quiet=FALSE,mode="wb",cacheOK=TRUE)

# Download data - takes ~30 sec / var
for (i in 1:length(var)){
  writeLines("")
  print(paste("Downloading", longVar[i], "data"))
  writeLines("")
  x<-paste("http://thredds.northwestknowledge.net:8080/thredds/ncss/agg_met_",var[i],"_1979_CurrentYear_CONUS.nc?var=",longVar[i],
           "&north=",box[1],"&west=",box[4],"&east=",box[3],"&south=",box[2],"&disableLLSubset=on&disableProjSubset=on&horizStride=1&time_start=",
           startDate,"T00%3A00%3A00Z&time_end=",endDate,"T00%3A00%3A00Z&timeStride=1&accept=netcdf",sep="")
  download.file(url=x, destfile=paste0(data.dir,var[i],".nc"),method="auto",quiet=FALSE,mode="wb",cacheOK=TRUE)
}

# parsing data
files <- list.files(data.dir)

for(i in 1:length(files)){
  nc<-nc_open(paste0(data.dir,var[i],".nc")) 
  varName <- names(nc$var)
  varUnits <- ncatt_get(nc, varName, "units")$value
  All_lat <- data.frame(nc$dim$lat$vals)
  All_lon <- data.frame(nc$dim$lon$vals)
  Lat_index = as.numeric(which.min(abs(All_lat$nc.dim.lat.vals - Lat)))
  Lon_index = as.numeric(which.min(abs(All_lon$nc.dim.lon.vals - Lon)))
  
  Date <- as.Date(nc$dim$day$vals, origin = "1900-01-01")
  Extr <- ncvar_get(nc, varName, c(Lon_index, Lat_index, 1), count=c(1,1,-1))
  if(varUnits == "mm"){
    Extr <- Extr/25.4
  } else {(varUnits == "K")
    Extr <- (Extr * (9/5)) - 459.67
  }
  Data <- data.frame(Date, Extr)
  nc_close(nc)
  names(Data)[2] <- var[i] 
  Data1<-Data[order(Date),]
  if (var[i] == var[1]){GridMet<-Data} else {GridMet<-cbind(GridMet,Data1[2])} 
}

#set names so match output from other scripts -- need to change if alter variables
names(GridMet)<-c("Date","precip","tmax","tmin")

write.csv(GridMet,"./data/park-specific/parsed-data/GridMet.csv",row.names=F)

# Remove saved climate files
if(Remove_files == "Y") {
  do.call(file.remove, list(list.files(data.dir, pattern = '.nc', full.names = TRUE)))
  print("Files removed")
} else {print("Files remain")}