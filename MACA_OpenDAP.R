################### OPeNDAP EXAMPLE SCRIPT: SIMPLE ########################################
##### FILENAME: OPeNDAPExample_TimeSeries_macav2livneh_SimpleExample.r   ##################
##### PURPOSE: THIS SCRIPT PULLS ONE POINT AND ALL TIME STEPS FROM A MACA DATA FILE #######
##### AUTHOR: HEATHER DINON ALDRIDGE (hadinon@ncsu.edu) ###################################
##### UPDATED: FEBRUARY 5, 2015                      ######################################
##### THIS SCRIPT IS RUN USING R version 3.0.1 (2013-05-16) ###############################
##### FOR MORE INFORMATION ON THE ncdf4 R PACKAGE, SEE: ###################################
##### http://cran.r-project.org/web/packages/ncdf4/ncdf4.pdf ##############################
##### ANOTHER WAY TO ACCESS DATA USING OPeNDAP CAN BE FOUND HERE: #########################
##### http://lukemiller.org/index.php/2011/02/accessing-noaa-tide-data-with-r/ ############
###########################################################################################

## LOAD THE REQUIRED LIBRARY
library(ncdf4)

Lat <- 41.83476
Lon = -103.707
cLon = 180 - Lon
SiteID <- "SCBL"

### DEFINE THE URL
vars = c("pr", "tasmax", "tasmin")
scens = c("rcp45", "rcp85")
set = c("historical_1950_2005","rcp45_2006_2099","rcp85_2006_2099")

longVar = c("precipitation", "daily_maximum_temperature", "daily_minimum_temperature")

GCMs = c('bcc-csm1-1','bcc-csm1-1-m','BNU-ESM','CanESM2','CCSM4','CNRM-CM5','CSIRO-Mk3-6-0',
         'GFDL-ESM2G','GFDL-ESM2M','HadGEM2-CC365','HadGEM2-ES365',
         'inmcm4','IPSL-CM5A-MR','IPSL-CM5A-LR','IPSL-CM5B-LR',
         'MIROC5','MIROC-ESM','MIROC-ESM-CHEM','MRI-CGCM3','NorESM1-M')

GCM_ensemble = paste0(GCMs,"_r1i1p1"); GCM_ensemble[5] <- "CCSM4_r6i1p1"

df = data.frame()
# loop to download/work with data"
for (G in 1:length(GCM_ensemble)){
  for (s in 1:length(set)){
    for (v in 1:length(vars)){
url = paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_",vars[v],"_",GCM_ensemble[G],"_",set[s],"_CONUS_daily.nc")

## OPEN THE FILE
nc <- nc_open(url)

## SHOW SOME METADATA 
nc

varName <- names(nc$var)
varUnits <- ncatt_get(nc, varName, "units")$value
All_lat <- data.frame(nc$dim$lat$vals)
All_lon <- data.frame(nc$dim$lon$vals)
Lat_index = as.numeric(which.min(abs(All_lat$nc.dim.lat.vals - Lat)))
Lon_index = as.numeric(which.min(abs(All_lon$nc.dim.lon.vals - cLon)))
endcount <- nc$var[[1]]$varsize[3] 


## READ THE DATA VARIABLE (e.g. precipitation IN THIS CASE): http://www.inside-r.org/packages/cran/ncdf4/docs/ncvar_get 
## AND http://stackoverflow.com/questions/19936432/faster-reading-of-time-series-from-netcdf
## ORDER OF start= AND count= IS BASED ON ORDER IN BRACKETS AFTER VARIABLE NAME (SHOWN WHEN DISPLAYING YOUR METADATA)
## FROM THE DOCUMENTATION... "If [start] not specified, reading starts at the beginning of the file (1,1,1,...)."
## AND "If [count] not specified and the variable does NOT have an unlimited dimension, the entire variable is read. 
## As a special case, the value "-1" indicates that all entries along that dimension should be read."
data <- ncvar_get(nc, varName, start=c(Lon_index,Lat_index,1),count=c(1,1,endcount))
## READ THE TIME VARIABLE
time <- ncvar_get(nc, "time", start=c(1),count=c(endcount))
## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
time=as.Date(time, origin="1900-01-01") ##note: assumes leap years! http://stat.ethz.ch/R-manual/R-patched/library/base/html/as.Date.html
# PUT EVERYTHING INTO A DATA FRAME
c <- data.frame(time,data)
names(c)[2] <- varName
c$GCM = GCMs[1]
c$rcp = sub("\\_.*", "", set[1])
df = 
## CLOSE THE FILE
nc_close(nc)
    }
  }
}

