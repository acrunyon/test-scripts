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
###urltotal<-"http://convection.meas.ncsu.edu:8080/thredds/dodsC/pinemap/maca/past/macav2livneh_pr_bcc-csm1-1-m_historical_1970_1989_CONUS.nc"
urltotal<-"http://thredds.northwestknowledge.net:8080/thredds/dodsC/macav2livneh_huss_BNU-ESM_r1i1p1_historical_1950_2005_CONUS_daily_aggregated.nc"

## OPEN THE FILE
nc <- nc_open(urltotal)

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

## CLOSE THE FILE
nc_close(nc)

## PLOT THE DATA 
plot(c$time,c$data,main=paste("Daily ",var," for ",c$time[1]," through ",c$time[nrow(c)], " at ",lat,",",lon,sep=""),xlab="Date",ylab="Precipitation (mm)")


#######################################3


