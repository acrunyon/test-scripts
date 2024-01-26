# # Testing ClimateR as temporary subsistute for cft()
# # https://github.com/mikejohnson51/climateR
#  
# remotes::install_github("mikejohnson51/climateR")
# remotes::install_github("mikejohnson51/AOI")
library(climateR)
library(AOI)
library(dplyr)
library(ggplot2)

rm(list=ls())

OutDir <- "C:/Users/arunyon/Downloads/SEKI/"

# Needs aoi to run -- dig back through original cft code at how to create aoi from lat/lon
# AOI<-geocode(location = c("Fort Collins"), pt = TRUE) 
GrantGrove<- aoi_get(list(36.746667, -118.94847,.01,.01)) #Coordiantes of east NIOB location
Buckeye <- aoi_get(list(36.52129, -118.76607,.01,.01))
Potwisha <- aoi_get(list(36.51697,  -118.80109,.01,.01))

vars = c("tasmax", "tasmin", "pr")
# scens = c("rcp45", "rcp85")

## GrandGrove
future_all <- data.frame()
for (i in 1:length(vars)){
future1 = getMACA(GrantGrove, 
                 model = "CNRM-CM5", varname = vars[i], scenario  = "rcp45",
                 startDate = "2024-01-01", endDate = "2099-12-31")
future2 = getMACA(GrantGrove, 
                  model = "MIROC-ESM", varname = vars[i], scenario  = "rcp85",
                  startDate = "2024-01-01", endDate = "2099-12-31")
future<- left_join(future1, future2, by="date")

future_long = future |>  
  tidyr::pivot_longer(-date)
FL <- future_long |> 
  mutate(GCM = gsub("^[^_]*_([^_]+)_.*$", "\\1", future_long$name),
         RCP = sub('.*_', '', future_long$name)) |> 
  rename(!!vars[i]:=value) |> select(-c(name))
if(i==1) { future_all = FL } else {
  future_all = left_join(future_all, FL, by=c("date","GCM","RCP"))
  rm(future_long, FL,future1, future2, future)
  }
}
future_all |> mutate(tmax_F = tasmax * 9/5 - 459.67,
                     tmin_F = tasmin * 9/5 - 459.67,
                     pr_in = pr/25.4) |> 
  select(date,GCM,RCP,tmax_F,tmin_F,pr_in) -> FA

gridmet = getGridMET(AOI = GrantGrove,
                          varname = c("tmmx","tmmn","pr"),
                          startDate = "1979-01-01",
                          endDate  = "2023-11-30")
gridmet |> mutate(tmax_F = tmmx * 9/5 - 459.67,
                  tmin_F = tmmn * 9/5 - 459.67,
                  pr_in = pr/25.4,
                  GCM = "gridmet",
                  RCP = "historical") |> 
  select(date,GCM,RCP,tmax_F,tmin_F,pr_in) -> GM

site <- rbind(GM,FA)

write.csv(site,paste0(OutDir, "GrantGrove.csv"),row.names=F)

## Buckeye
future_all <- data.frame()
for (i in 1:length(vars)){
  future1 = getMACA(Buckeye, 
                    model = "CNRM-CM5", varname = vars[i], scenario  = "rcp45",
                    startDate = "2024-01-01", endDate = "2099-12-31")
  future2 = getMACA(Buckeye, 
                    model = "MIROC-ESM", varname = vars[i], scenario  = "rcp85",
                    startDate = "2024-01-01", endDate = "2099-12-31")
  future<- left_join(future1, future2, by="date")
  
  future_long = future |>  
    tidyr::pivot_longer(-date)
  FL <- future_long |> 
    mutate(GCM = gsub("^[^_]*_([^_]+)_.*$", "\\1", future_long$name),
           RCP = sub('.*_', '', future_long$name)) |> 
    rename(!!vars[i]:=value) |> select(-c(name))
  if(i==1) { future_all = FL } else {
    future_all = left_join(future_all, FL, by=c("date","GCM","RCP"))
    rm(future_long, FL,future1, future2, future)
  }
}
future_all |> mutate(tmax_F = tasmax * 9/5 - 459.67,
                     tmin_F = tasmin * 9/5 - 459.67,
                     pr_in = pr/25.4) |> 
  select(date,GCM,RCP,tmax_F,tmin_F,pr_in) -> FA

gridmet = getGridMET(AOI = Buckeye,
                     varname = c("tmmx","tmmn","pr"),
                     startDate = "1979-01-01",
                     endDate  = "2023-11-30")
gridmet |> mutate(tmax_F = tmmx * 9/5 - 459.67,
                  tmin_F = tmmn * 9/5 - 459.67,
                  pr_in = pr/25.4,
                  GCM = "gridmet",
                  RCP = "historical") |> 
  select(date,GCM,RCP,tmax_F,tmin_F,pr_in) -> GM

site <- rbind(GM,FA)

write.csv(site,paste0(OutDir, "Buckeye.csv"),row.names=F)

## Potwisha
future_all <- data.frame()
for (i in 1:length(vars)){
  future1 = getMACA(Potwisha, 
                    model = "CNRM-CM5", varname = vars[i], scenario  = "rcp45",
                    startDate = "2024-01-01", endDate = "2099-12-31")
  future2 = getMACA(Potwisha, 
                    model = "MIROC-ESM", varname = vars[i], scenario  = "rcp85",
                    startDate = "2024-01-01", endDate = "2099-12-31")
  future<- left_join(future1, future2, by="date")
  
  future_long = future |>  
    tidyr::pivot_longer(-date)
  FL <- future_long |> 
    mutate(GCM = gsub("^[^_]*_([^_]+)_.*$", "\\1", future_long$name),
           RCP = sub('.*_', '', future_long$name)) |> 
    rename(!!vars[i]:=value) |> select(-c(name))
  if(i==1) { future_all = FL } else {
    future_all = left_join(future_all, FL, by=c("date","GCM","RCP"))
    rm(future_long, FL,future1, future2, future)
  }
}
future_all |> mutate(tmax_F = tasmax * 9/5 - 459.67,
                     tmin_F = tasmin * 9/5 - 459.67,
                     pr_in = pr/25.4) |> 
  select(date,GCM,RCP,tmax_F,tmin_F,pr_in) -> FA

gridmet = getGridMET(AOI = Potwisha,
                     varname = c("tmmx","tmmn","pr"),
                     startDate = "1979-01-01",
                     endDate  = "2023-11-30")
gridmet |> mutate(tmax_F = tmmx * 9/5 - 459.67,
                  tmin_F = tmmn * 9/5 - 459.67,
                  pr_in = pr/25.4,
                  GCM = "gridmet",
                  RCP = "historical") |> 
  select(date,GCM,RCP,tmax_F,tmin_F,pr_in) -> GM

site <- rbind(GM,FA)

write.csv(site,paste0(OutDir, "Potwisha.csv"),row.names=F)
