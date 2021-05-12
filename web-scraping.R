########### Web scraping ################
# Tutorial: https://www.analyticsvidhya.com/blog/2017/03/beginners-guide-on-web-scraping-in-r-using-rvest-with-hands-on-knowledge/

rm(list=ls())
# Load packages
library(rvest)
library(tidyverse)

url <- "https://www.ncdc.noaa.gov/cag/county/time-series/CO-069/tavg/all/12/1895-2021"
# https://www.ncdc.noaa.gov/cag/county/time-series/AL-001/tavg/ann/1/1895-2020
# https://www.ncdc.noaa.gov/cag/county/time-series/AL-001/tmax/ann/1/1895-2020
# https://www.ncdc.noaa.gov/cag/county/time-series/AL-001/tmin/ann/1/1895-2020
# https://www.ncdc.noaa.gov/cag/county/time-series/AL-001/pcp/ann/12/1895-2021

OutDir <- "location save data"

# Reading the HTML code from the website
webpage <- read_html(url)

## Testing
forecasts <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.58931000000007&lon=-105.09268999999995#.YDkzeGhKhPY") %>%
   html_nodes(".temp") %>% #figure out what these do!
   html_text()
 
forecasts

url<-"https://www.ncdc.noaa.gov/cag/county/time-series/CO-069/tavg/all/12/1895-2020"
noaa <- read_html(url)

CSS_selector<- '/html/body/div[1]/div[2]/div/div[2]/div/div/div[2]'
y<- url %>% 
  read_html() %>%
  html_nodes(xpath = CSS_selector) 



tables <- noaa %>% html_table(fill = TRUE)
first_table <- tables[[1]]
 
noaa <- read_html("https://www.ncdc.noaa.gov/cag/county/time-series/CO-069/tavg/all/12/1895-2020") %>%
  html_nodes("div class") %>%
  html_text()

head(noaa)

json<-"https://www.ncdc.noaa.gov/cag/county/time-series/CO-069-tavg-all-12-1895-2020.json?base_prd=true&begbaseyear=1901&endbaseyear=2000"
r<-read_html(json) %>% html_text()
head(r)
r[[1]]
