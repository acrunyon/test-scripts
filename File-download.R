# Script to download CF data from AWS for meta analysis

library(rvest)
library(plyr)
getOption('timeout')
options(timeout=500)

# Get list of links from website
URL <- "https://cf-results.s3.us-west-2.amazonaws.com/index.html"
page <- read_html(URL)
links <- page %>% html_nodes("a") %>% html_attr('href') #extract html elements that are links
links[grepl(".zip", links)] -> links # extract to only zipped files
links #inspect to see which indices are not park RCF files
links <- links[1:377] #removes last few that are not park RCF files


# Specify the file name and location where you want to save the file on your computer
file_names <- sub('.*\\/', '', links) #file names are park.zip
file_path <- "E:/RCF_2024/"

for (i in 1:length(links)){ #Run as loop to avoid timeout - which happens when calling too many units
# for (i in 351:length(links)-1){
download.file(links[i], paste(file_path, file_names[i], sep = ""), mode = "wb")
}


extraction_path <- "E:/RCF_2024/RCF_opened/"
zip.list <- list.files(path = file_path, pattern = "*.zip", full.names = TRUE)

ldply(.data = zip.list[1:377], .fun = unzip, exdir = extraction_path)


