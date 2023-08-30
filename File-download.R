# Script to download CF data from AWS for meta analysis

library(rvest)
getOption('timeout')
options(timeout=500)

# Get list of links from website
URL <- "https://cf-results.s3.us-west-2.amazonaws.com/index.html"
page <- read_html(URL)
links <- page %>% html_nodes("a") %>% html_attr('href') #extract html elements that are links
links[grepl(".zip", links)] -> links # extract to only zipped files


# Specify the file name and location where you want to save the file on your computer
file_names <- sub('.*\\/', '', links) #file names are park.zip
file_path <- "C:/Users/arunyon/3D Objects/Local-files/DL-test/"
# Call the download.file() function, passing in the URL and file name/location as arguments

download.file(links[1], paste(file_path, file_names[1], sep = ""), mode = "wb")

zip.list <- list.files(file_path,full.names = T)
unzip(zip.list,exdir=file_path)

