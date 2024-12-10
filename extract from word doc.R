library(dplyr)

rm(list=ls())

WordDir <- "D:/CF-summary-word/"
FileList <- list.files(WordDir, pattern = ".docx",full.names=TRUE)

messages.list <- data.frame()

### Start loop here
for(i in 1:length(FileList)){
df <- data.frame(matrix(nrow=1,ncol=0))
df$SiteID <- sub("^([^/]*/){2}(....).*", "\\2", FileList[i])

doc.text <- readtext::readtext(FileList[i])$text

# Split text into parts using new line character:
doc.parts <- strsplit(doc.text, "\n")[[1]]

# Need to ID which says "Key messages" and select 4 after that
message.index <- grep("Key messages", doc.parts)[2]
df$KM1 <- doc.parts[message.index+1]
df$KM2 <- doc.parts[message.index+2]
df$KM3 <- doc.parts[message.index+3]
df$KM4 <- doc.parts[message.index+4]

messages.list <- rbind(messages.list,df)
}

write.csv(messages.list, "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/CFsummary_key_messages.csv",row.names = FALSE)
write.table(messages.list, "C:/Users/arunyon/3D Objects/Local-files/RCF_Testing/CFsummary_key_messages.txt",row.names = FALSE, sep = ",")
