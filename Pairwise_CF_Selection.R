library(dplyr)
library(tibble)
library(reshape2)
library(ggplot2)
library(ggrepel)
library(ggfortify)

rm(list=ls())
# load("C:/Users/achildress/Documents/RCF_Testing/VAFO/input-data/Final_Environment.RData")
# load("C:/Users/achildress/Documents/RCF_Testing/VALL/input-data/Final_Environment.RData")
# load("C:/Users/achildress/OneDrive - DOI/Documents/RSS/Completed/FOUS/MACA2/FOUS_48.0002_-104.0415_Final_Environment-FINAL.RData")
# load("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/CARE/Climate-futures-FY22/CARE/input-data/Final_Environment.RData")
load("C:/Users/achildress/DOI/NPS-NRSS-CCRP-FC Science Adaptation - Documents/General/RSS Stuff/Parks/CONG/Figs Projections/CONG_33.791868_-80.748665_Final_Environment.RData")

head(Future_Means)

Percent.CF.dec <- sum(Future_Means$DeltaPr < 0) / nrow(Future_Means)
Percent.CF.inc <- sum(Future_Means$DeltaPr > 0) / nrow(Future_Means)

# Future_Means$Pr.weight <- abs(Future_Means$DeltaPr - PrAvg)
# Future_Means$Tavg.weight<- abs(Future_Means$DeltaTavg - Tavg)
Future_Means$Distance.avg <- sqrt((Future_Means$DeltaTavg - Tavg)^2 + (Future_Means$DeltaPr - PrAvg)^2)
# Future_Means %>% group_by(CF) %>% filter(Distance.avg == max(Distance.avg)) %>% mutate(D = CF) -> FM # Distance from means same as distance from corners

xy <- Future_Means %>% column_to_rownames( var = "GCM") %>% select(c(DeltaTavg,DeltaPr)) #tibble pkg

dist <- dist(xy, diag=T, upper=T)
xxyy <- melt(as.matrix(dist), varnames = c("GCM", "gcm2"))

xxyy <- merge(xxyy, Future_Means[,c("GCM","CF","Distance.avg", "DeltaPr", "DeltaTavg")],by="GCM")

xxyy %>% filter(CF == "Warm Damp", DeltaPr <0) %>% filter(Distance.avg == max(Distance.avg)) #%>% 
  filter(value == max(value))

### Test using PCA w/ tavg and pr
head(Future_Means)
FM <- Future_Means %>% select("GCM","DeltaPr","DeltaTavg") %>% 
  remove_rownames %>% column_to_rownames(var="GCM")
  
pca <- prcomp(FM, center = TRUE,scale. = TRUE)
  
  head(pca$rotation)
  head(pca$x)
  autoplot(pca, data = FM, loadings = TRUE,label=TRUE)
  
  pca.df<-as.data.frame(pca$x)

  rownames(pca.df)[which.min(pca.df$PC1)]
  rownames(pca.df)[which.max(pca.df$PC1)]
  rownames(pca.df)[which.min(pca.df$PC2)]
  rownames(pca.df)[which.max(pca.df$PC2)]
  


