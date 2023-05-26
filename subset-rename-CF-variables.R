library(dplyr)

CF.Dir <- "C:/Users/achildress/Documents/RCF_Testing/GRSA/WarmWet_HotDry/"

New.Dir <- "C:/Users/achildress/Documents/RCF_Testing/GRSA-infrastructure/"
if(dir.exists(New.Dir) == FALSE){
  dir.create(New.Dir)
}

rename.save.plot <- function(old.name, new.name,folder.type="figures"){
  old.plot=paste0(CF.Dir,folder.type,"/", old.name)
  file.copy(from=old.plot, to=New.Dir, 
            overwrite = TRUE, recursive = FALSE, 
            copy.mode = TRUE)
  file.rename(from=paste0(New.Dir,old.name), to=paste0(New.Dir,new.name))
}

rename.save.plot(old.name="OverHighQ-Annual.png",new.name="ExtremeHeat-95thPercentile.png")
rename.save.plot(old.name="HI.Dan-Annual-bar.png",new.name="ExtremeHeat-DangerousHeatIndex.png")
rename.save.plot(old.name="GRSA_WW-HD_Plot_data.xlsx",new.name="Variable-summary-plots.xlsx",folder.type="tables")
