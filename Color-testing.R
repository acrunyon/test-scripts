library(scales)
library(viridis)
library(ggplot2)
library(RColorBrewer)



x <- c("darkolivegreen4", "darkgoldenrod4", "turquoise3", "red3")
show_col(x)

x <-  c("#6EB2D4", "#05689F", "#F6B294", "#CA0020")
show_col(x)

col2hex <- function(x, alpha = FALSE) {
  args <- as.data.frame(t(col2rgb(x, alpha = alpha)))
  args <- c(args, list(names = x, maxColorValue = 255))
  do.call(rgb, args)
}
col2hex(x)

show_col(turbo(4, alpha = 1, begin = 0, end = 1, direction = 1))
show_col(viridis(4, alpha = 1, begin = 0, end = 1, direction = 1))



display.brewer.all()
display.brewer.pal(4, "Spectral")
display.brewer.pal(4, "RdYlBu")
display.brewer.pal(4, "RdBu")
display.brewer.pal(4, "PuOr")

show_col(brewer.pal(4, "Spectral"))
