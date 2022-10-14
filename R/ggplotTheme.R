suppressWarnings(library(tidyverse))

# GGPLOT THEME #
#--------------#

theme_NIseries <- function(){
  ggplot2::theme_classic() %+replace%
    theme(text = element_text(colour = "#A0A0A3"), 
          title = element_text(colour = "#FFFFFF"), 
          axis.title.x = element_text(colour = "#A0A0A3", face = "bold"), 
          axis.title.y = element_text(colour = "#A0A0A3", face = "bold", angle = 90, vjust = 1.5), 
          panel.grid.major.y = element_line(colour = "#C5CDDE", size = 0.5),
          axis.line.x.bottom = element_line(color = "#9DB7CC"),
          axis.line.y.left = element_blank(),
          axis.ticks.y = element_blank())
}


