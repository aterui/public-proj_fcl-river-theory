library(tidyverse)

plt_theme <- theme_bw() + theme(
  plot.background = element_blank(),
  
  panel.background = element_rect(grey(0.96)),
  panel.border = element_rect(fill = NA),
  
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  
  axis.text = element_text(size = 10),
    
  strip.background = element_blank(),
  strip.text = element_text(size = 12),
  axis.title = element_text(size = 14)
)