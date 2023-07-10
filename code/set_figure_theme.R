library(tidyverse)

plt_theme <- theme_bw() + theme(
  plot.background = element_blank(),
  
  panel.background = element_rect(grey(0.98)),
  panel.border = element_rect(fill = NA),
  
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  
  legend.title = element_text(size = 18),
  legend.text = element_text(size = 18),
  
  axis.text = element_text(size = 18),
    
  strip.background = element_blank(),
  strip.text = element_text(size = 22),
  axis.title = element_text(size = 22),
  axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)),
  axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 20))
)
