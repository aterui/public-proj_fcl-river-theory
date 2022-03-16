
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))

source(here::here("code/figure_delta.R"))
source(here::here("code/figure_food_web.R"))
source(here::here("code/figure_network.R"))


# figure ------------------------------------------------------------------

layout <- "
ABCD
EEEG
"

g_web +
  g_delta + 
  g_net + 
  g_disturb + 
  plot_layout(design = layout)
