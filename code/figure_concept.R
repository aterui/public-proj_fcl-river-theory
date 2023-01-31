
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))

source(here::here("code/figure_delta.R"))
source(here::here("code/figure_food_web.R"))
source(here::here("code/figure_network.R"))


# figure ------------------------------------------------------------------

layout <- "
AAAB
CCCD
"
g_concept <-  wrap_plots(g_net) + g_disturb + ggtitle("B") +
  wrap_plots(g_web) + g_delta + ggtitle("D") +
  plot_layout(design = layout)

ggsave(g_concept,
       filename = here::here("figure/figure_concept.pdf"),
       width = 12,
       height = 6)
