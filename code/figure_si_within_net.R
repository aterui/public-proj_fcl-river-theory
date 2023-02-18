
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R"),
       source)


# figure ------------------------------------------------------------------

df_wn <- readRDS("output/sim_within_net.rds")

df_wn %>% 
  filter(n_patch == 150,
         a_bp == 0.04) %>% 
  ggplot(aes(x = carrying_capacity,
             y = fcl,
             color = factor(p_branch),
             fill = factor(p_branch))) +
  geom_point(alpha = 0.2) +
  geom_smooth()# +
  #facet_wrap(facets = ~factor(n_patch))
