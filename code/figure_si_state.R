
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R",
            "code/format_sim_data.R"),
       source)

## filter s & mean_disturb_source for visualization
s_set <- 1
mu_disturb <- 0.8
r_set <- c(8, 20)

## df for gam plot
df_plot <- df_sim %>% 
  filter(mean_disturb_source == mu_disturb,
         (s == 0 & a_bp == 0) | s == s_set,
         r_b %in% r_set) %>% 
  pivot_longer(cols = paste0("s", 0:4),
               values_to = "p_state",
               names_to = "state")


# heatmap -----------------------------------------------------------------

theme_set(plt_theme)

lab <- c(`8` = "Low~productivity~(r[b]==8)",
         `20` = "High~productivity~(r[b]==20)")

## ecosystem size effect
list_g_np <- foreach(x = c(0, 0.15)) %do% {

  g_np <-  df_plot %>% 
    filter(p_disturb == x) %>% 
    ggplot(aes(x = n_patch,
               y = p_state,
               color = factor(state),
               fill = factor(state))) +
    #geom_point(alpha = 0.1) +
    geom_smooth(method = "loess") +
    facet_grid(rows = vars(omn),
               cols = vars(disp, r_b),
               #scales = "free",
               labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
    labs(y = "Food chain length",
         x = "Ecosystem size (number of habitat patches)",
         color = "State",
         fill = "State") +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
  
}

## ecosystem complexity effect
list_g_bp <- foreach(x = c(0, 0.15)) %do% {
  
  g_bp <-  df_plot %>% 
    filter(p_disturb == x) %>% 
    ggplot(aes(x = p_branch,
               y = p_state,
               color = factor(state),
               fill = factor(state))) +
    #geom_point(alpha = 0.1) +
    geom_smooth(method = "loess") +
    facet_grid(rows = vars(omn),
               cols = vars(disp, r_b),
               #scales = "free",
               labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
    labs(y = "Food chain length",
         x = "Ecosystem complexity (branching prob.)",
         color = "State",
         fill = "State") +
    scale_color_viridis_d() +
    scale_fill_viridis_d()
  
}


# export ------------------------------------------------------------------

# ## heatmap
# ggsave(g_m,
#        filename = here::here("figure/figure_main_heatmap.pdf"),
#        height = 7,
#        width = 14)
# 
# ## ecosystem size with low productivity
# ggsave(g_np,
#        filename = here::here("figure/figure_main_n_patch.pdf"),
#        height = 9,
#        width = 15)
# 
# ## ecosystem size with low productivity
# ggsave(g_pb,
#        filename = here::here("figure/figure_main_p_branch.pdf"),
#        height = 9,
#        width = 15)
# 
# 
