
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R",
            "code/format_sim_data.R"),
       source)

## filter s & mean_disturb_source for visualization
s_set <- 1
mu_disturb <- 0.8

## df for gam plot
df_plot <- df_sim %>% 
  filter(mean_disturb_source == mu_disturb,
         (s == 0 & a_bp == 0) | s == s_set) %>% 
  pivot_longer(cols = paste0("s", 0:4),
               values_to = "p_state",
               names_to = "state")


# plot: state transition --------------------------------------------------

theme_set(plt_theme)

lab <- c(`8` = "Low~productivity",
         `16` = "High~productivity")

state_lab <- c(`s0` = "No species",
               `s1` = "B",
               `s2` = "B + C",
               `s3` = "B + P",
               `s4` = "B + C + P")

## ecosystem size effect
list_g_np <- foreach(x = unique(df_plot$omn)) %do% {

  g_np <-  df_plot %>% 
    filter(omn == x) %>% 
    ggplot(aes(x = n_patch,
               y = p_state,
               color = factor(p_disturb),
               fill = factor(p_disturb))) +
    #geom_point(alpha = 0.1) +
    geom_smooth(method = "loess") +
    facet_grid(rows = vars(state),
               cols = vars(disp, r_b),
               #scales = "free",
               labeller = labeller(r_b = as_labeller(lab, label_parsed),
                                   state = state_lab)) +
    labs(y = "Occupancy",
         x = "Ecosystem size (number of habitat patches)",
         color = "Disturbance prob.",
         fill = "Disturbance prob.") +
    scale_color_met_d("Hiroshige", direction = -1) +
    scale_fill_met_d("Hiroshige", direction = -1) +
    ggtitle(x)
}

g_np_all <- list_g_np[[1]] + list_g_np[[2]] + 
  list_g_np[[3]] + guide_area() +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", design = "AC
                                            BD")


## ecosystem complexity effect
list_g_bp <- foreach(x = unique(df_plot$omn)) %do% {
  
  g_bp <-  df_plot %>% 
    filter(omn == x) %>% 
    ggplot(aes(x = p_branch,
               y = p_state,
               color = factor(p_disturb),
               fill = factor(p_disturb))) +
    #geom_point(alpha = 0.1) +
    geom_smooth(method = "loess") +
    facet_grid(rows = vars(state),
               cols = vars(disp, r_b),
               #scales = "free",
               labeller = labeller(r_b = as_labeller(lab, label_parsed),
                                   state = state_lab)) +
    labs(y = "Occupancy",
         x = "Ecosystem complexity (branching prob.)",
         color = "Disturbance prob.",
         fill = "Disturbance prob.") +
    scale_x_continuous(breaks = c(0.1, 0.4, 0.7, 1.0)) +
    scale_color_met_d("Hiroshige", direction = -1) +
    scale_fill_met_d("Hiroshige", direction = -1) +
    ggtitle(x)
}

g_bp_all <- list_g_bp[[1]] + list_g_bp[[2]] + 
  list_g_bp[[3]] + guide_area() +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", design = "AC
                                            BD")


# export ------------------------------------------------------------------

ggsave(g_np_all,
       filename = here::here(paste0("figure/si_figure_state_n_patch.pdf")),
       height = 14,
       width = 14)

ggsave(g_bp_all,
       filename = here::here(paste0("figure/si_figure_state_p_branch.pdf")),
       height = 14,
       width = 14)
