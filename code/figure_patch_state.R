
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
               names_to = "state") %>% 
  mutate(state = case_when(state == "s0" ~ "No species",
                           state == "s1" ~ "B",
                           state == "s2" ~ "B + C",
                           state == "s3" ~ "B + P",
                           state == "s4" ~ "B + C + P"),
         state = fct_relevel(state, "No species", "B", "B + C", "B + P"))


# plot: state transition --------------------------------------------------

theme_set(plt_theme)

lab <- c(`8` = "Low~productivity",
         `16` = "High~productivity")

pm_lab <- c(`0` = "p[m]==0",
            `0.05` = "p[m]==0.05",
            `0.1` = "p[m]==0.10",
            `0.15` = "p[m]==0.15",
            `0.2` = "p[m]==0.20")

## ecosystem size effect
list_g_np <- foreach(x = unique(df_plot$omn)) %do% {

  g_np <-  df_plot %>% 
    filter(omn == x) %>% 
    ggplot(aes(x = n_patch,
               y = p_state,
               color = factor(state),
               fill = factor(state))) +
    geom_smooth(method = "loess",
                linewidth = 0.75) +
    facet_grid(rows = vars(p_disturb),
               cols = vars(disp, r_b),
               labeller = labeller(r_b = as_labeller(lab, label_parsed),
                                   p_disturb = as_labeller(pm_lab, label_parsed))) +
    labs(y = expression("Proportion of patch state"~bar(psi)),
         x = "Ecosystem size (number of habitat patches)",
         color = "Patch state",
         fill = "Patch state") +
    scale_color_met_d("Austria", direction = -1) +
    scale_fill_met_d("Austria", direction = -1) +
    theme(legend.title = element_text(size = 20),
          legend.text =  element_text(size = 18)) +
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
               color = factor(state),
               fill = factor(state))) +
    geom_smooth(method = "loess") +
    facet_grid(rows = vars(p_disturb),
               cols = vars(disp, r_b),
               labeller = labeller(r_b = as_labeller(lab, label_parsed),
                                   p_disturb = as_labeller(pm_lab, label_parsed))) +
    labs(y = expression("Proportion of patch state"~bar(psi)),
         x = "Ecosystem complexity (branching prob.)",
         color = "Patch state",
         fill = "Patch state") +
    scale_x_continuous(breaks = c(0.1, 0.4, 0.7, 1.0)) +
    scale_color_met_d("Austria", direction = -1) +
    scale_fill_met_d("Austria", direction = -1) +
    theme(legend.title = element_text(size = 20),
          legend.text =  element_text(size = 18)) +
    ggtitle(x)
}

g_bp_all <- list_g_bp[[1]] + list_g_bp[[2]] + 
  list_g_bp[[3]] + guide_area() +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect", design = "AC
                                            BD")


# export ------------------------------------------------------------------

ggsave(g_np_all,
       filename = here::here(paste0("figure/figure_state_n_patch.pdf")),
       height = 14,
       width = 14)

ggsave(g_bp_all,
       filename = here::here(paste0("figure/figure_state_p_branch.pdf")),
       height = 14,
       width = 14)
