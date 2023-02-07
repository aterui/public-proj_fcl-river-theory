
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R",
            "code/format_sim_data.R"),
       source)

## filter s & mean_disturb_source for visualization
s_set <- 0.5
mu_disturb <- 0.4

## df for heatmap
df_heat <- df_param %>% 
  left_join(df_coef,
            by = "param_set") %>% 
  filter(mean_disturb_source == mu_disturb,
         (s == 0 & a_bp == 0) | s == s_set)

## df for gam plot
df_plot <- df_sim %>% 
  filter(mean_disturb_source == mu_disturb,
         (s == 0 & a_bp == 0) | s == s_set)


# heatmap -----------------------------------------------------------------

r_set <- c(8, 20)
df_point <- expand.grid(r_b = r_set,
                        p_disturb = unique(df_sim$p_disturb),
                        rho1 = 0,
                        rho2 = 0)

## Ecosystem size
g_size <- df_heat %>% 
  ggplot(aes(x = r_b,
             y = p_disturb,
             fill = rho1)) +
  geom_raster() +
  geom_point(data = df_point) +
  facet_grid(cols = vars(disp),
             rows = vars(omn)) +
  labs(y = expression("Disturbance prob. ("*p[m]*")"),
       x = expression("Productivity ("*r[b]*")"),
       fill = "Slope") +
  scale_x_continuous(breaks = sort(unique(df_sim$r_b))) +
  scale_fill_gradient2(mid = 0) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  ggtitle("Ecosystem size")

## Ecosystem complexity
g_branch <- df_heat %>% 
  ggplot(aes(x = r_b,
             y = p_disturb,
             fill = rho2)) +
  geom_raster() +
  geom_point(data = df_point) +
  facet_grid(cols = vars(disp),
             rows = vars(omn)) +
  labs(y = expression("Disturbance prob. ("*p[m]*")"),
       x = expression("Productivity ("*r[b]*")"),
       fill = "Slope") +
  scale_x_continuous(breaks = sort(unique(df_sim$r_b))) +
  scale_fill_gradient2(mid = 0) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  ggtitle("Ecosystem complexity")

g_m <- (g_size + g_branch) + plot_annotation(tag_levels = "A")

# gam plot ----------------------------------------------------------------

## ecosystem size effect
g_gam_np <- foreach(i = 1:length(r_set)) %do% {
  
  df_plot %>% 
    filter(r_b == r_set[i]) %>% 
    ggplot(aes(x = n_patch,
               y = fcl,
               color = factor(p_disturb),
               fill = factor(p_disturb))) +
    geom_smooth(method = "gam") +
    facet_grid(rows = vars(omn),
               cols = vars(disp),
               scales = "free") +
    labs(y = "Food chain length",
         x = "Ecosystem size (number of habitat patches)",
         color = "Disturbance prob.",
         fill = "Disturbance prob.") +
    scale_color_met_d("Hiroshige", direction = -1) +
    scale_fill_met_d("Hiroshige", direction = -1) +
    ggtitle(ifelse(r_set[i] == min(r_set),
                   expression("Low productivity ("*r[b]~"= 8)"),
                   expression("High productivity ("*r[b]~"= 20)")))
}

g_np <- (g_gam_np[[1]] + g_gam_np[[2]]) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")

## ecosystem complexity effect
g_gam_pb <- foreach(i = 1:length(r_set)) %do% {
  
  df_plot %>% 
    filter(r_b == r_set[i]) %>% 
    ggplot(aes(x = p_branch,
               y = fcl,
               color = factor(p_disturb),
               fill = factor(p_disturb))) +
    geom_smooth(method = "gam") +
    facet_grid(rows = vars(omn),
               cols = vars(disp),
               scales = "free") +
    labs(y = "Food chain length",
         x = "Ecosystem complexity (branching probability)",
         color = "Disturbance prob.",
         fill = "Disturbance prob.") +
    scale_x_continuous(breaks = c(0.1, 0.4, 0.7, 1)) +
    scale_color_met_d("Hiroshige", direction = -1) +
    scale_fill_met_d("Hiroshige", direction = -1) +
    ggtitle(ifelse(r_set[i] == min(r_set),
                   expression("Low productivity ("*r[b]~"= 8)"),
                   expression("High productivity ("*r[b]~"= 20)")))
}

g_pb <- (g_gam_pb[[1]] + g_gam_pb[[2]]) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect")


# export ------------------------------------------------------------------

## heatmap
ggsave(g_m,
       filename = here::here("figure/figure_si_heatmap.pdf"),
       height = 7,
       width = 14)

## ecosystem size with low productivity
ggsave(g_np,
       filename = here::here("figure/figure_si_n_patch.pdf"),
       height = 9,
       width = 15)

## ecosystem size with low productivity
ggsave(g_pb,
       filename = here::here("figure/figure_si_p_branch.pdf"),
       height = 9,
       width = 15)


