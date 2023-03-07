
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R",
            "code/format_sim_data.R"),
       source)

df_coef <- readRDS("output/df_coef.rds")

## filter s & mean_disturb_source for visualization
s_set <- 1
mu_disturb <- 0.2

## df for loess plot
df_plot <- df_sim %>% 
  filter(mean_disturb_source == mu_disturb | p_disturb == 0,
         (s == 0 & a_bp == 0) | s == s_set)


# plot: geometry effect ---------------------------------------------------

lab <- c(`8` = "Low~productivity",
         `16` = "High~productivity")

## ecosystem size effect
g_np <-  df_plot %>% 
  ggplot(aes(x = n_patch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  #geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_grid(rows = vars(omn),
             cols = vars(disp, r_b),
             #scales = "free",
             labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
  labs(y = "Food chain length",
       x = "Ecosystem size (number of habitat patches)",
       color = "Disturbance prob.",
       fill = "Disturbance prob.") +
  scale_color_met_d("Hiroshige", direction = -1) +
  scale_fill_met_d("Hiroshige", direction = -1)

## ecosystem complexity effect
g_pb <-  df_plot %>% 
  ggplot(aes(x = p_branch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  #geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_grid(rows = vars(omn),
             cols = vars(disp, r_b),
             #scales = "free",
             labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
  labs(y = "Food chain length",
       x = "Ecosystem complexity (branching probability)",
       color = "Disturbance prob.",
       fill = "Disturbance prob.") +
  scale_x_continuous(breaks = c(0.1, 0.4, 0.7, 1)) +
  scale_color_met_d("Hiroshige", direction = -1) +
  scale_fill_met_d("Hiroshige", direction = -1)


# export ------------------------------------------------------------------

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


