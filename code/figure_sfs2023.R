
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R",
            "code/format_sim_data.R"),
       source)

df_coef <- readRDS("output/df_coef.rds")

## filter s & mean_disturb_source for visualization
s_set <- 1
mu_disturb <- 0.8

## df for loess plot
df_plot <- df_sim %>% 
  filter(mean_disturb_source == mu_disturb,
         (s == 0 & a_bp == 0) | s == s_set)


# plot: geometry effect ---------------------------------------------------

lab <- c(`8` = "Low~productivity",
         `16` = "High~productivity")

## ecosystem size effect
g_np <-  df_plot %>% 
  filter(theta == 1,
         r_b == 8,
         p_disturb == 0.15) %>% 
  ggplot(aes(x = n_patch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  #geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_wrap(facet = ~omn,
             ncol = 3,
             labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
  labs(y = "Food chain length",
       x = "Number of habitat patches",
       color = "Disturbance prob.",
       fill = "Disturbance prob.") +
  scale_color_met_d("Hiroshige", direction = -1) +
  scale_fill_met_d("Hiroshige", direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  guides(color = "none",
         fill = "none")

## ecosystem complexity effect
g_pb <-  df_plot %>% 
  filter(theta == 1,
         r_b == 8,
         p_disturb == 0.15) %>% 
  ggplot(aes(x = p_branch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  #geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_wrap(facet = ~omn,
             ncol = 3,
             labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
  labs(y = "Food chain length",
       x = "Branching probability",
       color = "Disturbance prob.",
       fill = "Disturbance prob.") +
  scale_x_continuous(breaks = c(0.1, 0.4, 0.7, 1)) +
  scale_color_met_d("Hiroshige", direction = -1) +
  scale_fill_met_d("Hiroshige", direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  guides(color = "none",
         fill = "none")

g1 <- g_np / g_pb

# plot: geometry effect ---------------------------------------------------

lab <- c(`8` = "Low~productivity",
         `16` = "High~productivity")

## ecosystem size effect
g_np <-  df_plot %>% 
  filter(theta == 0.1,
         r_b == 16,
         p_disturb == 0) %>% 
  ggplot(aes(x = n_patch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  #geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_wrap(facet = ~omn,
             ncol = 3,
             labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
  labs(y = "Food chain length",
       x = "Number of habitat patches",
       color = "Disturbance prob.",
       fill = "Disturbance prob.") +
  scale_color_met_d("Hiroshige", direction = -1) +
  scale_fill_met_d("Hiroshige", direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  guides(color = "none",
         fill = "none")

## ecosystem complexity effect
g_pb <-  df_plot %>% 
  filter(theta == 0.1,
         r_b == 16,
         p_disturb == 0) %>% 
  ggplot(aes(x = p_branch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  #geom_point(alpha = 0.1) +
  geom_smooth(method = "loess") +
  facet_wrap(facet = ~omn,
             ncol = 3,
             labeller = labeller(r_b = as_labeller(lab, label_parsed))) +
  labs(y = "Food chain length",
       x = "Branching probability",
       color = "Disturbance prob.",
       fill = "Disturbance prob.") +
  scale_x_continuous(breaks = c(0.1, 0.4, 0.7, 1)) +
  scale_color_met_d("Hiroshige", direction = -1) +
  scale_fill_met_d("Hiroshige", direction = -1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        strip.background = element_blank()) +
  guides(color = "none",
         fill = "none")

g2 <- g_np / g_pb


# export ------------------------------------------------------------------

ggsave(g1, height = 6, width = 8,
       filename = "figure/figure_high_disturb.pdf")

ggsave(g2, height = 6, width = 8,
       filename = "figure/figure_no_disturb.pdf")

