
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

## df for heatmap
df_heat <- df_param %>% 
  left_join(df_coef,
            by = "param_set") %>% 
  filter(mean_disturb_source == mu_disturb | p_disturb == 0,
         (s == 0 & a_bp == 0) | s == s_set)

## df for loess plot
df_plot <- df_sim %>% 
  filter(mean_disturb_source == mu_disturb | p_disturb == 0,
         (s == 0 & a_bp == 0) | s == s_set) %>% 
  mutate(disp = ifelse(disp == "Short dispersal", "Short", "Long"))


# plot: fcl boxplot -------------------------------------------------------

theme_set(plt_theme)

g_r <- df_plot %>% 
  ggplot(aes(x = factor(p_disturb),
             y = fcl,
             color = factor(prod),
             fill = factor(prod))) +
  geom_boxplot(alpha = 0.2,
               linewidth = 0.1,
               outlier.size = 0.5) +
  facet_wrap(facets =~ omn,
             ncol = 1) +
  MetBrewer::scale_color_met_d("VanGogh3") +
  MetBrewer::scale_fill_met_d("VanGogh3") +
  labs(x = "Disturbance probability",
       y = "Food chain length",
       color = "Productivity",
       fill = "Productivity")

g_disp <- df_plot %>% 
  ggplot(aes(x = factor(p_disturb),
             y = fcl,
             color = factor(disp),
             fill = factor(disp))) +
  geom_boxplot(alpha = 0.2,
               linewidth = 0.1,
               outlier.size = 0.5) +
  facet_wrap(facets =~ omn,
             ncol = 1) +
  labs(x = "Disturbance probability",
       y = "",
       color = "Dispersal",
       fill = "Dispersal")

g_rd <- g_r + g_disp + plot_layout(guides = "collect")


# plot: state -------------------------------------------------------------

label <- c(`0` = "p[m]==0",
           `0.05` = "p[m]==0.05",
           `0.1` = "p[m]==0.1",
           `0.15` = "p[m]==0.15")

df_plot_st <- df_plot %>% 
  pivot_longer(cols = paste0("s", 0:4),
               names_to = "state",
               values_to = "value") %>% 
  mutate(state = case_when(state == "s0" ~ "No species",
                           state == "s1" ~ "B",
                           state == "s2" ~ "B + C",
                           state == "s3" ~ "B + P",
                           state == "s4" ~ "B + C + P"),
         state = fct_relevel(state, c("No species",
                                      "B",
                                      "B + C",
                                      "B + P", 
                                      "B + C + P")))

g_r_state <- df_plot_st %>% 
  ggplot(aes(x = factor(state),
             y = value,
             color = factor(prod),
             fill = factor(prod))) +
  geom_boxplot(alpha = 0.2,
               linewidth = 0.1,
               outlier.size = 0.5) +
  MetBrewer::scale_color_met_d("VanGogh3") +
  MetBrewer::scale_fill_met_d("VanGogh3") +
  facet_grid(rows = vars(omn),
             cols = vars(p_disturb), 
             labeller = labeller(p_disturb = as_labeller(label,
                                                         label_parsed))) +
  labs(x = "State",
       y = "Occupancy",
       color = "Productivity",
       fill = "Productivity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g_disp_state <- df_plot_st %>% 
  ggplot(aes(x = factor(state),
             y = value,
             color = factor(disp),
             fill = factor(disp))) +
  geom_boxplot(alpha = 0.2,
               linewidth = 0.1,
               outlier.size = 0.5) +
  facet_grid(rows = vars(omn),
             cols = vars(p_disturb), 
             labeller = labeller(p_disturb = as_labeller(label,
                                                         label_parsed))) +
  labs(x = "State",
       y = "Occupancy",
       color = "Dispersal",
       fill = "Dispersal") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# export ------------------------------------------------------------------

ggsave(g_rd,
       filename = here::here("figure/figure_si_rb_disp.pdf"),
       height = 8,
       width = 7)

ggsave(g_r_state,
       filename = here::here("figure/figure_si_r_state.pdf"),
       height = 6,
       width = 10)

ggsave(g_disp_state,
       filename = here::here("figure/figure_si_disp_state.pdf"),
       height = 6,
       width = 10)
