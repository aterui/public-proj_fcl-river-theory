
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))


# analysis ----------------------------------------------------------------

df_sim <- readRDS(file = here::here("output/sim_main.rds")) %>% 
  mutate(omn = case_when(a_bp == 0 ~ "Chain",
                         a_bp == 0.01 ~ "Weak",
                         a_bp == 0.05 ~ "Strong"),
         omn = fct_relevel(omn, c("Chain", "Weak", "Strong")),
         disp = case_when(theta == 1 ~ "Short dispersal",
                          theta == 0.1 ~ "Long dispersal"))

df_param <- df_sim %>% 
  select(-c(fcl,
            p_basal,
            p_igprey,
            p_igpred,
            mc_capacity,
            p_branch,
            n_patch,
            n_rep)) %>% 
  distinct()

df_coef <- df_sim %>% 
  group_by(param_set) %>% 
  do(param_set = unique(.$param_set),
     #mod = lm(fcl ~ n_patch + p_branch, data = .)) %>% 
     mod = lm(log(fcl + 1) ~ log(n_patch) + log(p_branch), data = .)) %>% 
  summarize(param_set = param_set,
            rho1 = coef(mod)[2],
            rho2 = coef(mod)[3])

## filter s & mean_disturb_source for visualization
df0 <- df_param %>% 
  left_join(df_coef,
            by = "param_set") %>% 
  filter(mean_disturb_source == 0.8,
         s == 0.5)


# heatmap -----------------------------------------------------------------

theme_set(plt_theme)

## Ecosystem size
g_size <- df0 %>% 
  ggplot(aes(x = r_b,
             y = p_disturb,
             fill = rho1)) +
  geom_raster() +
  facet_grid(cols = vars(disp),
             rows = vars(omn)) +
  labs(y = expression("Disturbance prob. ("*p[d]*")"),
       x = expression("Productivity ("*r[b]*")"),
       fill = "Slope") +
  scale_x_continuous(breaks = sort(unique(df_sim$r_b))) +
  scale_fill_gradient2(mid = 0) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  ggtitle("Ecosystem size")

## Ecosystem complexity
g_branch <- df0 %>% 
  ggplot(aes(x = r_b,
             y = p_disturb,
             fill = rho2)) +
  geom_raster() +
  facet_grid(cols = vars(disp),
             rows = vars(omn)) +
  labs(y = expression("Disturbance prob. ("*p[d]*")"),
       x = expression("Productivity ("*r[b]*")"),
       fill = "Slope") +
  scale_x_continuous(breaks = sort(unique(df_sim$r_b))) +
  scale_fill_gradient2(mid = 0) +
  theme_classic() +
  theme(strip.background = element_blank()) +
  ggtitle("Ecosystem complexity")

g_m <- (g_size + g_branch) + plot_annotation(tag_levels = "A")

# gam plot ----------------------------------------------------------------

r_set <- c(8, 20)

## ecosystem size effect
g_gam_np <- foreach(i = 1:length(r_set)) %do% {
  df_sim %>% 
    filter(mean_disturb_source == 0.8,
           r_b == r_set[i],
           s == 0.5) %>% 
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
  df_sim %>% 
    filter(mean_disturb_source == 0.8,
           r_b == r_set[i],
           s == 0.5) %>% 
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
       filename = here::here("figure/figure_main_heatmap.pdf"),
       height = 7,
       width = 14)

## ecosystem size with low productivity
ggsave(g_np,
       filename = here::here("figure/figure_main_n_patch.pdf"),
       height = 9,
       width = 15)

## ecosystem size with low productivity
ggsave(g_pb,
       filename = here::here("figure/figure_main_p_branch.pdf"),
       height = 9,
       width = 15)


