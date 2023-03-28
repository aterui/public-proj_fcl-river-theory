
# setup -------------------------------------------------------------------

rm(list = ls())

lapply(list("code/library.R",
            "code/set_figure_theme.R"),
       source)

theme_set(plt_theme)


# data --------------------------------------------------------------------

df_sim <- readRDS(here::here("output/sim_main.rds")) %>% 
  filter(param_set == 1)

n_patch <- df_sim$n_patch
p_branch <- df_sim$p_branch

df_dist <- foreach(i = seq_len(length(p_branch)),
                   .combine = bind_rows) %do% {
                     net <- brnet(n_patch = n_patch[i],
                                  p_branch = p_branch[i])
                     
                     diag(net$distance_matrix) <- NA
                     df_y <- tibble(n_patch = n_patch[i],
                                    p_branch = p_branch[i],
                                    mu_distance = mean(net$distance_matrix, na.rm = T))
                     return(df_y)
                   }

df_plot <- left_join(df_sim, df_dist,
                     by = c("n_patch", "p_branch")) %>% 
  pivot_longer(cols = c(mc_capacity, mu_distance),
               values_to = "value",
               names_to = "y")


# plot --------------------------------------------------------------------

label <- c(`mc_capacity` = "Total carrying capacity",
           `mu_distance` = "Mean distance")

g_net <- df_plot %>% 
  ggplot(aes(x = n_patch,
             y = value,
             color = p_branch)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  facet_wrap(facets =~ y,
             ncol = 1,
             scales = "free",
             switch = "y",
             labeller = labeller(y = label)) +
  labs(x = "Ecosystem size (number of patches)",
       color = "Branching prob.") +
  theme(strip.placement = "outside",
        axis.title.y = element_blank())


# export ------------------------------------------------------------------

ggsave(g_net,
       filename = "figure/figure_si_k_distance.pdf",
       width = 8,
       height = 11)
