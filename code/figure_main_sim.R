# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))


# analysis ----------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_sim <- sim_main_result %>% 
  group_by(param_set) %>% 
  filter(theta == 0.1,
         sd_disturb_source == 5,
         sd_disturb_lon == 0.1)

# figure ------------------------------------------------------------------

g_np <- df_sim %>% 
  ggplot(aes(x = n_patch,
             y = fcl,
             color = factor(e_bp),
             fill = factor(e_bp))) +
  geom_smooth(method = "loess") +
  facet_grid(rows = vars(p_disturb),
             cols = vars(r_b)) +
  theme_bw()

ggsave(g_np,
       filename = here::here("output/figure_np_main.pdf"),
       height = 5,
       width = 6)

g_pb <- df_sim %>% 
  ggplot(aes(x = p_branch,
             y = fcl,
             color = factor(e_bp),
             fill = factor(e_bp))) +
  geom_smooth(method = "loess") +
  facet_grid(rows = vars(p_disturb),
             cols = vars(r_b)) +
  theme_bw()

ggsave(g_pb,
       filename = here::here("output/figure_pb_main.pdf"),
       height = 5,
       width = 6)
