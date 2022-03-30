# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))

# analysis ----------------------------------------------------------------

load(file = here::here("output/result_stvy.RData"))

## parameter set
df_param <- sim_stvy_result %>% 
  distinct(across(mean_disturb_source:param_set))

## warning appears when no variation in fcl (n_fcl = 1)
## treat them as zero correlation
df_r <- sim_stvy_result %>% 
  group_by(param_set) %>% 
  summarize(r_np = suppressWarnings(cor(fcl, n_patch,
                                        method = "spearman")),
            r_pb = suppressWarnings(cor(fcl, p_branch,
                                        method = "spearman")),
            n_fcl = n_distinct(fcl)) %>% 
  mutate(r_np = ifelse(n_fcl == 1, 0, r_np),
         r_pb = ifelse(n_fcl == 1, 0, r_pb)) %>% 
  left_join(df_param,
            by = "param_set") %>% 
  ungroup() %>% 
  relocate(param_set,
           r_np,
           r_pb,
           n_timestep,
           n_warmup,
           n_burnin,
           base_k,
           z,
           mean_disturb_source) %>% 
  mutate(sd_ratio = log(sd_disturb_source / sd_disturb_lon)) %>% 
  pivot_longer(cols = sd_disturb_source:sd_ratio,
               names_to = "param",
               values_to = "value")


# figure ------------------------------------------------------------------

g_np <- df_r %>% 
  ggplot(aes(x = value,
             y = r_np)) +
  #geom_point(alpha = 0.075,
  #           col = "blue") +
  geom_smooth(method = "lm") +
  facet_wrap(facets = ~param,
             scales = "free",
             ncol = 3) +
  labs(y = "Spearman corr. between FCL and N_p") +
  theme_bw()

ggsave(g_np,
       filename = here::here("output/figure_np_stvy.pdf"),
       height = 9,
       width = 6)

g_pb <- df_r %>% 
  ggplot(aes(x = value,
             y = r_pb)) +
  #geom_point(alpha = 0.075,
  #           col = "blue") +
  geom_smooth(method = "lm") +
  facet_wrap(facets = ~param,
             scales = "free_x",
             ncol = 3) +
  labs(y = "Spearman corr. between FCL and P_b") +
  theme_bw()

ggsave(g_pb,
       filename = here::here("output/figure_pb_stvy.pdf"),
       height = 9,
       width = 6)
