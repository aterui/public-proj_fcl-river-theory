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
  summarize(r_np = cor(fcl,
                       n_patch,
                       method = "spearman"),
            r_pb = cor(fcl,
                       p_branch,
                       method = "spearman"),
            n_fcl = n_distinct(fcl)) %>% 
  mutate(r_np = ifelse(n_fcl == 1, 0, r_np),
         r_pb = ifelse(n_fcl == 1, 0, r_pb)) %>% 
  left_join(df_param,
            by = "param_set") %>% 
  ungroup()


  