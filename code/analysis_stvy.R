# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))


# format ------------------------------------------------------------------

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
           n_timestep,
           n_warmup,
           n_burnin) %>% 
  mutate(across(r_np:theta, scale)) %>% 
  pivot_longer(cols = c(r_np, r_pb),
               names_to = "response",
               values_to = "y")


# regression --------------------------------------------------------------

fit_sense <- df_r %>% 
  group_by(response) %>% 
  do(fit = lm(y ~ 
                p_disturb +
                sd_disturb_source +
                sd_disturb_lon +
                r_b +
                e_bc +
                e_bp +
                e_cp +
                a_bc +
                a_bp +
                a_cp +
                h_bc +
                h_bp +
                h_cp +
                s0 +
                p_dispersal +
                theta,
              data = .))

