
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))


# format ------------------------------------------------------------------

df_sim <- readRDS(file = here::here("output/sim_main.rds")) %>% 
  filter(!(a_bp == 0 & s != 0)) %>% 
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
     mod = lm(log(fcl + 1) ~ log(n_patch) + log(p_branch), data = .)) %>% 
  summarize(param_set = param_set,
            rho1 = coef(mod)[2],
            rho2 = coef(mod)[3])
