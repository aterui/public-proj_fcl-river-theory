
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))


# format ------------------------------------------------------------------

df_sim <- readRDS(file = here::here("output/sim_main.rds")) %>% 
  #filter(!(a_bp == 0 & s != 0)) %>% 
  mutate(omn = case_when(a_bp == 0 ~ "Chain",
                         a_bp == 0.02 ~ "Weak omnivory",
                         a_bp == 0.04 ~ "Strong omnivory"),
         omn = fct_relevel(omn, c("Chain", "Weak omnivory", "Strong omnivory")),
         disp = case_when(theta == 1 ~ "Short dispersal",
                          theta == 0.1 ~ "Long dispersal"),
         prod = case_when(r_b == 8 ~ "Low",
                          r_b == 16 ~ "High"),
         disp = fct_relevel(disp, "Short dispersal"),
         prod = fct_relevel(prod, "Low"))

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

# df_coef <- df_sim %>% 
#   group_by(param_set) %>% 
#   do(param_set = unique(.$param_set),
#      rho1 = ppcor::pcor(tibble(.$fcl, .$n_patch, .$p_branch),
#                         method = "spearman")$estimate[1, 2],
#      rho2 = ppcor::pcor(tibble(.$fcl, .$n_patch, .$p_branch),
#                         method = "spearman")$estimate[1, 3]) %>% 
#   summarize(param_set = param_set,
#             rho1 = unlist(rho1),
#             rho2 = unlist(rho2))
# 
# 
# # export ------------------------------------------------------------------
# 
# saveRDS(df_coef,
#         "output/df_coef.rds")
