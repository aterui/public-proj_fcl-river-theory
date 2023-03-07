
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
