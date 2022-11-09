
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))


# table -------------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_value <- sim_main_result %>% 
  dplyr::select(-n_rep,
                -mc_capacity,
                -n_patch,
                -p_branch,
                -param_set,
                -base_k,
                -z,
                -n_timestep,
                -n_warmup,
                -n_burnin,
                -fcl,
                -p_basal,
                -p_igprey,
                -p_igpred) %>% 
  filter(sd_disturb_source > sd_disturb_lon) %>% 
  summarize(across(.fns = function(x) list(sort(unique(x))))) %>% 
  pivot_longer(cols = everything(),
               names_to = "param",
               values_to = "Main") %>%
  mutate(Parameter = case_when(param == "r_b" ~ "$r_b$",
                               param == "e_bc" ~"$e_{BC}$",
                               param == "e_bp" ~"$e_{BP}$",
                               param == "e_cp" ~"$e_{CP}$",
                               param == "a_bc" ~"$a_{BC}$",
                               param == "a_bp" ~"$a_{BP}$",
                               param == "a_cp" ~"$a_{CP}$",
                               param == "h_bc" ~"$h_{BC}$",
                               param == "h_bp" ~"$h_{BP}$",
                               param == "h_cp" ~"$h_{CP}$",
                               param == "theta" ~"$\\theta$",
                               param == "p_disturb" ~"$p_m$",
                               param == "mean_disturb_source" ~ "$\\mu_m$",
                               param == "sd_disturb_source" ~ "$\\sigma_h$",
                               param == "sd_disturb_lon" ~ "$\\sigma_l$",
                               param == "s0" ~ "$s_0$",
                               param == "p_dispersal" ~ "$p_d$"),
         Model = case_when(param %in% c("r_b",
                                        "s0",
                                        "e_bc",
                                        "e_bp",
                                        "e_cp",
                                        "a_bc",
                                        "a_bp",
                                        "a_cp",
                                        "h_bc",
                                        "h_bp",
                                        "h_cp",
                                        "theta",
                                        "p_dispersal",
                                        "p_disturb") ~ "Food web",
                           param %in% c("mean_disturb_source",
                                       "sd_disturb_source",
                                       "sd_disturb_lon") ~ "Network"),
         Sensitivity = case_when(param == "r_b" ~ "Unif(1, 10)",
                                 param == "e_bc" ~ "Unif(1, 10)",
                                 param == "e_bp" ~ "Unif(1, 10)",
                                 param == "e_cp" ~ "Unif(1, 10)",
                                 param == "a_bc" ~ "Unif(0.05, 0.5)",
                                 param == "a_bp" ~ "Unif(0.05, 0.5)",
                                 param == "a_cp" ~ "Unif(0.05, 0.5)",
                                 param == "h_bc" ~ "Unif(0.5, 5)",
                                 param == "h_bp" ~ "Unif(0.5, 5)",
                                 param == "h_cp" ~ "Unif(0.5, 5)",
                                 param == "theta" ~ "Unif(0.01, 1)",
                                 param == "p_disturb" ~ "Unif(0, 0.2)",
                                 param == "mean_disturb_source" ~ "0.8",
                                 param == "sd_disturb_source" ~ "Unif(0.05, 5)",
                                 param == "sd_disturb_lon" ~ "Unif(0.05, 5)",
                                 param == "s0" ~ "Unif(0.5, 1)",
                                 param == "p_dispersal" ~ "Unif(0, 0.1)"),
         Interpretation = case_when(param == "r_b" ~ "Reproductive rate of basal species",
                                    param == "e_bc" ~ "Conversion efficiency (B to C)",
                                    param == "e_bp" ~ "Conversion efficiency (B to P)",
                                    param == "e_cp" ~ "Conversion efficiency (C to P)",
                                    param == "a_bc" ~ "Attack rate (C on B)",
                                    param == "a_bp" ~ "Attack rate (P on B)",
                                    param == "a_cp" ~ "Attack rate (P on C)",
                                    param == "h_bc" ~ "Handling time (C on B)",
                                    param == "h_bp" ~ "Handling time (P on B)",
                                    param == "h_cp" ~ "Handling time (P on C)",
                                    param == "theta" ~ "Inverse of mean dispersal distance",
                                    param == "p_disturb" ~ "Disturbance prob.",
                                    param == "mean_disturb_source" ~ "Disturbance intensity",
                                    param == "sd_disturb_source" ~ "Disturbance variation at headwaters",
                                    param == "sd_disturb_lon" ~ "Local disturbance variation",
                                    param == "s0" ~ "Survival prob.",
                                    param == "p_dispersal" ~ "Dispersal prob.")) %>% 
  arrange(Model, Parameter) %>% 
  mutate(Model = ifelse(duplicated(Model), NA, Model)) %>% 
  dplyr::select(Model, Parameter, Interpretation, Main, Sensitivity)
