
# setup -------------------------------------------------------------------

rm(list = ls())
pacman::p_load(tidyverse)


# table -------------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_param <- sim_main_result %>% 
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
  summarize(across(.fns = function(x) list(unique(x)))) %>% 
  pivot_longer(cols = everything(),
               names_to = "param",
               values_to = "Main") %>%
  mutate(Parameter = case_when(param == "r_b" ~ "$r_b$",
                               param == "e_bc" ~"$e_{bc}$",
                               param == "e_bp" ~"$e_{bp}$",
                               param == "e_cp" ~"$e_{cp}$",
                               param == "a_bc" ~"$a_{bc}$",
                               param == "a_bp" ~"$a_{bp}$",
                               param == "a_cp" ~"$a_{cp}$",
                               param == "h_bc" ~"$h_{bc}$",
                               param == "h_bp" ~"$h_{bp}$",
                               param == "h_cp" ~"$h_{cp}$",
                               param == "theta" ~"$\\theta$",
                               param == "p_disturb" ~"$p_m$",
                               param == "mean_disturb_source" ~ "$m$",
                               param == "sd_disturb_source" ~ "$\\sigma_h$",
                               param == "sd_disturb_lon" ~ "$\\sigma_l$",
                               param == "s0" ~ "$s_0$",
                               param == "p_dispersal" ~ "$p_d$"),
         Group = case_when(param %in% c("r_b",
                                        "s0",
                                        "e_bc",
                                        "e_bp",
                                        "e_cp",
                                        "a_bc",
                                        "a_bp",
                                        "a_cp",
                                        "h_bc",
                                        "h_bp",
                                        "h_cp") ~ "Food web",
                           param %in% c("theta",
                                        "p_dispersal") ~ "Dispersal",
                           param %in% c("p_disturb",
                                        "mean_disturb_source",
                                        "sd_disturb_source",
                                        "sd_disturb_lon") ~ "Disturbance"),
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
                                 param == "p_dispersal" ~ "Unif(0, 0.1)")) %>% 
  arrange(Group, Parameter) %>% 
  mutate(Group = ifelse(duplicated(Group), NA, Group)) %>% 
  dplyr::select(Group, Parameter, Main, Sensitivity)
