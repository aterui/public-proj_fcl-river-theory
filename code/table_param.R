
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))


# table -------------------------------------------------------------------

df_sim <- readRDS(file = here::here("output/sim_main.rds"))

df_value <- df_sim %>% 
  dplyr::select(r_b,
                starts_with("e_"),
                starts_with("a_"),
                h,
                s,
                theta,
                p_disturb,
                base_k,
                mean_disturb_source,
                starts_with("sd_"),
                p_dispersal) %>% 
  summarize(across(.cols = everything(),
                   .fns = function(x) list(sort(unique(x))))) %>% 
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
                               param == "h" ~"$h$",
                               param == "s" ~ "$s$",
                               param == "theta" ~"$\\theta$",
                               param == "p_disturb" ~"$p_m$",
                               param == "base_k" ~ "$K_{base}$",
                               param == "mean_disturb_source" ~ "$\\mu_m$",
                               param == "sd_disturb_source" ~ "$\\sigma_h$",
                               param == "sd_disturb_lon" ~ "$\\sigma_l$",
                               param == "p_dispersal" ~ "$p_d$"),
         Model = case_when(param %in% c("r_b",
                                        "base_k",
                                        "e_bc",
                                        "e_bp",
                                        "e_cp",
                                        "a_bc",
                                        "a_bp",
                                        "a_cp",
                                        "h",
                                        "s",
                                        "theta",
                                        "p_dispersal",
                                        "p_disturb") ~ "Food web",
                           param %in% c("mean_disturb_source",
                                        "sd_disturb_source",
                                        "sd_disturb_lon") ~ "Network"),
         'One-patch' = case_when(param == "r_b" ~ "4, 8, 12, 16",
                                 param == "e_bc" ~ "1",
                                 param == "e_bp" ~ "1",
                                 param == "e_cp" ~ "1",
                                 param == "a_bc" ~ "0.25, 0.50",
                                 param == "a_bp" ~ "0 -- 0.10",
                                 param == "a_cp" ~ "0 -- 0.05",
                                 param == "h" ~ "0, 0.25, 0.50, 0.75, 1",
                                 param == "s" ~ "0, 1",
                                 param == "theta" ~ NA,
                                 param == "p_disturb" ~ NA,
                                 param == "base_k" ~ "100, 1000",
                                 param == "mean_disturb_source" ~ NA,
                                 param == "sd_disturb_source" ~ NA,
                                 param == "sd_disturb_lon" ~ NA,
                                 param == "p_dispersal" ~ NA),
         Interpretation = case_when(param == "r_b" ~ "Reproductive rate of basal species",
                                    param == "base_k" ~ "Carrrying capacity of basal species",
                                    param == "e_bc" ~ "Conversion efficiency (B to C)",
                                    param == "e_bp" ~ "Conversion efficiency (B to P)",
                                    param == "e_cp" ~ "Conversion efficiency (C to P)",
                                    param == "a_bc" ~ "Attack rate (C on B)",
                                    param == "a_bp" ~ "Attack rate (P on B)",
                                    param == "a_cp" ~ "Attack rate (P on C)",
                                    param == "h" ~ "Handling time",
                                    param == "theta" ~ "Inverse of mean dispersal distance",
                                    param == "p_disturb" ~ "Disturbance probability",
                                    param == "mean_disturb_source" ~ "Disturbance intensity",
                                    param == "sd_disturb_source" ~ "Disturbance variation at headwaters",
                                    param == "sd_disturb_lon" ~ "Local disturbance variation",
                                    param == "p_dispersal" ~ "Dispersal probability",
                                    param == "s" ~ "Switching parameter")) %>% 
  arrange(Model, Parameter) %>% 
  mutate(Model = ifelse(duplicated(Model), NA, Model)) %>% 
  dplyr::select(Model, Parameter, Interpretation, Main, 'One-patch')
