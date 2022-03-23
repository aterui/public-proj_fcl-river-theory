
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/analysis_stvy.R"))


# table -------------------------------------------------------------------

table_stvy <- foreach(i = 1:2) %do% {
  fit_sense$fit[[i]] %>% 
    tidy() %>%
    mutate(Group = case_when(term %in% c("r_b",
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
                             term %in% c("theta",
                                         "p_dispersal") ~ "Dispersal",
                             term %in% c("p_disturb",
                                         "mean_disturb_source",
                                         "sd_disturb_source",
                                         "sd_disturb_lon") ~ "Disturbance"),
           Term = case_when(term == "r_b" ~ "$r_b$",
                            term == "e_bc" ~"$e_{bc}$",
                            term == "e_bp" ~"$e_{bp}$",
                            term == "e_cp" ~"$e_{cp}$",
                            term == "a_bc" ~"$a_{bc}$",
                            term == "a_bp" ~"$a_{bp}$",
                            term == "a_cp" ~"$a_{cp}$",
                            term == "h_bc" ~"$h_{bc}$",
                            term == "h_bp" ~"$h_{bp}$",
                            term == "h_cp" ~"$h_{cp}$",
                            term == "theta" ~"$\\theta$",
                            term == "p_disturb" ~"$p_m$",
                            term == "sd_disturb_source" ~ "$\\sigma_h$",
                            term == "sd_disturb_lon" ~ "$\\sigma_l$",
                            term == "s0" ~ "$s_0$",
                            term == "p_dispersal" ~ "$p_d$"),
           Interpretation = case_when(term == "r_b" ~ "Reproductive rate of basal species",
                                      term == "e_bc" ~ "Conversion efficiency (B to C)",
                                      term == "e_bp" ~ "Conversion efficiency (B to P)",
                                      term == "e_cp" ~ "Conversion efficiency (C to P)",
                                      term == "a_bc" ~ "Attack rate (C on B)",
                                      term == "a_bp" ~ "Attack rate (P on B)",
                                      term == "a_cp" ~ "Attack rate (P on C)",
                                      term == "h_bc" ~ "Handling time (C on B)",
                                      term == "h_bp" ~ "Handling time (P on B)",
                                      term == "h_cp" ~ "Handling time (P on C)",
                                      term == "theta" ~ "Inverse of mean dispersal distance",
                                      term == "p_disturb" ~ "Disturbance prob.",
                                      term == "mean_disturb_source" ~ "Disturbance intensity",
                                      term == "sd_disturb_source" ~ "Disturbance variation at headwaters",
                                      term == "sd_disturb_lon" ~ "Local disturbance variation",
                                      term == "s0" ~ "Survival prob.",
                                      term == "p_dispersal" ~ "Dispersal prob.",
                                      term == "(Intercept)" ~ "Intercept")) %>% 
    arrange(Group,
            Term) %>% 
    mutate(Group = ifelse(duplicated(Group), NA, Group)) %>% 
    dplyr::select(Group,
                  Term,
                  Interpretation,
                  Estimate =estimate,
                  SE = std.error) %>% 
    kable(format = "markdown",
          digits = 3)
}
