
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))


# analysis ----------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_sim <- sim_main_result %>% 
  group_by(param_set) %>% 
  mutate(omn = case_when(e_bp == 0 ~ "Chain",
                         e_bp == 2 ~ "Weak",
                         e_bp == 4 ~ "Strong"),
         omn = factor(omn, levels = c("Chain",
                                      "Weak",
                                      "Strong")),
         productivity = recode(r_b,
                               `4` = sprintf('"Low productivity"~(r[b]=="%.2f")',
                                             r_b),
                               `8` = sprintf('"High productivity"~(r[b]=="%.2f")',
                                             r_b)),
         productivity = factor(productivity,
                               levels = rev(levels(factor(productivity)))),
         disturb = recode(p_disturb,
                          `0.01` = sprintf('"Low disturbance"~(p[m]=="%.2f")',
                                           p_disturb),
                          `0.1` = sprintf('"High disturbance"~(p[m]=="%.2f")',
                                          p_disturb)),
         disturb = factor(disturb,
                          levels = rev(levels(factor(disturb))))) %>% 
  pivot_longer(cols = c(fcl, p_basal, p_igprey, p_igpred),
               names_to = "y",
               values_to = "value") %>% 
  mutate(y = case_when(y == "fcl" ~ "Food~chain~length",
                       y == "p_basal" ~ "Basal~occupancy~(B)",
                       y == "p_igprey" ~ "Consumer~occupancy~(C)",
                       y == "p_igpred" ~ "Predator~occupancy~(P)"),
         y = factor(y,
                    levels = c("Food~chain~length",
                               "Basal~occupancy~(B)",
                               "Consumer~occupancy~(C)",
                               "Predator~occupancy~(P)")))

df_param <- expand.grid(theta = c(0.1, 1),
                        sd_disturb_source = c(0.1, 3),
                        sd_disturb_lon = c(0.1, 3)) %>% 
  as_tibble() %>% 
  filter(sd_disturb_source > sd_disturb_lon)


# figure ------------------------------------------------------------------

theme_set(plt_theme)

## ecosystem size
list_g_np <- foreach(i = seq_len(nrow(df_param))) %do% {
  
  g_np <- df_sim %>% 
    filter(theta == df_param$theta[i],
           sd_disturb_lon == df_param$sd_disturb_lon[i],
           sd_disturb_source == df_param$sd_disturb_source[i]) %>% 
    ggplot(aes(x = n_patch,
               y = value,
               color = omn,
               fill = omn)) +
    geom_smooth(method = "loess",
                size = 0.5) +
    facet_grid(rows = vars(y),
               cols = vars(disturb, productivity),
               labeller = label_parsed,
               scales = "free_y",
               switch = "y") +
    labs(x = "Ecosystem size (number of patches)",
         y = "",
         color = "Omnivory",
         fill = "Omnivory") +
    MetBrewer::scale_color_met_d("Hiroshige") +
    MetBrewer::scale_fill_met_d("Hiroshige") +
    theme(strip.placement = "outside")
  
  return(g_np)
}

## ecosystem complexity
list_g_pb <- foreach(i = seq_len(nrow(df_param))) %do% {

  g_pb <- df_sim %>% 
    filter(theta == df_param$theta[i],
           sd_disturb_lon == df_param$sd_disturb_lon[i],
           sd_disturb_source == df_param$sd_disturb_source[i]) %>% 
    ggplot(aes(x = p_branch,
               y = value,
               color = omn,
               fill = omn)) +
    geom_smooth(method = "loess",
                size = 0.5) +
    facet_grid(rows = vars(y),
               cols = vars(disturb, productivity),
               labeller = label_parsed,
               scales = "free_y",
               switch = "y") +
    labs(x = "Ecosystem complexity (branching prob.)",
         y = "",
         color = "Omnivory",
         fill = "Omnivory") +
    MetBrewer::scale_color_met_d("Hiroshige") +
    MetBrewer::scale_fill_met_d("Hiroshige") +
    theme(strip.placement = "outside")
  
  return(g_pb)
}

list_g_fcl <- list(list_g_np, list_g_pb)
