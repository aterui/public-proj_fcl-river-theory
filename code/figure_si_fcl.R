
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# analysis ----------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_sim <- sim_main_result %>% 
  group_by(param_set) %>% 
  filter(!(theta == 1 &
           sd_disturb_source == 3 &
           sd_disturb_lon == 0.1)) %>% 
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
                          levels = rev(levels(factor(disturb))))
  )

df_param <- expand.grid(theta = 0.1,
                        sd_disturb_source = c(0.1, 3),
                        sd_disturb_lon = c(0.1, 3)) %>% 
  as_tibble() %>% 
  filter(sd_disturb_source > sd_disturb_lon)


# figure ------------------------------------------------------------------

theme_set(plt_theme)

list_g_np <- foreach(i = seq_len(nrow(df_param))) %do% {
  
  g_np <- df_sim %>% 
    filter(theta == df_param$theta[i],
           sd_disturb_lon == df_param$sd_disturb_lon[i],
           sd_disturb_source == df_param$sd_disturb_source[i]) %>% 
    ggplot(aes(x = n_patch,
               y = fcl,
               linetype = omn)) +
    geom_smooth(method = "loess",
                size = 0.5,
                color = "salmon",
                fill = "salmon") +
    facet_grid(rows = vars(disturb),
               cols = vars(productivity),
               labeller = label_parsed) +
    labs(x = "Ecosystem size (number of patches)",
         y = "Food chain length",
         linetype = "Omnivory")
  
  return(g_np)
}

list_g_pb <- foreach(i = seq_len(nrow(df_param))) %do% {

  g_pb <- df_sim %>% 
    filter(theta == df_param$theta[i],
           sd_disturb_lon == df_param$sd_disturb_lon[i],
           sd_disturb_source == df_param$sd_disturb_source[i]) %>% 
    ggplot(aes(x = p_branch,
               y = fcl,
               linetype = omn)) +
    geom_smooth(method = "loess",
                size = 0.5,
                color = "steelblue",
                fill = "steelblue") +
    facet_grid(rows = vars(disturb),
               cols = vars(productivity),
               labeller = label_parsed) +
    labs(x = "Ecosystem complexity (branching prob.)",
         y = "Food chain length",
         linetype = "Omnivory")
  
  return(g_pb)
}


list_g_fcl <- list()
list_g_fcl[[1]] <- list_g_np[[1]]
list_g_fcl[[2]] <- list_g_pb[[1]]
