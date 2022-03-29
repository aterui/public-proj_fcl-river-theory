
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# analysis ----------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_sim <- sim_main_result %>% 
  group_by(param_set) %>% 
  filter(sd_disturb_source == 3,
         sd_disturb_lon == 0.1) %>% 
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
                          levels = rev(levels(factor(disturb)))),
         dispersal = recode(theta,
                            `0.1` = sprintf('"Long dispersal"~(theta=="%.2f")',
                                            theta),
                            `1` = sprintf('"Short dispersal"~(theta=="%.2f")',
                                          theta)),
         
  )


# figure ------------------------------------------------------------------

theme_set(plt_theme)
param <- c(0.01, 0.1)

## Ecosystem size
g_np_list <- foreach(i = seq_len(length(param))) %do% {
  df_sim %>% 
    filter(p_disturb == param[i]) %>% 
    ggplot(aes(x = n_patch,
               y = fcl,
               color = omn,
               fill = omn)) +
    geom_smooth(method = "loess",
                size = 0.5) +
    facet_grid(rows = vars(dispersal),
               cols = vars(productivity),
               labeller = label_parsed) +
    labs(x = "Ecosystem size (number of patches)",
         y = "Food chain length",
         color = "Omnivory",
         fill = "Omnivory") +
    MetBrewer::scale_color_met_d("Hiroshige") +
    MetBrewer::scale_fill_met_d("Hiroshige")
}

## Ecosystem complexity
g_pb_list <- foreach(i = seq_len(length(param))) %do% {
  df_sim %>% 
    filter(p_disturb == param[i]) %>% 
    ggplot(aes(x = p_branch,
               y = fcl,
               color = omn,
               fill = omn)) +
    geom_smooth(method = "loess",
                size = 0.5) +
    facet_grid(rows = vars(dispersal),
               cols = vars(productivity),
               labeller = label_parsed) +
    labs(x = "Ecosystem complexity (branching prob.)",
         y = "Food chain length",
         color = "Omnivory",
         fill = "Omnivory") +
    MetBrewer::scale_color_met_d("Hiroshige") +
    MetBrewer::scale_fill_met_d("Hiroshige")
}

## merge
g_ld <- g_np_list[[1]] + g_pb_list[[1]] + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")
g_hd <- g_np_list[[2]] + g_pb_list[[2]] + plot_annotation(tag_levels = "A") + plot_layout(guides = "collect")

ggsave(g_ld,
       filename = here::here("output/figure_ld_main.pdf"),
       height = 7.5,
       width = 12)

ggsave(g_hd,
       filename = here::here("output/figure_hd_main.pdf"),
       height = 7.5,
       width = 12)

