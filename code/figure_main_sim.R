
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# analysis ----------------------------------------------------------------

load(file = here::here("output/result_main.RData"))

df_sim <- sim_main_result %>% 
  group_by(param_set) %>% 
  filter(theta == 1,
         sd_disturb_source == 5,
         sd_disturb_lon == 0.1) %>% 
  mutate(igp = case_when(e_bp == 0 ~ "Chain",
                         e_bp == 2 ~ "Weak",
                         e_bp == 4 ~ "Strong"),
         igp = factor(igp, levels = c("Chain",
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


# figure ------------------------------------------------------------------

theme_set(plt_theme)

g_np <- df_sim %>% 
  ggplot(aes(x = n_patch,
             y = fcl,
             linetype = igp)) +
  geom_smooth(method = "loess",
              size = 0.5,
              color = "salmon",
              fill = "salmon") +
  facet_grid(rows = vars(disturb),
             cols = vars(productivity),
             labeller = label_parsed) +
  labs(x = "Ecosystem size (number of patches)",
       y = "Food chain length",
       linetype = "IGP")

ggsave(g_np,
       filename = here::here("output/figure_np_main.pdf"),
       height = 6,
       width = 7.5)

g_pb <- df_sim %>% 
  ggplot(aes(x = p_branch,
             y = fcl,
             linetype = igp)) +
  geom_smooth(method = "loess",
              size = 0.5,
              color = "steelblue",
              fill = "steelblue") +
  facet_grid(rows = vars(disturb),
             cols = vars(productivity),
             labeller = label_parsed) +
  labs(x = "Ecosystem complexity (branching prob.)",
       y = "Food chain length",
       linetype = "IGP")

ggsave(g_pb,
       filename = here::here("output/figure_pb_main.pdf"),
       height = 6,
       width = 7.5)
