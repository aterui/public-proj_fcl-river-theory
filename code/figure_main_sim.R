
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# analysis ----------------------------------------------------------------

df_sim <- readRDS(file = here::here("output/sim_main.rds"))

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

df_coef <- df_sim %>% 
  group_by(param_set) %>% 
  do(param_set = unique(.$param_set),
     rho1 = cor(.$fcl, .$n_patch, method = "spearman"),
     rho2 = cor(.$fcl, .$p_branch, method = "spearman")) %>% 
  summarize(param_set = param_set,
            rho1 = unlist(rho1),
            rho2 = unlist(rho2))

# h, s, theta, a_bp, mean_disturb_source
df0 <- df_param %>% 
  left_join(df_coef,
            by = "param_set") %>% 
  filter(!(s == 0 & a_bp != 0),
         h == 1.25,
         mean_disturb_source == 0.8) %>% 
  mutate(omn = case_when(a_bp == 0 ~ "Chain",
                         a_bp == 0.025 ~ "Weak",
                         a_bp == 0.25 ~ "Strong"),
         omn = fct_relevel(omn, c("Chain", "Weak", "Strong")),
         disp = case_when(theta == 1 ~ "Short dispersal",
                          theta == 0.1 ~ "Long dispersal"))


# figure ------------------------------------------------------------------

theme_set(plt_theme)

## Ecosystem size
g_size <- df0 %>% 
  ggplot(aes(x = r_b,
             y = p_disturb,
             fill = rho1)) +
  geom_raster() +
  facet_grid(cols = vars(omn),
             rows = vars(disp)) +
  scale_fill_gradient2(mid = 0,
                       limits = c(min(df0$rho1, df0$rho2, na.rm = T),
                                  max(df0$rho1, df0$rho2, na.rm = T))
                       ) + 
  theme_classic() +
  theme(strip.background = element_blank())

## Ecosystem complexity
g_branch <- df0 %>% 
  ggplot(aes(x = r_b,
             y = p_disturb,
             fill = rho2)) +
  geom_raster() +
  facet_grid(cols = vars(omn),
             rows = vars(disp)) +
  scale_fill_gradient2(mid = 0,
                       limits = c(min(df0$rho1, df0$rho2, na.rm = T),
                                  max(df0$rho1, df0$rho2, na.rm = T))
  ) + 
  theme_classic() +
  theme(strip.background = element_blank())

g_size / g_branch + plot_layout(guides = "collect")



# test --------------------------------------------------------------------

df_sim %>% 
  filter(!(s == 0 & a_bp != 0),
         h == 1.25,
         mean_disturb_source == 0.8,
         r_b == 20) %>% 
  mutate(omn = case_when(a_bp == 0 ~ "Chain",
                         a_bp == 0.025 ~ "Weak",
                         a_bp == 0.25 ~ "Strong"),
         omn = fct_relevel(omn, c("Chain", "Weak", "Strong")),
         disp = case_when(theta == 1 ~ "Short dispersal",
                          theta == 0.1 ~ "Long dispersal")) %>% 
  ggplot(aes(x = p_branch,
             y = fcl,
             color = factor(p_disturb),
             fill = factor(p_disturb))) +
  geom_smooth(method = "loess") +
  facet_grid(rows = vars(omn),
             cols = vars(disp))
