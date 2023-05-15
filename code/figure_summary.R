
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))
source(here::here("code/format_sim_data.R"))


# data --------------------------------------------------------------------

df_rho <- df_sim %>% 
  group_by(param_set) %>% 
  do(rho_n = cor(.$fcl, .$n_patch, method = "spearman"),
     rho_pb = cor(.$fcl, .$p_branch, method = "spearman")) %>% 
  mutate(across(.cols = where(is.list), .fns = unlist)) %>% 
  left_join(df_param,
            by = "param_set") %>% 
  filter(mean_disturb_source == 0.8)
