
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# data --------------------------------------------------------------------

lab <- list(c(`0` = "No~switching~(s==0)",
              `0.5` = "Weak~switching~(s==0.5)",
              `1` = "Strong~switching~(s==1)"),
            c(`2` = "r[B]==2",
              `8` = "r[B]==8",
              `14` = "r[B]==14",
              `20` = "r[B]==20"),
            c(`100` = "K==100",
              `500` = "K==500"))

df_fcl <- readRDS(here::here("output/sim_one_patch.rds"))
h_set <- unique(df_fcl$h)

foreach(i = 1:length(h_set)) %do% {
  
  g_one <- df_fcl %>% 
    group_by(param_set) %>% 
    summarize(across(.fns = unique, .cols = -c(fcl, state)),
              state = max(state)) %>% 
    filter(h == h_set[i]) %>% 
    ggplot(aes(x = a1,
               y = a2,
               fill = factor(state))) +
    geom_raster(alpha = 0.8) +
    geom_point(data = expand.grid(a1 = 0.1,
                                  a2 = c(0.01, 0.05),
                                  state = 0)) +
    facet_grid(rows = vars(r_b),
               cols = vars(s, k), labeller = labeller(s = as_labeller(lab[[1]], label_parsed),
                                                      r_b = as_labeller(lab[[2]], label_parsed),
                                                      k = as_labeller(lab[[3]], label_parsed))) +
    #scale_fill_met_d("Hiroshige",  direction = -1) +
    scale_fill_viridis_d(limits = factor(0:4)) +
    labs(x = expression("Attack rate ("*a[CP]*")"),
         y = expression("Attack rate ("*a[BP]*")"),
         fill = "State") +
    theme_classic() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12), 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 10))
  
  # export ------------------------------------------------------------------
  
  ggsave(g_one, 
         filename = here::here(paste0("figure/figure_one_patch_h",
                                      h_set[i] * 100,
                                      ".pdf")),
         width = 12,
         height = 11)
  
}


