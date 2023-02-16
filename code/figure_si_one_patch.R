
# setup -------------------------------------------------------------------

rm(list = ls())
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))


# data --------------------------------------------------------------------

lab <- list(c(`0` = "h==0.0",
              `0.25` = "h==0.25",
              `0.5` = "h==0.50",
              `0.75` = "h==0.75",
              `1` = "h==1.00"),
            c(`2` = "r[B]==2",
              `8` = "r[B]==8",
              `14` = "r[B]==14",
              `20` = "r[B]==20"))

df_fcl <- readRDS(here::here("output/sim_one_patch.rds"))
k_set <- unique(df_fcl$k)
a1_set <- unique(df_fcl$a1)

foreach(i = 1:length(a1_set)) %do% {
  
  list_g_one <- foreach(j = 1:length(k_set)) %do% {
    
    g_one <- df_fcl %>% 
      group_by(param_set) %>% 
      summarize(across(.fns = unique, .cols = -c(fcl, state)),
                state = max(state)) %>% 
      filter(a1 == a1_set[i],
             k == k_set[j]) %>% 
      ggplot(aes(x = a2,
                 y = a3,
                 fill = factor(state))) +
      geom_raster(alpha = 0.8) +
      facet_grid(rows = vars(r_b),
                 cols = vars(h), labeller = labeller(h = as_labeller(lab[[1]], label_parsed),
                                                        r_b = as_labeller(lab[[2]], label_parsed))) +
      scale_fill_viridis_d(limits = factor(0:4)) +
      scale_x_continuous(breaks = c(0, 0.05, 0.1)) +
      labs(x = expression("Attack rate ("*a[BP]*")"),
           y = expression("Attack rate ("*a[CP]*")"),
           fill = "State") +
      theme_classic() +
      theme(strip.background = element_blank(),
            strip.text = element_text(size = 12), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 10)) +
      ggtitle(paste0("K = ", k_set[j])) +
      geom_point(data = expand.grid(a2 = c(0, 0.02, 0.04),
                                    a3 = 0.02,
                                    h = 0.5,
                                    state = 0))
    
    return(g_one)
  }
  
  g_one_joint <- list_g_one[[1]] / list_g_one[[2]] + 
    plot_annotation(tag_levels = "A") +
    plot_layout(guides = "collect")
  
  # export ------------------------------------------------------------------
  
  ggsave(g_one_joint, 
         filename = here::here(paste0("figure/si_figure_one_patch_a1_",
                                      a1_set[i] * 100,
                                      ".pdf")),
         width = 10,
         height = 15)
  
}


