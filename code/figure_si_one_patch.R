
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# data --------------------------------------------------------------------

lab1 <- c(`0` = "No~switching~(s==0)",
          `1` = "Strong~switching~(s==1)")

lab2 <- c(`5` = "r[B]==5",
          `10` = "r[B]==10",
          `20` = "r[B]==20",
          `40` = "r[B]==40")

g_one <- readRDS(here::here("output/sim_one_patch.rds")) %>% 
  group_by(param_set) %>% 
  summarize(across(.fns = unique, .cols = -fcl),
            fcl = median(fcl)) %>% 
  ggplot(aes(x = a,
             y = h,
             fill = fcl)) +
  geom_raster(alpha = 0.8) +
  facet_grid(rows = vars(r_b),
             cols = vars(s), labeller = labeller(s = as_labeller(lab1, label_parsed),
                                                 r_b = as_labeller(lab2, label_parsed))) +
  MetBrewer::scale_fill_met_c("Hiroshige",
                              direction = -1) +
  labs(x = "Attack rate (a)",
       y = "Handling time (h)",
       fill = "FCL") +
  theme_classic() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12), 
        axis.title =  element_text(size = 14),
        axis.text =  element_text(size = 10))


# export ------------------------------------------------------------------

ggsave(g_one, 
       filename = here::here("figure/figure_one_patch.pdf"),
       width = 7.5,
       height = 10)
