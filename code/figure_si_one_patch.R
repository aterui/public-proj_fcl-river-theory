
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# data --------------------------------------------------------------------

lab1 <- c(`0` = "No~switching~(s==0)",
          `1` = "Strong~switching~(s==1)")

lab2 <- c(`5` = "r[B]==5",
          `10` = "r[B]==10")

lab3 <- c(`500` = "K==500",
          `1000` = "K==1000")

g_one <- readRDS(here::here("output/sim_one_patch.rds")) %>% 
  group_by(param_set) %>% 
  summarize(across(.fns = unique, .cols = -fcl),
            fcl = median(fcl)) %>% 
  ggplot(aes(x = a,
             y = h,
             fill = fcl)) +
  geom_raster(alpha = 0.8) +
  geom_point(data = expand.grid(a = c(0.025, 0.5),
                                h = c(0.5, 1.25),
                                fcl = 0) %>% 
               filter(!(a == 0.5 & h == 0.5)),
             color = "black") +
  facet_grid(rows = vars(r_b),
             cols = vars(s, k), labeller = labeller(s = as_labeller(lab1, label_parsed),
                                                    r_b = as_labeller(lab2, label_parsed),
                                                    k = as_labeller(lab3, label_parsed))) +
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
       width = 12,
       height = 6)
