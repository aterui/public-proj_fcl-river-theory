
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))


# data --------------------------------------------------------------------

lab1 <- c(`0` = "No~switching~(s==0)",
          `0.5` = "Strong~switching~(s==0.5)")

lab2 <- c(`2` = "r[B]==2",
          `20` = "r[B]==20")

lab3 <- c(`100` = "K==100",
          `500` = "K==500")

g_one <- readRDS(here::here("output/sim_one_patch.rds")) %>% 
  group_by(param_set) %>% 
  summarize(across(.fns = unique, .cols = -fcl),
            fcl = median(fcl)) %>% 
  ggplot(aes(x = a,
             y = h,
             fill = fcl)) +
  geom_raster(alpha = 0.8) +
  geom_point(data = expand.grid(a = c(0.025, 0.25),
                                h = c(0.5, 1.25),
                                fcl = 0) %>% 
               filter(!(a != 0.025 & h == 0.5)),
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
