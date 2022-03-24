
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))

# network -----------------------------------------------------------------

n_patch <- 50
p_branch <- c(0.2, 0.5, 0.8)
sd_lon <- sd_source <- c(0.1, 3)

para <- expand.grid(n_patch = n_patch,
                    p_branch = p_branch,
                    sd_source = sd_source,
                    sd_lon = sd_lon) %>% 
  filter(!(sd_lon == sd_source))


net <- foreach(i = seq_len(nrow(para))) %do% {
  set.seed(124)
  brnet(n_patch = para$n_patch[i],
        p_branch = para$p_branch[i],
        mean_disturb_source = 0.8,
        sd_disturb_source = para$sd_source[i],
        sd_disturb_lon = para$sd_lon[i],
        plot = FALSE)
}

ng <- foreach(i = seq_len(length(net))) %do% {
  
  adj <-  net[[i]]$adjacency_matrix %>%
    graph.adjacency()
  
  V(adj)$disturb <- net[[i]]$df_patch$disturbance
  
  g <- adj %>% 
    ggraph(layout = layout_as_tree(.,
                                   flip.y = FALSE,
                                   root = 1)) +
    geom_edge_link(color = "steelblue",
                   edge_width = 0.01) +
    geom_node_point(aes(fill = disturb),
                    shape = 21,
                    color = grey(0.5),
                    stroke = 0.1,
                    size = 2) +
    MetBrewer::scale_fill_met_c("Hiroshige") +
    #scale_fill_continuous(high = "steelblue",
    #                      low = grey(0.95)) +
    guides(fill = ifelse(i == 3, guide_legend(), "none")) +
    labs(subtitle = paste("Branching prob. =",
                          para$p_branch[i]),
         fill = "Disturbance") +
    theme(rect = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(g)
}

g_net <- (ng[[1]] + labs(title = "C")) + ng[[2]] + ng[[3]] + plot_layout(guide = "collect")


# disturbance distribution ------------------------------------------------

theme_set(plt_theme)

g_disturb <- bind_rows(mutate(net[[1]]$df_patch, p_branch = 0.2),
                       mutate(net[[2]]$df_patch, p_branch = 0.5),
                       mutate(net[[3]]$df_patch, p_branch = 0.8)) %>% 
  ggplot(aes(x = disturbance,
             color = factor(p_branch),
             fill = factor(p_branch))) +
  geom_density(alpha = 0.2) +
  geom_vline(xintercept = 0.8,
             color = grey(0.5),
             linetype = "dashed") +
  labs(x = "Disturbance intensity",
       y = "Density",
       fill = "Branching prob.",
       color = "Branching prob.")