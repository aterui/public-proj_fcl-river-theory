
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))

# network -----------------------------------------------------------------

n_patch <- 50
p_branch <- c(0.2, 0.5, 0.8)
sd_lon <- sd_source <- c(0.01, 1)

para <- expand.grid(n_patch = n_patch,
                    p_branch = p_branch,
                    sd_source = sd_source,
                    sd_lon = sd_lon) %>% 
  filter(sd_lon < sd_source)


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
                   edge_width = 0.1) +
    geom_node_point(aes(fill = disturb),
                    shape = 21,
                    color = grey(0.5),
                    stroke = 0.1) +
    MetBrewer::scale_fill_met_c("Hiroshige", direction = -1) +
    guides(fill = ifelse(i == 3, guide_legend(), "none")) +
    labs(subtitle = paste("Branching prob. =",
                          para$p_branch[i]),
         fill = "Disturbance") +
    theme(rect = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(g)
}

g_net <- (ng[[1]] + labs(title = "A")) + ng[[2]] + ng[[3]] + plot_layout(guide = "collect")


# disturbance distribution ------------------------------------------------

theme_set(plt_theme)

df_assemble <- foreach(i = seq_len(nrow(para))) %do% {
  
  foreach(j = 1:10,
          .combine = bind_rows) %do% {
            set.seed(j)
            x <- brnet(n_patch = para$n_patch[i],
                       p_branch = para$p_branch[i],
                       mean_disturb_source = 0.8,
                       sd_disturb_source = para$sd_source[i],
                       sd_disturb_lon = para$sd_lon[i],
                       plot = FALSE)
            
            return(x$df_patch)
          }  
  
}

g_disturb <- bind_rows(mutate(df_assemble[[1]], p_branch = 0.2),
                       mutate(df_assemble[[2]], p_branch = 0.5),
                       mutate(df_assemble[[3]], p_branch = 0.8)) %>% 
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
