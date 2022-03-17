
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/figure_theme.R"))

# network -----------------------------------------------------------------

n_patch <- 100
p_branch <- c(0.2, 0.5, 0.8)
sd_lon <- sd_source <- c(0.1, 5)

para <- expand.grid(n_patch = n_patch,
                    p_branch = p_branch,
                    sd_source = sd_source,
                    sd_lon = sd_lon) %>% 
  filter(!(sd_lon == sd_source))


net <- foreach(i = seq_len(nrow(para))) %do% {
  set.seed(123)
  brnet(n_patch = para$n_patch[i],
        p_branch = para$p_branch[i],
        mean_disturb_source = 0.8,
        sd_disturb_source = para$sd_source[i],
        sd_disturb_lon = para$sd_lon[i],
        plot = FALSE)
}

v_disturb <- unlist(lapply(seq_len(length(net)),
                           FUN = function(x) net[[x]]$df_patch$disturbance))

colvalue <- data.frame(color = viridis::viridis(length(v_disturb)),
                       value = sort(v_disturb))


ng <- foreach(i = seq_len(length(net))) %do% {
  
  adj <-  net[[i]]$adjacency_matrix %>%
    graph.adjacency()
  
  V(adj)$disturb <- net[[i]]$df_patch$disturbance
  
  g <- adj %>% 
    ggraph(layout = layout_as_tree(.,
                                   flip.y = FALSE,
                                   root = 1)) +
    geom_edge_link(color = "steelblue") +
    geom_node_point(shape = 21,
                    fill = colvalue$color[match(V(adj)$disturb, colvalue$value)],
                    color = grey(0.5),
                    size = 3) +
    labs(subtitle = paste("Branching prob. =", para$p_branch[i])) +
    theme_graph()
  
  return(g)
}

g_net <- (ng[[1]] + ggtitle("C")) | ng[[2]] | ng[[3]]


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
       fill = "Branching probability") +
  guides(color = "none") +
  ggtitle("D")
