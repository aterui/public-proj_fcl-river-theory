
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))


# figure ------------------------------------------------------------------

delta <- c(0, 0.05, 0.95)
title_fw <- c("Chain", "Weak omnivory", "Strong omnivory")

g_fw <- foreach(i = 1:3) %do% {
  
  m_weight <- rbind(c(0, 1, 1, 0),
                    c(0, 0, 1, 0),
                    c(0, 0, 0, 0),
                    c(0, 0, 0, 0))
  
  m_weight[1, 3] <- delta[i]
  m_weight[2, 3] <- 1 - delta[i]
  
  graph <- as_tbl_graph(m_weight)
  
  layout <- create_layout(graph, "fr")
  
  if (m_weight[2, 3] == 1) {
    
    layout$x <- c(1, 1, 1, 1)  
    
  } else {
    
    layout$x <- c(1, 0.5, 1.5, 1.5)
    
  }
  
  layout$y <- TrophInd(m_weight)$TL
  layout$y[4] <- 3
  layout$label <- c("B", "C", "P", "")
  
  g <- ggraph(layout) + 
    geom_edge_link(arrow = arrow(type = "closed",
                                 ends = "last",
                                 length = unit(5, "mm"),
                                 angle = 15),
                   aes(edge_color = weight),
                   start_cap = circle(7, 'mm'),
                   end_cap = circle(7, 'mm'),
                   show.legend = T) +
    scale_edge_color_continuous(low = "lightgrey",
                                high = "black",
                                guide = "none") +
    geom_node_label(aes(label = label), 
                    color = grey(0.25),
                    size = 10,
                    label.size = 0,
                    fill = NA) +
    theme_graph() +
    labs(title = title_fw[i])
  
  return(g)
}

g_web <- g_fw[[1]] |
  g_fw[[2]] |
  g_fw[[3]]