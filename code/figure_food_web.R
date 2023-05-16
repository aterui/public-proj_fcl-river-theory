
# setup -------------------------------------------------------------------

#rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))


# figure ------------------------------------------------------------------

delta <- c(0, 0.30, 0.70)
title_fw <- c("Chain", "Weak omnivory", "Strong omnivory")

g_fw <- foreach(i = 1:3) %do% {
  
  # interaction matrix; sp 4 & 5 are dummy
  m_weight <- rbind(c(0, 1, 1, 0, 0),
                    c(0, 0, 1, 0, 0),
                    c(0, 0, 0, 0, 0),
                    c(0, 0, 0, 0, 0),
                    c(0, 0, 0, 0, 0))
  
  m_weight[1, 3] <- delta[i]
  m_weight[2, 3] <- 1 - delta[i]
  
  # ggraph object
  graph <- as_tbl_graph(m_weight)
  
  # trophic position
  layout <- create_layout(graph, "fr")
  layout$y <- TrophInd(m_weight)$TL
  layout$y[4] <- layout$y[5] <- 3
  layout$label <- c("B", "C", "P", "", "")
  
  if (m_weight[2, 3] == 1) {
    
    layout$x <- rep(0.5, 5)
    
    g <- ggraph(layout) + 
      geom_edge_link(arrow = arrow(type = "closed",
                                   ends = "last",
                                   length = unit(0.1, "npc"),
                                   angle = 20),
                     start_cap = circle(0.07, 'npc'),
                     end_cap = circle(0.07, 'npc'),
                     show.legend = T) +
      geom_node_label(aes(label = label), 
                      color = grey(0.25),
                      size = 6,
                      label.size = 0,
                      fill = NA) +
      labs(subtitle = title_fw[i]) +
      theme(rect = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())
    
  } else {
    
    layout$x <- c(0.5, # Basal
                  0.5 - 1/layout$y[3], # Intraguild prey
                  0.5 + 1/layout$y[3], # Intraguild predator
                  0, # dummy 1
                  1 # dummy 2
    )
    
    g <- ggraph(layout) + 
      geom_edge_link(arrow = arrow(type = "closed",
                                   ends = "last",
                                   length = unit(0.1, "npc"),
                                   angle = 20),
                     aes(edge_color = weight),
                     start_cap = circle(0.07, 'npc'),
                     end_cap = circle(0.07, 'npc'),
                     show.legend = T) +
      geom_node_label(aes(label = label), 
                      color = grey(0.25),
                      size = 6,
                      label.size = 0,
                      fill = NA) +
      scale_edge_color_gradient(low = "grey",
                                high = "black") +
      labs(subtitle = title_fw[i]) +
      theme(rect = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      guides(edge_color = "none")
    
  }
  
  return(g)
}

g_web <- (g_fw[[1]] + ggtitle("C")) | g_fw[[2]] | g_fw[[3]]