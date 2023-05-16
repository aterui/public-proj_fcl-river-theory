
# setup -------------------------------------------------------------------

rm(list = ls(all.names = TRUE))
source(here::here("code/library.R"))
source(here::here("code/set_figure_theme.R"))
source(here::here("code/format_sim_data.R"))


# figure ------------------------------------------------------------------

## size effect
m1 <- matrix(0, 12, 12)
m1[1, c(2, 3)] <- 1 # disturbance frequency
m1[2, 4:6] <- 1 # no distb x omn
m1[3, 7:8] <- 1 # disturbance intensity
m1[4, 9:10] <- 1 # freq:no x omn:chain x prod
m1[5, 11:12] <- 1 # freq:no x omn:weak x prod

adj <- igraph::graph.adjacency(m1)

V(adj)$label[1] <- "Disturbance frequency"
V(adj)$label[c(2, 8)] <- "Omnivory"
V(adj)$label[3] <- "Disturbance intensity"
V(adj)$label[4:5] <- "Productivity"

V(adj)$label[c(6, 10, 11)] <- "No effect"
V(adj)$label[7] <- "Very weak effect \n (Fig. S3)"
V(adj)$label[c(8:9, 12)] <- "Positive"

V(adj)$color[c(1:dim(m1)[1])] <- "node"
V(adj)$color[c(6, 7, 8:12)] <- "pattern"

E(adj)$label[1] <- "Absent"; E(adj)$label[2] <- "Present"
E(adj)$label[3:5] <- c("None (Chain)", "Weak", "Strong")
E(adj)$label[6:7] <- c("Weak", "Strong")
E(adj)$label[8:11] <- rep(c("Low", "High"), 2)

g_tree1 <- ggraph(adj, layout = 'tree') +
  geom_edge_link(aes(label = label),
                 color = grey(0.7),
                 linetype = "dashed",
                 arrow = arrow(length = unit(2,'mm'),
                               type = "closed"),
                 start_cap = circle(8,'mm'),
                 end_cap = circle(8,'mm')) +
  geom_node_label(aes(label = label,
                      color = color),
                  label.size = 0) +
  scale_color_manual(values = c("black", "steelblue")) +
  theme_void() +
  guides(color = "none")


## Complexity effect
m2 <- matrix(0, 10, 10)
m2[1, 2:3] <- 1
m2[2, 4:6] <- 1
m2[3, 7:8] <- 1
m2[4, 9:10] <- 1

adj <- igraph::graph.adjacency(m2)

V(adj)$label[1] <- "Disturbance frequency"
V(adj)$label[2] <- "Omnivory"
V(adj)$label[3] <- "Disturbance intensity"
V(adj)$label[4] <- "Productivity"

V(adj)$label[c(5, 8)] <- "Positive"
V(adj)$label[c(6, 10)] <- "No effect"
V(adj)$label[7] <- "Very weak effect \n (Fig. S4)"
V(adj)$label[9] <- "Negative"

V(adj)$color[1:10] <- "node"
V(adj)$color[c(5:10)] <- "pattern"

E(adj)$label[1:(dim(m2)[1] - 1)] <- "NA"
E(adj)$label[1:2] <- c("Absent", "Present")
E(adj)$label[3:5] <- c("None (Chain)", "Weak", "Strong")
E(adj)$label[6:7] <- c("Weak", "Strong")
E(adj)$label[8:9] <- c("Low", "High")

g_tree2 <- ggraph(adj, layout = 'tree') +
  geom_edge_link(aes(label = label),
                 color = grey(0.7),
                 linetype = "dashed",
                 arrow = arrow(length = unit(2,'mm'),
                               type = "closed"),
                 start_cap = circle(8,'mm'),
                 end_cap = circle(8,'mm')) +
  geom_node_label(aes(label = label,
                      color = color),
                  label.size = 0) +
  scale_color_manual(values = c("black", "steelblue")) +
  theme_void() +
  guides(color = "none")


# merge figure ------------------------------------------------------------

g_tree <- (g_tree1 + ggtitle("(A) Ecosystem size")) + (g_tree2 + ggtitle("(B) Ecosystem complexity"))

ggsave(g_tree,
       filename = "figure/figure_tree.pdf",
       width = 14,
       height = 6)
