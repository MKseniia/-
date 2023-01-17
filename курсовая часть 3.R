library(tidygraph)
library(ggraph)
library(readr)
library(dplyr)

corr_norm_p <- read_csv("корреляция здоровые, p.csv")
corr_disease_p <- read_csv("корреляция больные, p.csv")
corr_norm_p <- corr_norm_p[,c(-1, -5)]
corr_disease_p <- corr_disease_p[c(-558, -1715, -4356),c(-1, -5)]


norm_graph <- as_tbl_graph(corr_norm_p, directed = FALSE)

network_norm <- ggraph(norm_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = 6) +
  geom_node_text(aes(label = name), size = 6, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("corr_network_norm.png", network_norm, width = 70, height = 70, units="cm")



disease_graph <- as_tbl_graph(corr_disease_p, directed = FALSE)

network_disease <- ggraph(disease_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = 6) +
  geom_node_text(aes(label = name), size = 6, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("corr_network_disease.png", network_disease, scale = 5)
