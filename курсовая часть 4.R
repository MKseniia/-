library(tidygraph)
library(ggraph)
library(readr)
library(dplyr)

corr_norm_p <- read_csv("корреляция здоровые, p.csv")
corr_disease_p <- read_csv("корреляция больные, p.csv")
corr_norm_p <- corr_norm_p[,c(-1, -5)]
corr_disease_p <- corr_disease_p[c(-558, -1715, -4356),c(-1, -5)]

norm_graph <- as_tbl_graph(corr_norm_p, directed = FALSE)

disease_graph <- as_tbl_graph(corr_disease_p, directed = FALSE)


corr_norm_p$cor <- abs(corr_norm_p$cor)

hub_norm <- norm_graph %>% 
  mutate(importance = centrality_hub(weights = corr_norm_p$cor))

hub_norm_data <- as.data.frame(hub_norm)

important_norm_hub <- filter(hub_norm_data, importance > 0.96)


corr_norm_pb <- corr_norm_p
corr_norm_pb$cor <- 1 - corr_norm_pb$cor
corr_norm_pb <- filter(corr_norm_pb, corr_norm_pb$cor != 0)

norm_graph_b <- as_tbl_graph(corr_norm_pb, directed = FALSE)

betw_norm <- norm_graph_b %>%
  mutate(importance = centrality_betweenness(corr_norm_pb$cor))

betw_norm_data <- as.data.frame(betw_norm)

important_norm_betw <- filter(betw_norm_data, importance > 344)


------------------------------------------------------

corr_disease_p$cor <- abs(corr_disease_p$cor)

hub_disease <- disease_graph %>% 
  mutate(importance = centrality_hub(weights = corr_disease_p$cor))

hub_disease_data <- as.data.frame(hub_disease)

important_dis_hub <- filter(hub_disease_data, importance > 0.957)


corr_disease_pb <- corr_disease_p
corr_disease_pb$cor <- 1 - corr_disease_pb$cor
corr_disease_pb <- filter(corr_disease_pb, corr_disease_pb$cor != 0)

disease_graph_b <- as_tbl_graph(corr_disease_pb, directed = FALSE)

betw_disease <- disease_graph_b %>%
  mutate(importance = centrality_betweenness(corr_disease_pb$cor))

betw_disease_data <- as.data.frame(betw_disease)

important_dis_betw <- filter(betw_disease_data, importance > 360)

-----------------------------------------

groups_norm <- norm_graph_b %>%
  mutate(group = group_edge_betweenness(weights = corr_norm_pb$cor))

groups_norm_data <- as.data.frame(groups_norm)

important_norm_groups <- filter(groups_norm_data, groups_norm_data$group == 1)
group1_norm <- c(important_norm_groups$name)


groups_disease <- disease_graph_b %>%
  mutate(group = group_edge_betweenness(weights = corr_disease_pb$cor))

groups_disease_data <- as.data.frame(groups_disease)

group1_disease <- filter(groups_disease_data, groups_disease_data$group == 1)
group1_disease <- c(group1_disease$name)

group2_disease <- filter(groups_disease_data, groups_disease_data$group == 2)
group2_disease <- c(group2_disease$name)

group3_disease <- filter(groups_disease_data, groups_disease_data$group == 3)
group3_disease <- c(group3_disease$name)



network_norm <- ggraph(norm_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = hub_norm_data$importance) +
  geom_node_text(aes(label = name, color = groups_norm_data$group), size = betw_norm_data$importance, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("corr_network_norm2.png", network_norm, width = 70, height = 70, units="cm")

network_disease <- ggraph(disease_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = hub_disease_data$importance) +
  geom_node_text(aes(label = name, color = groups_disease_data$group), size = betw_disease_data$importance, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("corr_network_disease2.png", network_disease, width = 70, height = 70, units="cm")