library(tidygraph)
library(ggraph)
library(readr)
library(dplyr)
library(readr)

corr_norm_p <- read_csv("корреляция здоровые, p.csv")
corr_disease_p <- read_csv("корреляция больные, p.csv")
corr_norm_p <- corr_norm_p[,c(-1, -5)]
corr_disease_p <- corr_disease_p[c(-558, -1715, -4356),c(-1, -5)]

norm_graph <- as_tbl_graph(corr_norm_p, directed = FALSE)

disease_graph <- as_tbl_graph(corr_disease_p, directed = FALSE)


corr_norm_p <- mutate(corr_norm_p, cor2 = abs(corr_norm_p$cor))

hub_norm <- norm_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_hub(weights = corr_norm_p$cor2)) %>%
  as.data.frame()

important_norm_hub <- filter(hub_norm, importance > 0.96)


betw_norm <- norm_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_betweenness(corr_norm_p$cor2)) %>%
  as.data.frame()

important_norm_betw <- filter(betw_norm, importance > 119)


#------------------------------------------------------

corr_disease_p <- mutate(corr_disease_p, cor2 = abs(corr_disease_p$cor))

hub_disease <- disease_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_hub(weights = corr_disease_p$cor2)) %>%
  as.data.frame()

important_dis_hub <- filter(hub_disease, importance > 0.957)


betw_disease <- disease_graph %>%
  activate(nodes) %>%
  mutate(importance = centrality_betweenness(corr_disease_p$cor2)) %>%
  as.data.frame()

important_dis_betw <- filter(betw_disease, importance > 130)

#-----------------------------------------

groups_norm <- norm_graph %>%
  activate(nodes) %>%
  mutate(group = group_edge_betweenness(weights = corr_norm_p$cor2)) %>%
  as.data.frame()

important_norm_groups <- filter(groups_norm, groups_norm$group < 5)

group1_norm <-  filter(important_norm_groups, group == 1)
group1_norm <- c(group1_norm$name)

group2_norm <-  filter(important_norm_groups, group == 2)
group2_norm <- c(group2_norm$name)

group3_norm <-  filter(important_norm_groups, group == 3)
group3_norm <- c(group3_norm$name)

group4_norm <-  filter(important_norm_groups, group == 4)
group4_norm <- c(group4_norm$name)


groups_disease <- disease_graph %>%
  activate(nodes) %>%
  mutate(group = group_edge_betweenness(weights = corr_disease_p$cor2)) %>%
  as.data.frame()

important_dis_groups <- filter(groups_disease, groups_disease$group == 1)
group1_disease <- c(important_dis_groups$name)




hub_norm <- mutate(hub_norm, log = 9 + log2(hub_norm$importance))

betw_norm <- mutate(betw_norm, log = 2 + log2(betw_norm$importance))

betw_norm[2, 3] <-  1
betw_norm[24, 3] <-  1
betw_norm[63, 3] <-  1
betw_norm[76, 3] <-  1
betw_norm[88, 3] <-  1
betw_norm[96, 3] <-  1
betw_norm[144, 3] <-  1

hub_disease <- mutate(hub_disease, log = 11 + log2(hub_disease$importance))

betw_disease <- mutate(betw_disease, log = 2 + log2(betw_disease$importance))

betw_disease[24, 3] <- 0.9
betw_disease[63, 3] <- 0.9
betw_disease[124, 3] <- 0.9

#---------------------------------

network_norm <- ggraph(norm_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = hub_norm$log) +
  geom_node_text(aes(label = name, color = groups_norm$group), size = betw_norm$log, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("corr_network_norm2.png", network_norm, width = 70, height = 70, units="cm")

network_disease <- ggraph(disease_graph) + 
  geom_edge_link(aes(color = cor, width = cor)) + 
  geom_node_point(size = hub_disease$log) +
  geom_node_text(aes(label = name, color = groups_disease$group), size = betw_disease$log, repel = TRUE) +
  theme_graph() +
  scale_edge_color_gradient2(low = "blue", high = "red", mid = "white")

ggsave("corr_network_disease4.png", network_disease, width = 70, height = 70, units="cm")

write.csv(important_dis_betw, "betweennes disease.csv")
write.csv(important_dis_groups, "groups disease.csv")
write.csv(important_dis_hub, "hub disease.csv")
write.csv(important_norm_betw, "betweennes norm.csv")
write.csv(important_norm_groups, "groups norm.csv")
write.csv(important_norm_hub, "hub norm.csv")