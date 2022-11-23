library(dplyr)
library(janitor)
library(tidyr)
library(rstatix)
library(readr)

norm_table <- read.csv("таблица здоровых.csv")
disease_table <- read.csv("таблица больных.csv")

norm_table <- norm_table[,-1]
disease_table <- disease_table[,-1]

table_disease2 <- disease_table %>% tidyr::gather(Samples, Expression, park2_pd1np_1:park2_pd3dn_3) %>%
  tidyr::spread(genes, Expression)

rownames(table_disease2) <- table_disease2$Samples
table_disease2 <- table_disease2[,-1]

table_norm2 <- norm_table %>% tidyr::gather(Samples, Expression, norma1np_1:norma1dn_3) %>%
  tidyr::spread(genes, Expression)

rownames(table_norm2) <- table_norm2$Samples
table_norm2 <- table_norm2[,-1]


cor.mat <- table_norm2 %>% cor_mat(method="spearman")

cor.mat <- cor.mat[-c(13, 36, 49, 64, 67, 82, 101), -c(14, 37, 50, 65, 68, 83, 102)]

cor.mat %>% cor_reorder() %>% pull_lower_triangle() %>% cor_plot()

corr_norm1 <- table_norm2 %>% cor_test(method="spearman")

corr_norm <- filter(corr_norm1, p<0.05)

write.csv(corr_norm, "корреляция здоровые, p.csv")


cor.mat2 <- table_disease2 %>% cor_mat(method="spearman")

cor.mat2 <- cor.mat2[-c(36, 49, 82), -c(37, 50, 83)]

cor.mat2 %>% cor_reorder() %>% pull_lower_triangle() %>% cor_plot()

corr_disease1 <- table_disease2 %>% cor_test(method="spearman")

corr_disease <- filter(corr_disease1, p<0.05)

write.csv(corr_disease, "корреляция больные, p.csv")
