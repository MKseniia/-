library(dplyr)
library(readr)
library(tibble)
library(cluster)
library(tidyverse)
library(factoextra)
library(dendextend)

norm_table <- read.csv("таблица здоровых.csv")
disease_table <- read.csv("таблица больных.csv")

norm_table <- norm_table[,-1]
disease_table <- disease_table[,-1]

norm_table <- column_to_rownames(norm_table, var = "genes")
disease_table <- column_to_rownames(disease_table, var = "genes")

dist_norm <- dist(norm_table, method = "euclidean")
dist_disease <- dist(disease_table, method = "euclidean")

hc_norm <- hclust(dist_norm, method = "complete")
hc_disease <- hclust(dist_disease, method = "complete")

tanglegram(hc_norm, hc_disease)

cor_cophenetic(hc_norm, hc_disease) # 0,5702325