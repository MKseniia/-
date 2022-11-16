library(dplyr)
library(janitor)
library(tidyr)
library(rstatix)

tableNP <- read.table("GSE181029_TPM_NP.tsv", header=TRUE)
tableNP <- clean_names(tableNP)

tableDN <- read.table("GSE181029_TPM_DN.tsv", header=TRUE)
tableDN <- clean_names(tableDN)

t_gene <- read.csv("parkinsons.csv")

all1 <- inner_join(t_gene, tableNP, by=c('genes'='gene_name'))
all2 <- inner_join(t_gene, tableDN, by=c('genes'='gene_name'))

all2 <- subset(all2, select=-gene_id)
all1 <- subset(all1, select=-gene_id)

data_frame <- inner_join(all1, all2, by='genes')

table_norm <- select(data_frame, starts_with("g"), starts_with("n"))
table_disease <- select(data_frame, starts_with("g"), starts_with("p"))

table_disease2 <- table_disease %>% tidyr::gather(Idea, Warning_Level, park2_pd1np_1:park2_pd3dn_3) %>%
  tidyr::spread(genes, Warning_Level)

table_norm2 <- table_norm %>% tidyr::gather(Idea, Warning_Level, norma1np_1:norma1dn_3) %>%
  tidyr::spread(genes, Warning_Level)

rownames(table_norm2) <- table_norm2$Idea
table_norm2 <- table_norm2[,-1]

rownames(table_disease2) <- table_disease2$Idea
table_disease2 <- table_disease2[,-1]

table_norm2$ABCG2 <- as.numeric(table_norm2$ABCG2)
table_norm2$ADH1C <- as.numeric(table_norm2$ADH1C)
table_norm2$ADORA1 <- as.numeric(table_norm2$ADORA1)
table_norm2$ANKRD30A <- as.numeric(table_norm2$ANKRD30A)
table_norm2$APOE <- as.numeric(table_norm2$APOE)

cor.mat <- table_norm2 %>% select(1:5) %>% cor_mat(method="spearman")

cor.mat %>% cor_reorder() %>% pull_lower_triangle() %>% cor_plot()

corr_norm1 <- table_norm2 %>% cor_test(1:5, method="spearman")

corr_norm <- filter(corr1, p<0.05)
