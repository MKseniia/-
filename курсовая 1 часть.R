library(dplyr)
library(janitor)
library(readr)

tableNP <- read_table("GSE181029_TPM_NP.tsv")
tableNP <- clean_names(tableNP)

tableDN <- read_table("GSE181029_TPM_DN.tsv")
tableDN <- clean_names(tableDN)

t_gene <- read.csv("parkinsons.csv")

all1 <- inner_join(t_gene, tableNP, by=c('genes'='gene_name'))
all2 <- inner_join(t_gene, tableDN, by=c('genes'='gene_name'))

all2 <- subset(all2, select=-gene_id)
all1 <- subset(all1, select=-gene_id)

data_frame <- inner_join(all1, all2, by='genes')

table_norm <- select(data_frame, starts_with("g"), starts_with("n"))
table_disease <- select(data_frame, starts_with("g"), starts_with("p"))

write.csv(table_norm, "таблица здоровых.csv")
write.csv(table_disease, "таблица больных.csv")