library(dplyr)
library(janitor)

tableNP <- read.table("GSE181029_TPM_NP.tsv", header=TRUE)
tableNP <- clean_names(tableNP)

tableDN <- read.table("GSE181029_TPM_DN.tsv", header=TRUE)
tableDN <- clean_names(tableDN)

t_gene <- read.csv("parkinsons.csv")

all1 <- inner_join(t_gene, tableNP, by=c('genes'='gene_name'))
all2 <- inner_join(t_gene, tableDN, by=c('genes'='gene_name'))

all2 <- subset(all2, select=-gene_id)

data_frame <- inner_join(all1, all2, by='genes')

table_norm <- select(data_frame, starts_with("g"), starts_with("n"))
table_disease <- select(data_frame, starts_with("g"), starts_with("p"))