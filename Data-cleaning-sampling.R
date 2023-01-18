
library(tidyverse)
## reding data
library(data.table)
click.2022.12<-as.data.frame(fread("clickstream-enwiki-2022-12.tsv"))
## specify column names
colnames(click.2022.12) <- c('prev', 'curr', 'type', 'freq')

## add id column
click.2022.12 %>%
  mutate('id' = )