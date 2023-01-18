
library(tidyverse)
## reading data
library(data.table)
click.2022.12<-as.data.frame(fread("clickstream-enwiki-2022-12.tsv"))
## specify column names
colnames(click.2022.12) <- c('prev', 'curr', 'type', 'freq')

## make all the outflows from one source
click.2022.12  <- data.frame(lapply(click.2022.12, sub, pattern = "other-.*", replacement = "other"))

## add id column
click.2022.12 <- click.2022.12 %>%
  mutate('id' = paste(prev, curr, sep = ':'))

## sample 300 rows
set.seed(123)
click.2022.12.sample <- click.2022.12 %>% slice_sample(n = 300)


