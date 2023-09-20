
library(tidyverse)
## reading data
library(data.table)

## get 31000 sample from 2022-12

click.2023.02<-as.data.frame(fread("Sample.PERSON.lg.2023.02.csv"))
## specify column names
colnames(click.2023.02) <- c('prev', 'curr', 'type', 'freq')

## make all the outflows from one source
click.2023.02  <- data.frame(lapply(click.2023.02, sub, pattern = "other-.*", replacement = "other"))

## add id column
click.2023.02 <- click.2023.02 %>%
  mutate('id' = paste(prev, curr, sep = '::'))

## sample 1000 rows
#set.seed(12)
#click202302 <- click.2023.02 %>% slice_sample(n = 35000)

#write.csv(click202302 , '2023-02.csv')
## get the same sample from other dataframes
## Use product sample - not randomly sampled series
click202302  <- click.data.network.all.sample#read_csv('2023-02.csv')#[,-1]


## list of datasets to read
DatasetNames <- c("clickstream-enwiki-2017-11.tsv", "clickstream-enwiki-2017-12.tsv", "clickstream-enwiki-2018-01.tsv",
                  "clickstream-enwiki-2018-02.tsv", "clickstream-enwiki-2018-03.tsv", "clickstream-enwiki-2018-04.tsv",
                  "clickstream-enwiki-2018-05.tsv", "clickstream-enwiki-2018-06.tsv", "clickstream-enwiki-2018-07.tsv",
                  "clickstream-enwiki-2018-08.tsv", "clickstream-enwiki-2018-09.tsv", "clickstream-enwiki-2018-10.tsv",
                  "clickstream-enwiki-2018-11.tsv", "clickstream-enwiki-2018-12.tsv", "clickstream-enwiki-2019-01.tsv",
                  "clickstream-enwiki-2019-02.tsv", "clickstream-enwiki-2019-03.tsv", "clickstream-enwiki-2019-04.tsv",
                  "clickstream-enwiki-2019-05.tsv", "clickstream-enwiki-2019-06.tsv", "clickstream-enwiki-2019-07.tsv",
                  "clickstream-enwiki-2019-08.tsv", "clickstream-enwiki-2019-09.tsv", "clickstream-enwiki-2019-10.tsv",
                  "clickstream-enwiki-2019-11.tsv", "clickstream-enwiki-2019-12.tsv", "clickstream-enwiki-2020-01.tsv",
                  "clickstream-enwiki-2020-02.tsv", "clickstream-enwiki-2020-03.tsv", "clickstream-enwiki-2020-04.tsv",
                  "clickstream-enwiki-2020-05.tsv", "clickstream-enwiki-2020-06.tsv", "clickstream-enwiki-2020-07.tsv", 
                  "clickstream-enwiki-2020-08.tsv", "clickstream-enwiki-2020-09.tsv", "clickstream-enwiki-2020-10.tsv",
                  "clickstream-enwiki-2020-11.tsv", "clickstream-enwiki-2020-12.tsv", "clickstream-enwiki-2021-01.tsv",
                  "clickstream-enwiki-2021-02.tsv", "clickstream-enwiki-2021-03.tsv", "clickstream-enwiki-2021-04.tsv", 
                  "clickstream-enwiki-2021-05.tsv", "clickstream-enwiki-2021-06.tsv", "clickstream-enwiki-2021-07.tsv", 
                  "clickstream-enwiki-2021-08.tsv", "clickstream-enwiki-2021-09.tsv", "clickstream-enwiki-2021-10.tsv",
                  "clickstream-enwiki-2021-11.tsv", "clickstream-enwiki-2021-12.tsv", "clickstream-enwiki-2022-01.tsv",
                  "clickstream-enwiki-2022-02.tsv", "clickstream-enwiki-2022-03.tsv", "clickstream-enwiki-2022-04.tsv", 
                  "clickstream-enwiki-2022-05.tsv", "clickstream-enwiki-2022-06.tsv", "clickstream-enwiki-2022-07.tsv", 
                  "clickstream-enwiki-2022-08.tsv", "clickstream-enwiki-2022-09.tsv", "clickstream-enwiki-2022-10.tsv",
                  "clickstream-enwiki-2022-11.tsv", "clickstream-enwiki-2022-12.tsv", "clickstream-enwiki-2023-01.tsv")
## list of samples to wriet
SampleNames <- c('2017-11', '2017-12', '2018-01', '2018-02', '2018-03', '2018-04', '2018-05', 
                 '2018-06', '2018-07', '2018-08', '2018-09', '2018-10', '2018-11', '2018-12',
                 '2019-01', '2019-02', '2019-03', '2019-04', '2019-05', '2019-06', '2019-07', 
                 '2019-08', '2019-09', '2019-10', '2019-11', '2019-12','2020-01', '2020-02', 
                 '2020-03', '2020-04', '2020-05', '2020-06', '2020-07', '2020-08', '2020-09', 
                 '2020-10', '2020-11', '2020-12', '2021-01', '2021-02', '2021-03', '2021-04', 
                 '2021-05', '2021-06', '2021-07', '2021-08', '2021-09', '2021-10', '2021-11', 
                 '2021-12', '2022-01', '2022-02', '2022-03', '2022-04', '2022-05', '2022-06', 
                 '2022-07', '2022-08', '2022-09', '2022-10', '2022-11', '2022-12', '2023-01')

## for loop to creat samples from big dataframes

for(i in 1:length(DatasetNames)){
  ## read dataset
  click.data<-as.data.frame(fread(DatasetNames[i]))
  ## specify column names
  colnames(click.data) <- c('prev', 'curr', 'type', 'freq')
  
  ## make all the outflows from one source
  click.data  <- data.frame(lapply(click.data, sub, pattern = "other-.*", replacement = "other"))
  
  ## add id column
  click.data <- click.data %>%
    mutate('id' = paste(prev, curr, sep = '::'))
  
  ## sample from other files
  click.data.sample <- subset(click.data, click.data$id %in% click202302$id )
  
  ## save results
  write_csv(click.data.sample, path = paste0("",SampleNames[i],".csv"))
  
  ## remove the dataframe (better memory usage)
  rm(click.data)
}



