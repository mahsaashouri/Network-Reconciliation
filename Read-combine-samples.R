

library(data.table)
library(lubridate)
library(tidyverse)
library(tsibble)
## read all the sample data - add the date column (same as each file name) - combine all the samples from each month as one file
file_list <- list()
file_names <- dir()
file_list <- lapply(file_names, function(x){
  sampleclick <- read_csv(x)[,-1]
  sampleclick$date <- substr(x,1,nchar(x)-4)
  return(sampleclick)})
## combine samples
SampleClick <- rbindlist(file_list)

## add up repeated rows
SampleClick <- SampleClick %>%
  group_by(prev, curr, type, id, date) %>%
  summarise(freq = sum(freq))

## fill incomplete monthly series ## total number of seriesL 305 - length of each series: 62

SampleClick <- SampleClick %>% 
  mutate(date = yearmonth((parse_date_time(date, "ym")))) %>%
  group_by(prev, curr, type, id) %>% 
  complete(date = rep(min(date) + 0:61), fill = list(amount = 0))

# replacing NA with 0
SampleClick <- SampleClick %>%
  (function(x) { x[is.na(x)] <- 0; return(x) })

#write.csv(SampleClick, 'SampleClick.csv')
