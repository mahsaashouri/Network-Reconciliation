

library(data.table)
library(lubridate)
library(tidyverse)
library(tsibble)
## read all the sample data - add the date column (same as each file name) - combine all the samples from each month as one file
file_list <- list()
file_names <- dir()
file_list <- lapply(file_names, function(x){
  sampleclick <- read_csv(x)
  sampleclick$date <- substr(x,1,nchar(x)-4)
  return(sampleclick)})
## combine samples
SampleClick <- rbindlist(file_list)



## add up repeated rows
SampleClick <- SampleClick %>%
  group_by(id, date) %>%
  summarise(freq = sum(freq))

## fill incomplete monthly series ## total number of series 843- length of each series: 62

SampleClick <- SampleClick %>% 
  mutate(date = yearmonth((parse_date_time(date, "ym")))) 

date.sample <-  rep(min(SampleClick$date) + 0:61)

SampleClick <- SampleClick %>%
  group_by(id) %>% 
  complete(date = date.sample, fill = list(amount = 0))

# replacing NA with 0
SampleClick <- SampleClick %>%
  (function(x) { x[is.na(x)] <- 0; return(x) })

### removing 80% zero series ## total series to work on is 843 time series
SampleClick <- SampleClick %>%
  group_by(id) %>%
  filter(mean(freq == 0) <= 0.8)


#write.csv(SampleClick, 'SampleClick.csv')
