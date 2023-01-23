

library(data.table)
library(lubridate)
## read all the sample data - add the date column (same as each file name) - combine all the samples from each month as one file
file_list <- list()
file_names <- dir()
file_list <- lapply(file_names, function(x){
  sampleclick <- read_csv(x)[,-1]
  sampleclick$date <- substr(x,1,nchar(x)-4)
  return(sampleclick)})
## combine samples
SampleClick <- rbindlist(file_list)

## fix date of the data

#parse_date_time(SampleClick$date, "ym")

## add up repeated rows
SampleClick <- SampleClick %>%
  group_by(prev, curr, type, id, date) %>%
  summarise(freq = sum(freq))

## fill incomplete monthly series
library(tsibble)
test <- SampleClick %>% 
  group_by(prev, curr, type, id) %>% 
  complete(date = rep(yearmonth(min(date)) + 0:61), fill = list(amount = 0))
