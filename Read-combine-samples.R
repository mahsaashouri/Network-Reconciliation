

library(data.table)
library(lubridate)
library(tidyverse)
library(tsibble)
## read all the sample data - add the date column (same as each file name) - combine all the samples from each month as one file
file_list <- list()
#file_names <- dir()
file_names <- list.files(pattern="*.csv")
file_list <- lapply(file_names, function(x){
  sampleclick <- read_csv(x)
  sampleclick$date <- substr(x,1,nchar(x)-4)
  return(sampleclick)})
## combine samples
SampleClick <- rbindlist(file_list)


#SampleClick <- rbind(SampleClick, click202302)
## add up repeated rows
SampleClick <- SampleClick %>%
  group_by(id, date) %>%
  summarise(freq = sum(as.numeric(freq)))

## fill incomplete monthly series ## total number of series 843- length of each series: 62

SampleClick <- SampleClick %>% 
  mutate(date = yearmonth((parse_date_time(date, "ym")))) 

#date.sample <-  rep(min(SampleClick$date) + 0:61)
#SampleClick <- SampleClick %>%
#  group_by(id) %>% 
#  complete(date = date.sample, fill = list(amount = 0))
mindate <- min(SampleClick$date)
maxdate <- max(SampleClick$date)
SampleClick <- SampleClick %>%
  group_by(id) %>% 
  complete(date = seq(mindate, maxdate, by = 1), fill = list(amount = 0))


# replacing NA with 0
SampleClick <- SampleClick %>%
  (function(x) { x[is.na(x)] <- 0; return(x) })

### removing 80% zero series ## total series to work on is 843 time series
SampleClick1 <- SampleClick %>%
  group_by(id) %>%
  filter(mean(freq == 0) <= 0.7)


write.csv(SampleClick1, 'SampleLOC.csv')


## if we need to choose the most frequent articles

# Load necessary libraries
library(dplyr)
library(tidyr)

# Extract P1 and P2 
SampleClick1 <- SampleClick1 %>%
  mutate(P1_P2 = id) %>%
  separate(P1_P2, into = c("P1", "P2"), sep = "::") 

# Aggregate frequencies by P1 and P2 separately, then combine results
top_subjects <- SampleClick1 %>%
  group_by(subject = P1) %>%
  summarise(total_freq = sum(freq)) %>%
  bind_rows(SampleClick1 %>%
              group_by(subject = P2) %>%
              summarise(total_freq = sum(freq))) %>%
  group_by(subject) %>%
  summarise(total_freq = sum(total_freq)) %>%
  arrange(desc(total_freq)) %>%
  head(7)

print(top_subjects)
top_subjects <- top_subjects[-2, ]
# Filter dataset based on top repeated pages
subset_dataset <- SampleClick1 %>%
  filter(P1 %in% top_subjects$subject | P2 %in% top_subjects$subject)

SampleMost <- subset_dataset[, c(-4,-5)]
write.csv(SampleMost, 'SampleLOC5Most.csv')

