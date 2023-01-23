
## reading all the sample data - add the date column (same as each file name) - combine all the samples from each month as one sample
library(data.table)
file_list <- list()
file_names <- dir()
file_list <- lapply(file_names, function(x){
  sampleclick <- read_csv(x)[,-1]
  sampleclick$date <- substr(x,1,nchar(x)-4)
  return(sampleclick)})
SampleClick <- rbindlist(file_list)
