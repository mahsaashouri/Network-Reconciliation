
library(tidyverse)

data.network.all <- read_csv('SampleClick.csv')[,-1]
data.network <- data.network.all[,c('id', 'freq')]
colnames(data.network) <- c('cat', 'series')

## summing matrix
smatrix <- function(data.network){
  
  ## series before and after '.' which shows inner and outer series
  char.before <- sub(":.*", "", data.network$cat)
  char.after <- sub(".*:", "", data.network$cat)
  
  ## number of rows in smatrix.network
  number.row <- 1 + 1 + ifelse(length((filter(data.network, char.before =='other'))$cat)!=0, 1, 0) +
    ifelse(sum(unique(char.before) %in% "other"),length(unique(char.before))-1, length(unique(char.before))) + 
    length(unique(char.after)) + length(unique(data.network$cat))
  
  ## emty matrix for smatrix
  smatrix.network <- matrix(NA, ncol = length(unique(data.network$cat)), nrow = number.row)
  # total IN 
  smatrix.network[1,] <- rep(1, ncol(smatrix.network))
  # total OUT
  smatrix.network[2,] <- c(rep(1, ncol(smatrix.network)-length(unique(filter(data.network, char.before == 'other')$cat))), 
                           rep(0, length(unique(filter(data.network, char.before == 'other')$cat))))
  # total Outer series (other)
  if(sum(unique(char.before) %in% "other") == 1){
    h <- 3
    smatrix.network[h,] <- c(rep(0, ncol(smatrix.network)-length(unique(filter(data.network, char.before == 'other')$cat))), 
                             rep(1, length(unique(filter(data.network, char.before == 'other')$cat))))  
  }
  else{
    h <- 0
  }
  
  cat.un <-  unique(data.network$cat)
  # IN series
  no.in.series <- ifelse(sum(unique(char.before) %in% "other"),length(unique(char.before))-1, 
                         length(unique(char.before)))
  for(i in 1:no.in.series){
    s.in <- unique(char.after)
    smatrix.network[h+i,] <- ifelse(sub(".*:", "", cat.un) %in% s.in[i], 1, 0)
  }
  
  # OUT series
  no.out.series <- length(unique(char.after))
  for(i in 1:no.out.series){
    s.out <- unique(char.before)[!unique(char.before) %in% "other"]
    smatrix.network[h+no.in.series+i,] <- ifelse(sub(":.*", "", cat.un) %in% s.out[i], 1, 0)
  }
  # Bottom level series
  smatrix.network[(h+no.in.series+no.out.series+1):number.row,] <- diag(1, length(unique(data.network$cat)))
  
  return(smatrix.network)
}


## aggregation function
Aggreg.func <- function(data.network){
  
  # total IN 
  TotalIn <- data.network %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalIn <- matrix(TotalIn, nrow = length(TotalIn))
  colnames(TotalIn) <- 'Total.in'
  
  # total OUT 
  TotalOut <- data.network %>%
    filter(sub(":.*", "", cat)!='other') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalOut <- matrix(TotalOut, nrow = length(TotalOut))
  colnames(TotalOut) <- 'Total.out'
  
  # total Outer series (other)
  Outer <- data.network %>%
    filter(sub(":.*", "", cat)=='other') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  Outer <- matrix(Outer, nrow = length(Outer))
  colnames(Outer) <- 'Outer'
  
  # IN series
  DataIn <- data.network %>%
    mutate('curr.id'= factor(sub(".*:", "", cat), level = unique(sub(".*:", "", cat)))) %>%
    group_split(curr.id) 
  
  SumIn <- matrix(NA, nrow = length(TotalIn), ncol = length(DataIn)); NameIn <- c()
  
  for(i in 1:length(DataIn)){
    name.in <- unique(as.character(DataIn[[i]]$curr.id))
    NameIn <- c(NameIn, name.in)
  }
  colnames(SumIn) <- paste(NameIn,'in',sep='.')
  
  for (i in 1:length(DataIn)){
    SumIn[,i] <- DataIn[[i]]%>%
      group_split(cat) %>%
      map(~.[['series']]) %>%
      reduce(`+`)
  }
  
  # OUT series
  DataOut <- data.network %>%
    filter(sub(":.*", "", cat)!='other') %>%
    mutate( 'prev.id'= factor(sub(":.*", "", cat), level = unique(sub(":.*", "", cat)))) %>%
    group_split(prev.id) 
  
  SumOut <- matrix(NA, nrow = length(TotalOut), ncol = length(DataOut)); NameOut <- c()
  
  for(i in 1:length(DataOut)){
    name.out <- unique(as.character(DataOut[[i]]$prev.id))
    NameOut <- c(NameOut, name.out)
  }
  colnames(SumOut) <- paste(NameOut,'out',sep='.')
  
  for (i in 1:length(DataOut)){
    SumOut[,i] <- DataOut[[i]]%>%
      group_split(cat) %>%
      map(~.[['series']]) %>%
      reduce(`+`)
  }
  
  # Bottom level series
  BottomLevel <- matrix(data.network$series, nrow = nrow(TotalIn))
  colnames(BottomLevel) <- unique(data.network$cat)
  
  ## Final aggregated matrix
  
  AggregMat <- bind_cols(TotalIn, TotalOut, Outer, SumIn, SumOut, BottomLevel)
  
  return(AggregMat)
}













