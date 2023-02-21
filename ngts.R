
library(tidyverse)
## Generating sample series
#A.B <- round(runif(10),2)
#A.C <- round(runif(10),2)
#B.A <- round(runif(10),2)
#B.C <- round(runif(10),2)
#C.A <- round(runif(10),2)
#C.B <- round(runif(10),2)
#O.A <- round(runif(10),2)
#O.B <- round(runif(10),2)
#O.C <- round(runif(10),2)

#data.all <- dplyr::bind_rows(list(A.B = A.B, A.C = A.C, B.A = B.A, B.C = B.C, C.A = C.A, C.B = C.B, 
#                                  O.A = O.A, O.B = O.B, O.C = O.C), .id = 'id')
#data.all <- reshape2::melt(data.all)
#colnames(data.all) <- c('cat', 'series')


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
    filter(sub("\\..$", "", cat)!='O') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalOut <- matrix(TotalOut, nrow = length(TotalOut))
  colnames(TotalOut) <- 'Total.out'
  
  # total Outer series (other)
  Outer <- data.network %>%
    filter(sub("\\..$", "", cat)=='O') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  Outer <- matrix(Outer, nrow = length(Outer))
  colnames(Outer) <- 'Outer'
  
  # IN series
  DataIn <- data.network %>%
    mutate('curr.id'= sub("*..", "", cat)) %>%
    group_split(curr.id) 
  
  SumIn <- matrix(NA, nrow = length(TotalIn), ncol = length(DataIn)); NameIn <- c()
  
  for(i in 1:length(DataIn)){
    name.in <- unique(DataIn[[i]]$curr.id)
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
    filter(sub("\\..$", "", cat)!='O') %>%
    mutate( 'prev.id'= sub("\\..$", "", cat)) %>%
    group_split(prev.id) 
  
  SumOut <- matrix(NA, nrow = length(TotalOut), ncol = length(DataOut)); NameOut <- c()
  
  for(i in 1:length(DataOut)){
    name.out <- unique(DataOut[[i]]$prev.id)
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

#Aggreg.func(data.all)









