

## input should be a dataframe with two columns 1- series 2- cat which shows the name of the series 
## structure of the names: 'first series.second series' - 'O' shows the 'Outer' series 

smatrix <- function(data.network){
  
  ## series before and after '.' which shows inner and outer series
  char.before <- sub("\\..*$", "", data.network$cat)
  char.after <- sub(".*\\.", "", data.network$cat)
  
  ## number of rows in smatrix.network
  number.row <- 1 + 1 + ifelse(length((filter(data.network, char.before =='O'))$cat)!=0, 1, 0) +
    ifelse(sum(unique(char.before) %in% "O"),length(unique(char.before))-1, length(unique(char.before))) + 
    length(unique(char.after)) + length(unique(data.network$cat))
  
  ## emty matrix for smatrix
  smatrix.network <- Matrix(0, ncol = length(unique(data.network$cat)), nrow = number.row, sparse = TRUE)  
  
  # total IN 
  smatrix.network[1,] <- 1
  # total OUT
  smatrix.network[2,] <- c(rep(1, ncol(smatrix.network)-length(unique(filter(data.network, char.before == 'O')$cat))), 
                           rep(0, length(unique(filter(data.network, char.before == 'O')$cat))))
  # total Outer series (other)
  if(sum(unique(char.before) %in% "O") == 1){
    h <- 3
    smatrix.network[h,] <- c(rep(0, ncol(smatrix.network)-length(unique(filter(data.network, char.before == 'O')$cat))), 
                             rep(1, length(unique(filter(data.network, char.before == 'O')$cat))))  
  }
  else{
    h <- 0
  }
  
  cat.un <-  unique(data.network$cat)
  # IN series
  no.in.series <- length(unique(char.after))
  for(i in 1:no.in.series){
    s.in <- unique(char.after)
    smatrix.network[h+i,] <- ifelse(sub(".*\\.", "", cat.un) %in% s.in[i], 1, 0)
  }
  
  # OUT series
  no.out.series <- ifelse(sum(unique(char.before) %in% "O"),length(unique(char.before))-1, 
                         length(unique(char.before)))
  for(i in 1:no.out.series){
    s.out <- unique(char.before)[!unique(char.before) %in% "O"]
    smatrix.network[h+no.in.series+i,] <- ifelse(sub("\\..*$", "", cat.un) %in% s.out[i], 1, 0)
  }
  # Bottom level series
  smatrix.network[(h+no.in.series+no.out.series+1):number.row,] <- diag(1, length(unique(data.network$cat)))
  
  return(smatrix.network)
}

## run the function
#smatrix.net <- smatrix(data.network = data.all)

