
library(tidyverse)
library(forecast)
library(Matrix)

data.network.all <- read_csv('SampleClick.csv')[,-1]
data.network <- data.network.all[,c('id', 'freq')]
colnames(data.network) <- c('cat', 'series')

## Smooth the series
library(purrr)
# split data by category and convert to time series
ts_list <- split(data.network, data.network$cat) %>%
  map(~ ts(.x$series, frequency = 12)) %>%
  map(~ round(tsclean(.x), 0))

# combine time series into a single dataframe
data.network <- ts_list %>%
  imap(~ tibble(cat = .y, series = .x)) %>%
  bind_rows()

## summing matrix
smatrix <- function(data.network){
  
  ## series before and after '.' which shows inner and outer series
  char.before <- sub(":.*", "", data.network$cat)
  char.after <- sub(".*:", "", data.network$cat)
  
  ## number of rows in smatrix.network
  number.row <- 1 + 1 + ifelse(length((filter(data.network, char.before =='other'))$cat)!=0, 1, 0) +
    ifelse(sum(unique(char.before) %in% "other"),length(unique(char.before))-1, length(unique(char.before))) + 
    length(unique(char.after)) + length(unique(data.network$cat))
  
  ## empty matrix for smatrix
  smatrix.network <- Matrix(0, ncol = length(unique(data.network$cat)), nrow = number.row, sparse = TRUE)  
  # total IN 
  smatrix.network[1,] <- 1
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
  no.in.series <- length(unique(char.after))
  for(i in 1:no.in.series){
    s.in <- unique(char.after)
    smatrix.network[h+i,] <- ifelse(sub(".*:", "", cat.un) %in% s.in[i], 1, 0)
  }
  
  # OUT series
  no.out.series <- ifelse(sum(unique(char.before) %in% "other"),length(unique(char.before))-1, 
                         length(unique(char.before)))
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
  TotalIn <- Matrix(TotalIn, nrow = length(TotalIn), sparse = TRUE)
  colnames(TotalIn) <- 'Total.in'
  
  # total OUT 
  TotalOut <- data.network %>%
    filter(sub(":.*", "", cat)!='other') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalOut <- Matrix(TotalOut, nrow = length(TotalOut), sparse = TRUE)
  colnames(TotalOut) <- 'Total.out'
  
  # total Outer series (other)
  Outer <- data.network %>%
    filter(sub(":.*", "", cat)=='other') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  Outer <- Matrix(Outer, nrow = length(Outer), sparse = TRUE)
  colnames(Outer) <- 'Outer'
  
  # IN series
  DataIn <- data.network %>%
    mutate('curr.id'= factor(sub(".*:", "", cat), level = unique(sub(".*:", "", cat)))) %>%
    group_split(curr.id) 
  
  SumIn <- Matrix(0, nrow = length(TotalIn), ncol = length(DataIn), sparse = TRUE); NameIn <- c()
  
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
  
  SumOut <- Matrix(0, nrow = length(TotalOut), ncol = length(DataOut), sparse = TRUE); NameOut <- c()
  
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
  BottomLevel <- Matrix(as.numeric(data.network$series), nrow = nrow(TotalIn), sparse = TRUE)
  colnames(BottomLevel) <- unique(data.network$cat)
  
  ## Final aggregated matrix
  
  AggregMat <- cbind(TotalIn, TotalOut, Outer, SumIn, SumOut, BottomLevel)
  
  return(AggregMat)
}



smatrix.net <- smatrix(data.network = data.network)
ngts.net <- ts(as.matrix(Aggreg.func(data.network)), frequency = 12, start = c(2017, 11))


## base forecasts using ARIMA

## training and test sets
# Splitting data into training and test sets
net.train <- window(ngts.net, end = c(2022, 6))
net.test <- window(ngts.net, start = c(2022, 7))
h <- 6 ## number of forecast points
fc.arima <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.error <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc <- forecast(auto.arima(net.train[,i]), h = h)
  fc.arima[,i] <- fc$mean
  train.error[,i] <- fc$fitted - net.train[,i]
}
colnames(fc.arima) <- colnames(net.test)
colnames(train.error) <- colnames(net.train)

## computing reconciliation matrix - simplest type
lambda <- diag(rowSums(smatrix.net))

rec.adj.lambda <- as.matrix(smatrix.net%*%solve(t(smatrix.net)%*%solve(lambda)%*%smatrix.net)%*%t(smatrix.net)%*%solve(lambda))

fc.arima.rec <- matrix(NA, nrow = h, ncol = ncol(net.test))
for(i in 1:nrow(fc.arima)){
  f.1 <- matrix(as.numeric(fc.arima[i,]), ncol = 1, nrow = ncol(fc.arima))
  fc.arima.rec [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.arima.rec ) <- colnames(net.test)

## computing reconciliation matrix - mint_shrink 
n <- nrow(train.error)
covm <- crossprod(stats::na.omit(train.error)) / n
tar <- diag(apply(train.error, 2, compose(crossprod, stats::na.omit))/n)
corm <- cov2cor(covm)
xs <- scale(train.error, center = FALSE, scale = sqrt(diag(covm)))
xs <- xs[stats::complete.cases(xs),]
v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
diag(v) <- 0
corapn <- cov2cor(tar)
d <- (corm - corapn)^2
lambda <- sum(v)/sum(d)
lambda <- max(min(lambda, 1), 0)
W <- lambda * tar + (1 - lambda) * covm
R <- t(smatrix.net)%*%solve(W)
P <- Matrix::solve(R%*%smatrix.net)%*%R
SP <- as.matrix(smatrix.net%*%P)

## multiply the based forecasts by SP matrix

fc.mint.shrink.arima <- matrix(NA, nrow = h, ncol = ncol(net.test))

for(i in 1:nrow(fc.arima)){
  f.1 <- matrix(as.numeric(fc.arima[i,]), ncol = 1, nrow = ncol(fc.arima))
  fc.mint.shrink.arima [i,] <- SP %*% f.1
}
colnames(fc.mint.shrink.arima) <- colnames(net.test)


# bind the datasARIMA into a single data frame
merged_arima <- rbind(cbind(reshape2::melt(net.test), data = 'test.set'), cbind(reshape2::melt(fc.arima), data = 'arima'),
                    cbind(reshape2::melt(fc.arima.rec), data = 'arima.rec'), 
                    cbind(reshape2::melt(fc.mint.shrink.arima), data = 'mint.shrink.arima'))

merged_arima%>%
  filter(Var2 == "Total.in") %>%
  ggplot(aes(x = Var1, y = value, colour = data)) +
  geom_line() +
  xlab("Horizon") +
  ylab("Count") +
  theme_bw()


## base forecasts using ETS

## training and test sets
# Splitting data into training and test sets
net.train <- window(ngts.net, end = c(2022, 6))
net.test <- window(ngts.net, start = c(2022, 7))
h <- 6 ## number of forecast points
fc.ETS <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.error <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc <- forecast(ets(net.train[,i]), h = h)
  fc.ETS[,i] <- fc$mean
  train.error[,i] <- fc$fitted - net.train[,i]
}
colnames(fc.ETS) <- colnames(net.test)
colnames(train.error) <- colnames(net.train)

## computing reconciliation matrix - simplest type
lambda <- diag(rowSums(smatrix.net))

rec.adj.lambda <- as.matrix(smatrix.net%*%solve(t(smatrix.net)%*%solve(lambda)%*%smatrix.net)%*%t(smatrix.net)%*%solve(lambda))

fc.ETS.rec <- matrix(NA, nrow = h, ncol = ncol(net.test))
for(i in 1:nrow(fc.ETS)){
  f.1 <- matrix(as.numeric(fc.ETS[i,]), ncol = 1, nrow = ncol(fc.ETS))
  fc.ETS.rec [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.ETS.rec ) <- colnames(net.test)

## computing reconciliation matrix - mint_shrink 
n <- nrow(train.error)
covm <- crossprod(stats::na.omit(train.error)) / n
tar <- diag(apply(train.error, 2, compose(crossprod, stats::na.omit))/n)
corm <- cov2cor(covm)
xs <- scale(train.error, center = FALSE, scale = sqrt(diag(covm)))
xs <- xs[stats::complete.cases(xs),]
v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
diag(v) <- 0
corapn <- cov2cor(tar)
d <- (corm - corapn)^2
lambda <- sum(v)/sum(d)
lambda <- max(min(lambda, 1), 0)
W <- lambda * tar + (1 - lambda) * covm
R <- t(smatrix.net)%*%solve(W)
P <- Matrix::solve(R%*%smatrix.net)%*%R
SP <- as.matrix(smatrix.net%*%P)

## multiply the based forecasts by SP matrix

fc.mint.shrink.ETS <- matrix(NA, nrow = h, ncol = ncol(net.test))

for(i in 1:nrow(fc.ETS)){
  f.1 <- matrix(as.numeric(fc.ETS[i,]), ncol = 1, nrow = ncol(fc.ETS))
  fc.mint.shrink.ETS [i,] <- SP %*% f.1
}
colnames(fc.mint.shrink.ETS) <- colnames(net.test)


# bind the datasets into a single data frame
merged_ETS <- rbind(cbind(reshape2::melt(net.test), data = 'test.set'), cbind(reshape2::melt(fc.ETS), data = 'ETS'),
                    cbind(reshape2::melt(fc.ETS.rec), data = 'ETS.rec'), 
                    cbind(reshape2::melt(fc.mint.shrink.ETS), data = 'mint.shrink.ETS'))

merged_ETS%>%
 filter(Var2 == "Innale.out") %>%
  ggplot(aes(x = Var1, y = value, colour = data)) +
  geom_line() +
  xlab("Horizon") +
  ylab("Count") +
  theme_bw()















