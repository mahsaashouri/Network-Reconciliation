
## simulate correlated AR(1) series 

library(forecast)
library(MASS)



# Set the number of time series and length of each series
n_series <- 400
n_steps <- 120

# Generate the AR(1) coefficients for each series
ar_coef <- matrix(runif(n_series, -1, 1), ncol = n_series)

# Generate the covariance matrix for the innovations
corr <- 0.5 # Set the desired correlation
covar <- matrix(corr, ncol = n_series, nrow = n_series)
diag(covar) <- 1
innov_cov <- chol(covar)

# Generate the innovations for each series
innovations <- mvrnorm(n_steps, rep(0, n_series), innov_cov)

# Generate the time series data
ts_data <- matrix(NA, nrow = n_steps, ncol = n_series)

for (i in 1:n_series) {
  ts_data[, i] <- arima.sim(model = list(ar = ar_coef[, i]), n = n_steps, rand.gen = rnorm, innov = innovations[, i])
}

# Convert the time series data to a ts object with frequency 12
ts_data <- ts(ts_data, start = c(2010, 1), frequency = 12)


## rename the generated series - 20 nodes + 1 outer
names.node <- c()
for(i in 1:20){
  for(j in 1:20){
    if(i!=j){
      nam <- paste(paste0('A', i),  paste0('A', j), sep = '.') 
      names.node <- c(names.node, nam)
    }
  }
}
# Outer series
names.node.outer <- c()
for(i in 1:20){
  nam.o <- paste('O', paste0('A', i), sep = '.')
  names.node.outer <- c(names.node.outer, nam.o)
}

net.name <- c(names.node, names.node.outer)
## rename the series
colnames(ts_data) <- net.name

## generate the aggregated network series and smatrix for the dataset

source('smatrix.R')
source('ngts.R')

data.network <- reshape2::melt(ts_data)
data.network <- data.network[,-1]
colnames(data.network) <- c('cat', 'series')
smatrix.net <- smatrix(data.network = data.network)
ngts.net <- ts(Aggreg.func(data.network), frequency = 12, start = c(2010, 1))


## base forecasts using ARIMA

## training and test sets
# Splitting data into training and test sets
net.train <- window(ngts.net, end = c(2018, 12))
net.test <- window(ngts.net, start = c(2019, 1))
h <- 12 ## number of forecast points
fc <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
for(i in seq(NCOL(net.train)))
{
  fc[,i] <- forecast(auto.arima(net.train[,i]), h = h)$mean
}
colnames(fc) <- colnames(net.test)

## computing reconciliation matrix
lambda <- diag(rowSums(smatrix.net))

rec.adj.lambda <- as.matrix(smatrix.net%*%solve(t(smatrix.net)%*%solve(lambda)%*%smatrix.net)%*%t(smatrix.net)%*%solve(lambda))

fc.rec <- matrix(NA, nrow = 12, ncol = ncol(net.test))
for(i in 1:nrow(fc)){
  f.1 <- matrix(as.numeric(fc[i,]), ncol = 1, nrow = ncol(fc))
  fc.rec [i,] <- rec.adj.lambda %*% f.1
}
colnames(fc.rec ) <- colnames(net.test)



