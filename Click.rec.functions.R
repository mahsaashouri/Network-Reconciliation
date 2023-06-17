
library(tidyverse)
library(forecast)
library(Matrix)
source('smatrix.R')
source('ngts.R')
source('Scale_data.R')
source('Invert_scale.R')

data.network.all <- read_csv('SampleClickProduct.csv')[,-1]
data.network <- data.network.all[,c('id', 'freq')]
colnames(data.network) <- c('cat', 'series')

## Smooth the series
library(purrr)
# split data by category and convert to time series
#ts_list <- split(data.network, data.network$cat) %>%
#  map(~ ts(.x$series, frequency = 12)) %>%
#  map(~ round(tsclean(.x), 0))

# combine time series into a single dataframe
#data.network <- ts_list %>%
#  imap(~ tibble(cat = .y, series = .x)) %>%
#  bind_rows()

smatrix.net <- smatrix(data.network = data.network)
ngts.net <- ts(as.matrix(Aggreg.func(data.network)), frequency = 12, start = c(2017, 11))

## plot the series 
ngts.net.melt <- reshape2::melt(ngts.net)
ngts.net.melt%>%
  filter(Var2 == "Total.in") %>%
  ggplot(aes(x = Var1, y = value)) +
  geom_line() +
  xlab("Horizon") +
  ylab("Count") +
  theme_bw()

#plot(as.matrix(ngts.net)[,1], type = 'l')
## base forecasts using ARIMA

## training and test sets
# Splitting data into training and test sets
net.train <- window(ngts.net, end = c(2022, 2))
net.test <- window(ngts.net, start = c(2022, 3))
h <- 12 ## number of forecast points

# Apply scaling to net.gts dataset
Scaled <- scale_data(net.train, net.test, c(-1, 1))
train.all  <- Scaled$scaled_train
test.all <- Scaled$scaled_test
# Access the scaled data
y_train <- Scaled$scaled_train
y_test <- Scaled$scaled_test


# Apply inverse scaling to the scaled data - just to check
#inverted_data <- invert_scaling(Scaled$scaled_train, Scaled$scaler, c(-1, 1))

fc.arima <- matrix(NA, nrow = nrow(y_test), ncol = ncol(y_test))
train.fit <- matrix(NA, nrow = nrow(y_train), ncol = ncol(y_train))
for(i in seq(NCOL(y_train))){
  fc <- forecast(auto.arima(train.all[,i], D = 1, d = 1), h = h)
  fc.arima[,i] <- fc$mean
  train.fit[,i] <- fc$fitted #- y_train[,i]
}
colnames(fc.arima) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)
#inverted_data.test <- invert_scaling(fc.arima, Scaled$scaler, c(-1, 1))

## computing reconciliation matrix - simplest type
lambda_vector <- Matrix::rowSums(smatrix.net)
lambda <- sparseMatrix(i = 1:length(lambda_vector),
             j = 1:length(lambda_vector),
             x = lambda_vector,
             dims = c(length(lambda_vector), length(lambda_vector)))
Inv_lambda <- solve(lambda)
#test <- t(smatrix.net)%*%Inv_lambda
#test2 <- test%*%smatrix.net
#test3 <- solve(as.matrix(test2))
Inv_smatrix.net <- solve(t(smatrix.net)%*%Inv_lambda%*%smatrix.net)
rec.adj.lambda <- (smatrix.net%*%Inv_smatrix.net%*%t(smatrix.net)%*%Inv_lambda)

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
  filter(Var2 == "Mleccha.in") %>%
  ggplot(aes(x = Var1, y = value, colour = data)) +
  geom_line() +
  xlab("Horizon") +
  ylab("Count") +
  theme_bw()


  
## base forecasts using ETS

## training and test sets
# Splitting data into training and test sets
net.train <- window(ngts.net, end = c(2021, 12))
net.test <- window(ngts.net, start = c(2022, 1))
h <- 12 ## number of forecast points
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
 filter(Var2 == "Total.in") %>%
  ggplot(aes(x = Var1, y = value, colour = data)) +
  geom_line() +
  xlab("Horizon") +
  ylab("Count") +
  theme_bw()















