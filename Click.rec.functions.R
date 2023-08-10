
library(tidyverse)
library(forecast)
library(Matrix)
library(hts)
library(SparseM)
source('smatrix.R')
source('ngts.R')
source('CG.shrink.R')
source('olsfc.pwfc.AIC-BIC.R')


data.network.all <- read_csv('SampleClickProduct.csv')[,-1]
data.network <- data.network.all[,c('id', 'freq')]
colnames(data.network) <- c('cat', 'series')

## Smooth the series
#library(purrr)
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
#ngts.net.melt <- reshape2::melt(ngts.net)
#ngts.net.melt%>%
#  filter(Var2 == "Total.in") %>%
#  ggplot(aes(x = Var1, y = value)) +
#  geom_line() +
#  xlab("Horizon") +
#  ylab("Count") +
#  theme_bw()


## training and test sets
# Splitting data into training and test sets
net.train <- window(ngts.net, end = c(2022, 2))
net.test <- window(ngts.net, start = c(2022, 3))
write.csv(net.train, 'net.train.csv')
write.csv(net.test, 'net.test.csv')
h <- 12

## ARIMA
fc.arima <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train)-1, ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  c <- forecast(auto.arima(net.train[,i], D = 1, d = 1), h = h)
  fc.arima[,i] <- fc$mean
  train.fit[,i] <- fc$fitted 
}
colnames(fc.arima) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)

write.csv(fc.arima, 'fc.arima.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train) - train.fit)
write.csv(res, 'res.train.arima.csv')
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")

fc.CG.null <- CG(fc.arima, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.arima.csv')
fc.CG <- CG(fc.arima, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.arima.csv')



## OLS
fc.ols <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train)-1, ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc.1 <- olsfc.pwfc(net.train[,i], h, breakpoints = c(5, 10, 15, 20), maxlag = 12, nolag = c(1))
  fc.ols[,i] <- fc.1[[1]]
  train.fit[,i] <- fc.1[[2]]
}
colnames(fc.ols) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)

write.csv(fc.ols, 'fc.ols.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train)[-1,] - train.fit)
write.csv(res, 'res.train.ols.csv')
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")

fc.CG.null <- CG(fc.ols, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.ols.csv')
fc.CG <- CG(fc.ols, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.ols.csv')


## NAIVE
fc.naive <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc.1 <- naive(net.train[,i], h)
  fc.naive[,i] <- fc.1$mean
  train.fit[,i] <- fc.1$fitted
}
colnames(fc.naive) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)

write.csv(fc.naive, 'fc.naive.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train)[-1,] - (train.fit)[-1,])
write.csv(res, 'res.train.naive.csv')
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")

fc.CG.null <- CG(fc.naive, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.naive.csv')
fc.CG <- CG(fc.naive, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.naive.csv')


## ETS
fc.ets <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc.1 <- forecast(ets(net.train[,i]), h)
  fc.ets[,i] <- fc.1$mean
  train.fit[,i] <- fc.1$fitted
}
colnames(fc.ets) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)

write.csv(fc.ets, 'fc.ets.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train) - (train.fit)[-1,])
write.csv(res, 'res.train.ets.csv')
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")

fc.CG.null <- CG(fc.ets, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.ets.csv')
fc.CG <- CG(fc.ets, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.ets.csv')


## Plot the results
#merged_arima <- rbind(cbind(reshape2::melt(net.test), data = 'test.set'), cbind(reshape2::melt(fc.arima), data = 'arima'), 
#                     cbind(reshape2::melt(t(fc.CG)), data = 'arima.rec'))
#merged_arima_train <- rbind(cbind(reshape2::melt(net.train[-1,]), data = 'train.set'), cbind(reshape2::melt(train.fit), data = 'arima'))

#merged_arima_train%>%
# filter(Var2 == "Total.in") %>%
# ggplot(aes(x = Var1, y = value, colour = data)) +
# geom_line() +
# xlab("Horizon") +
# ylab("Count") +
# theme_bw()

#merged_arima%>%
#  filter(Var2 == "Total.in") %>%
#  ggplot(aes(x = Var1, y = value, colour = data)) +
#  geom_line() +
#  xlab("Horizon") +
#  ylab("Count") +
#  theme_bw()


# Extra codes
# bind the datasARIMA into a single data frame
#tst1 <- cbind(reshape2::melt(net.test), data = 'actual', count = rep(1:12, 1140))[,-1]
#colnames(inverted_data.test) <- colnames(net.test)
#tst2 <- cbind(reshape2::melt(inverted_data.test), data = 'arima', count = rep(1:12, 1140))
#rec.fore.CHOL <- t(rec.fore.CHOL)
#fc.CG <- t(fc.CG)
#colnames(fc.CG) <- colnames(net.test)
#colnames(rec.fore.CHOL) <- colnames(net.test)
#tst3 <- cbind(reshape2::melt(rec.fore.CHOL), data = 'arima.rec', count = rep(1:12, 1140))[,-1]
#tst4 <- cbind(reshape2::melt(fc.CG), data = 'arima.rec.CG', count = rep(1:12, 1140))[,-1]

#colnames(tst3) <- c("series", "value", "data", "count")
#merged_arima <- rbind(tst1, tst2, tst3, tst4)

#merged_arima%>%
#  filter(series == "Total.out") %>%
# ggplot(aes(x = count, y = value, colour = data)) +
#geom_line() +
#xlab("Horizon") +
#ylab("Count") +
#theme_bw()









