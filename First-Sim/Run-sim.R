


#set.seed(123)
#devtools::install_github("earowang/hts")
library(hts)
library(forecast)
library(Matrix)
library(reshape)
library(tsibble)
library(tidyverse)
library(SparseM)
source('CG-shrink.R')
source('olsfc.pwfc.AIC-BIC.R')
source('smatrix-V2.R')
source('ngts-V2.R')


complete_df <- read.csv('complete_df.csv', header = TRUE)[,-1]
data.network <- cbind.data.frame(complete_df$path, complete_df$frequency)
colnames(data.network) <- c('cat', 'series')
smatrix.net <- smatrix.v2(data.network = data.network)
ngts.net <- ts(as.matrix(Aggreg.func.v2(data.network)), frequency = 12, start = c(2012, 01))



# Splitting data into training and test sets - 6 months forecasts
#h = 1
#net.train <- window(ngts.net, end = c(2023, 12))
#net.test <- window(ngts.net, start = c(2024, 01), end = c(2024, 01))
#h = 3
#net.train <- window(ngts.net, end = c(2023, 10))
#net.test <- window(ngts.net, start = c(2023, 11), end = c(2024, 01))
#h = 6
#net.train <- window(ngts.net, end = c(2023, 07))
#net.test <- window(ngts.net, start = c(2023, 08), end = c(2024, 01))
#h = 12
net.train <- window(ngts.net, end = c(2023, 01))
net.test <- window(ngts.net, start = c(2023, 02), end = c(2024, 01))

write.csv(net.test, 'net.test.csv')
write.csv(net.train, 'net.train.csv')

## number of forecast points
#h <- 1
#h <- 3
#h <- 6
h <- 12


fc.arima <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc <- forecast(auto.arima(net.train[,i], nmodels = 200), h = h)
  fc.arima[,i] <- fc$mean
  train.fit[,i] <- fc$fitted
}

colnames(fc.arima) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)



write.csv(fc.arima, 'fc.arima.unrec.csv')

## BU
fc.arima.b <- fc.arima[, (ncol(fc.arima) - (ncol(smatrix.net)-1)):ncol(fc.arima)]
fc.arima.b.melt <- reshape2::melt(fc.arima.b)
## for h=1
#fc.arima.b.melt <- cbind.data.frame(row.names(fc.arima.b.melt), fc.arima.b.melt)
## other h
fc.arima.b.melt <- fc.arima.b.melt[,-1]
colnames(fc.arima.b.melt) <- c('cat', 'series')
fc.BU.arima <- as.matrix(Aggreg.func.v2(fc.arima.b.melt))
write.csv(t(fc.BU.arima), 'fc.rec.BU.arima.csv')

## residuals
res <- as.matrix(as.data.frame(net.train) - train.fit)
write.csv(res, 'res.train.arima.csv')

## structural scaling
lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.lambda <- CG(fc.arima, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.lambda), 'fc.rec.CG.lambda.arima.csv')

## variance scaling
n.res <- nrow(res)
covm <- crossprod(stats::na.omit(res)) / n.res
W <- diag(diag(covm))
var.scale <- methods::as(W, "sparseMatrix")
fc.CG.VAR <- CG(fc.arima, smatrix.net, weights = var.scale)
write.csv(t(fc.CG.VAR), 'fc.rec.CG.var.arima.csv')

## OLS rec
fc.CG.null <- CG(fc.arima, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.arima.csv')

## MinT
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")
fc.CG <- CG(fc.arima, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.arima.csv')

## OLS
fc.ols <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train)-1, ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc.1 <- olsfc.pwfc(net.train[,i], h, breakpoints = c(5, 15, 25), maxlag = 12, nolag = c(1))
  fc.ols[,i] <- fc.1[[1]]
  train.fit[,i] <- fc.1[[2]]
}
colnames(fc.ols) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)



write.csv(fc.ols, 'fc.ols.unrec.csv')

## BU
fc.ols.b <- fc.ols[, (ncol(fc.ols) - (ncol(smatrix.net)-1)):ncol(fc.ols)]
fc.ols.b.melt <- reshape2::melt(fc.ols.b)
## for h=1
#fc.ols.b.melt <- cbind.data.frame(row.names(fc.ols.b.melt), fc.ols.b.melt)
## other h
fc.ols.b.melt <- fc.ols.b.melt[,-1]
colnames(fc.ols.b.melt) <- c('cat', 'series')
fc.BU.ols <- as.matrix(Aggreg.func.v2(fc.ols.b.melt))
write.csv(t(fc.BU.ols), 'fc.rec.BU.ols.csv')

## residuals
res <- as.matrix(as.data.frame(net.train)[-1,] - train.fit)
write.csv(res, 'res.train.ols.csv')

## structural scaling
lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.lambda <- CG(fc.ols, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.lambda), 'fc.rec.CG.lambda.ols.csv')

## variance scaling
n.res <- nrow(res)
covm <- crossprod(stats::na.omit(res)) / n.res
W <- diag(diag(covm))
var.scale <- methods::as(W, "sparseMatrix")
fc.CG.VAR <- CG(fc.ols, smatrix.net, weights = var.scale)
write.csv(t(fc.CG.VAR), 'fc.rec.CG.var.ols.csv')

## OLS rec
fc.CG.null <- CG(fc.ols, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.ols.csv')

## MinT
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")
fc.CG <- CG(fc.ols, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.ols.csv')



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

## BU
fc.ets.b <- fc.ets[, (ncol(fc.ets) - (ncol(smatrix.net)-1)):ncol(fc.ets)]
fc.ets.b.melt <- reshape2::melt(fc.ets.b)
## for h=1
#fc.ets.b.melt <- cbind.data.frame(row.names(fc.ets.b.melt), fc.ets.b.melt)
## other h
fc.ets.b.melt <- fc.ets.b.melt[,-1]
colnames(fc.ets.b.melt) <- c('cat', 'series')
fc.BU.ets <- as.matrix(Aggreg.func.v2(fc.ets.b.melt))
write.csv(t(fc.BU.ets), 'fc.rec.BU.ets.csv')

## residuals
res <- as.matrix(as.data.frame(net.train) - (train.fit)[-1,])
write.csv(res, 'res.train.ets.csv')

## structural scaling
lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.lambda <- CG(fc.ets, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.lambda), 'fc.rec.CG.lambda.ets.csv')

## variance scaling
n.res <- nrow(res)
covm <- crossprod(stats::na.omit(res)) / n.res
W <- diag(diag(covm))
var.scale <- methods::as(W, "sparseMatrix")
fc.CG.VAR <- CG(fc.ets, smatrix.net, weights = var.scale)
write.csv(t(fc.CG.VAR), 'fc.rec.CG.var.ets.csv')

## OLS rec
fc.CG.null <- CG(fc.ets, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.ets.csv')

## MinT
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")
fc.CG <- CG(fc.ets, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.ets.csv')


## STL
fc.stl <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc.1 <- forecast(stl(net.train[,i], s.window = 'periodic'), h = h, method = 'arima')
  fc.stl[,i] <- fc.1$mean
  train.fit[,i] <- fc.1$fitted
}
colnames(fc.stl) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)


write.csv(fc.stl, 'fc.stl.unrec.csv')

## BU
fc.stl.b <- fc.stl[, (ncol(fc.stl) - (ncol(smatrix.net)-1)):ncol(fc.stl)]
fc.stl.b.melt <- reshape2::melt(fc.stl.b)
## for h=1
#fc.stl.b.melt <- cbind.data.frame(row.names(fc.stl.b.melt), fc.stl.b.melt)
## other h
fc.stl.b.melt <- fc.stl.b.melt[,-1]
colnames(fc.stl.b.melt) <- c('cat', 'series')
fc.BU.stl <- as.matrix(Aggreg.func.v2(fc.stl.b.melt))
write.csv(t(fc.BU.stl), 'fc.rec.BU.stl.csv')

## residuals
res <- as.matrix(as.data.frame(net.train) - (train.fit)[-1,])
write.csv(res, 'res.train.stl.csv')

## structural scaling
lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.lambda <- CG(fc.stl, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.lambda), 'fc.rec.CG.lambda.stl.csv')

## variance scaling
n.res <- nrow(res)
covm <- crossprod(stats::na.omit(res)) / n.res
W <- diag(diag(covm))
var.scale <- methods::as(W, "sparseMatrix")
fc.CG.VAR <- CG(fc.stl, smatrix.net, weights = var.scale)
write.csv(t(fc.CG.VAR), 'fc.rec.CG.var.stl.csv')

## OLS rec
fc.CG.null <- CG(fc.stl, smatrix.net, weights = NULL)
write.csv(t(fc.CG.null), 'fc.rec.CG.null.stl.csv')

## MinT
tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")
fc.CG <- CG(fc.stl, smatrix.net, weights = weights)
write.csv(t(fc.CG), 'fc.rec.CG.shrink.stl.csv')


