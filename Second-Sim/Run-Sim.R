devtools::install_github("earowang/hts")
library(hts)
library(forecast)
library(Matrix)
library(reshape)
library(tsibble)
library(tidyverse)
library(SparseM)
source('CHOL.R')
source('CG.R')
source('olsfc.pwfc.AIC-BIC2.R')
#source('olsfc.R')
source('smatrix-V2.R')
source('ngts-V2.R')


actual.sim <- readr::read_csv('actual.sim.noise.FH.3Most.n.csv')
## Choosing series with different added noises (0.01, 0.1, 0.5, 1)
actual.sim.001 <- actual.sim[actual.sim$Sim == 'sig0.01',]
actual.sim.01 <- actual.sim[actual.sim$Sim == 'sig0.1',]
actual.sim.05 <- actual.sim[actual.sim$Sim == 'sig0.5',]
actual.sim.1 <- actual.sim[actual.sim$Sim == 'sig1',]

data.network <- cbind.data.frame(actual.sim.05$Var2, actual.sim.05$value)
colnames(data.network) <- c('cat', 'series')

## if needed - in case we have some errors in labels
#tst <- data.network %>%
#  group_split(cat)

#reference_dimensions <- dim(tst[[1]])
#different_dimensions_indices <- integer(0)

#for (i in seq_along(tst)) {
#  if (any(as.vector(dim(tst[[i]]))!= as.vector(reference_dimensions))==TRUE) {
#    different_dimensions_indices <- c(different_dimensions_indices, i)
#  }
#}

#tst2 <- tst[-c(different_dimensions_indices)]

#data.network <- do.call(rbind, tst2)
#######
smatrix.net <- smatrix.v2(data.network = data.network)
ngts.net <- ts(as.matrix(Aggreg.func.v2(data.network)), frequency = 12, start = c(2017, 11))


# Splitting data into training and test sets - 6 months forecasts
#h = 1
#net.train <- window(ngts.net, end = c(2022, 8))
#net.test <- window(ngts.net, start = c(2022, 9), end = c(2022,9))
#h = 2
#net.train <- window(ngts.net, end = c(2022, 8))
#net.test <- window(ngts.net, start = c(2022, 9), end = c(2022,10))
#h = 3
#net.train <- window(ngts.net, end = c(2022, 8))
#net.test <- window(ngts.net, start = c(2022, 9), end = c(2022,11))
#h = 4
#net.train <- window(ngts.net, end = c(2022, 8))
#net.test <- window(ngts.net, start = c(2022, 9), end = c(2022,12))
#h = 5
#net.train <- window(ngts.net, end = c(2022, 8))
#net.test <- window(ngts.net, start = c(2022, 9), end = c(2023,1))
#h = 6
net.train <- window(ngts.net, end = c(2022, 8))
net.test <- window(ngts.net, start = c(2022, 9), end = c(2023,2))

write.csv(net.test, 'net.test.csv')
write.csv(net.train, 'net.train.csv')

## number of forecast points
#h <- 1
#h <- 2
#h <- 3
#h <- 4
#h <- 5
h <- 6


fc.arima <- matrix(NA, nrow = nrow(net.test), ncol = ncol(net.test))
train.fit <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc <- forecast(auto.arima(net.train[,i]), h = h)
  fc.arima[,i] <- fc$mean
  train.fit[,i] <- fc$fitted
}

colnames(fc.arima) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)



write.csv(fc.arima, 'fc.arima.unrec.csv')

## BU
fc.arima.b <- fc.arima[, (ncol(fc.arima) - (ncol(smatrix.net)-1)):ncol(fc.arima)]
fc.arima.b.melt <- reshape2::melt(fc.arima.b)
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
  fc.1 <- olsfc.pwfc2(net.train[,i], h, breakpoints = c(5, 15, 25), maxlag = 12, nolag = c(1))
  fc.ols[,i] <- fc.1[[1]]
  train.fit[,i] <- fc.1[[2]]
}
colnames(fc.ols) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)



write.csv(fc.ols, 'fc.ols.unrec.csv')

## BU
fc.ols.b <- fc.ols[, (ncol(fc.ols) - (ncol(smatrix.net)-1)):ncol(fc.ols)]
fc.ols.b.melt <- reshape2::melt(fc.ols.b)
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


