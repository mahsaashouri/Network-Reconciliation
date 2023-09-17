
library(tidyverse)
library(forecast)
library(Matrix)
library(hts)
library(SparseM)
source('smatrix-V2.R')
source('ngts-V2.R')
source('CG-shrink.R')
source('olsfc.pwfc.AIC-BIC.R')


data.network.all <- read_csv('SampleClickProduct.csv')[,-1]
data.network <- data.network.all[,c('id', 'freq')]
colnames(data.network) <- c('cat', 'series')

## only choose the connected network

library(igraph)

# Create a graph from your dataset
split_names <- strsplit(unique(data.network$cat), "::")
before_parts <- sapply(split_names, "[[", 1)
after_parts <- sapply(split_names, "[[", 2)

data <- cbind.data.frame('before' = c(before_parts), 'after' = c(after_parts))

graph <- graph_from_data_frame(data, directed = TRUE)

components <- components(graph)
largest_component <- which.max(components$csize)
largest_vertices <- which(components$membership == largest_component)
largest_subgraph <- induced_subgraph(graph, vids = largest_vertices)

largest_subgraph_data <- as.data.frame(get.edgelist(largest_subgraph))
largest_subgraph_data_id <- largest_subgraph_data %>%
  mutate('id' = paste(largest_subgraph_data$V1, largest_subgraph_data$V2, sep = '::'))

## plot the graph
#ColMap = rep("orange", vcount(largest_subgraph))
#ColMap[grep("^SA", V(largest_subgraph)$name)] = "red"
#plot(largest_subgraph,
#     edge.label.cex = 1,
#     edge.arrow.size=.2,
#     vertex.color=ColMap,
#     vertex.size=4, 
#     vertex.frame.color="orange",
#     vertex.label.color="black", 
#     vertex.label.cex=0.8,
#     vertex.label.dist=c(rep(1.2,5), rep(2.2,17)),
#     vertex.label.degree = c(rep(-pi/2, 5), pi, rep(-0.1,8), rep(0.1,8)),
#     margin=-.2,
#     vertex.shape='circle')

click.data.network.all <- subset(data.network, data.network$cat %in% largest_subgraph_data_id$id )
## If we want to choose graph with most frequent nodes
tst3 <- largest_subgraph_data_id$V1[largest_subgraph_data_id$V1 != "other"]
page_counts <- table(c(tst3, largest_subgraph_data_id$V2))
# Find the pages with the highest frequency
max_frequency <- max(page_counts)
most_frequent_pages <- names(head(sort(page_counts, decreasing = TRUE), 1000))


library(dplyr)
df <- as.data.frame(unique(data.network.all$id))
colnames(df)  <- "cat"

filtered_df <- df %>%
  filter(sapply(strsplit(unique(df$cat), "::"), function(x) any(x[1] == most_frequent_pages | x[2] == most_frequent_pages)))

click.data.network.all.sample <- subset(click.data.network.all, click.data.network.all$cat %in% filtered_df$cat)


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

smatrix.net <- smatrix.v2(data.network = click.data.network.all.sample)
ngts.net <- ts(as.matrix(Aggreg.func.v2(click.data.network.all.sample)), frequency = 12, start = c(2017, 11))

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
train.fit <- matrix(NA, nrow = nrow(net.train), ncol = ncol(net.train))
for(i in seq(NCOL(net.train))){
  fc <- forecast(auto.arima(log(net.train[,i]+ 0.001), D = 1, d = 1), h = h)
  fc.arima[,i] <- fc$mean
  train.fit[,i] <- fc$fitted 
}
colnames(fc.arima) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)
#fc.arima[fc.arima <0 ] <- 0

fc.arima <- exp(fc.arima)
train.fit <- exp(train.fit)

write.csv(fc.arima, 'fc.arima.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train) - train.fit)
write.csv(res, 'res.train.arima.csv')

lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.null <- CG(fc.arima, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.null), 'fc.rec.CG.lambda.arima.csv')

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
  fc.1 <- olsfc.pwfc(net.train[,i], h, breakpoints = c(5, 10, 15, 20), maxlag = 12, nolag = c(1))
  fc.ols[,i] <- fc.1[[1]]
  train.fit[,i] <- fc.1[[2]]
}
colnames(fc.ols) <- colnames(net.test)
colnames(train.fit) <- colnames(net.train)
fc.ols[fc.ols < 0] <- 0
write.csv(fc.ols, 'fc.ols.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train)[-1,] - train.fit)
write.csv(res, 'res.train.ols.csv')

lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.lambda <- CG(fc.ols, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.lambda), 'fc.rec.CG.lambda.ols.csv')

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
fc.ets[fc.ets < 0] <- 0

write.csv(fc.ets, 'fc.ets.unrec.csv')
## residuals
res <- as.matrix(as.data.frame(net.train) - (train.fit)[-1,])
write.csv(res, 'res.train.ets.csv')
lambda.1 <- as(diag(rowSums(smatrix.net)), 'dgCMatrix')
fc.CG.lambda <- CG(fc.ets, smatrix.net, weights = lambda.1)
write.csv(t(fc.CG.lambda), 'fc.rec.CG.lambda.ets.csv')

tar <- lowerD(res)
shrink <- shrink.estim(res, tar)
w.1 <- shrink[[1]]
lambda <- shrink[[2]]
weights <-  methods::as(w.1, "sparseMatrix")
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









