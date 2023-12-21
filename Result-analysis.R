

library(dplyr)
library(purrr)
library(ggplot2)
library(readr)
## actual
net.test <- read_csv('net.test.csv')[,-1]
net.train <- read_csv('net.train.csv')[,-1]

## res train
res.train.arima <- read_csv('res.train.arima.csv')[,-1]
res.train.ets <- read_csv('res.train.ets.csv')[,-1]
res.train.ols <- read_csv('res.train.ols.csv')[,-1]
res.train.stl <- read_csv('res.train.stl.csv')[,-1]

## base fc
arima.unrec <- read_csv('fc.arima.unrec.csv')[,-1]
ets.unrec <- read_csv('fc.ets.unrec.csv')[,-1] 
ols.unrec <- read_csv('fc.ols.unrec.csv')[,-1] 
stl.unrec <- read_csv('fc.stl.unrec.csv')[,-1] 

## base res
res.arima.unrec <- net.test - arima.unrec
res.ets.unrec <- net.test - ets.unrec
res.ols.unrec <- net.test - ols.unrec
res.stl.unrec <- net.test - stl.unrec

## base train fc
fc.train.arima.unrec <- res.train.arima[-1,] + res.train.arima[-1,]
fc.train.ets.unrec <- res.train.ets[-1,] + res.train.ets[-1,]
fc.train.ols.unrec <- res.train.ols + res.train.ols
fc.train.stl.unrec <- res.train.stl[-1,] + res.train.stl[-1,]

## rec fc - BU
BU.arima <- read_csv('fc.rec.BU.arima.csv')[,-1]
BU.ets <- read_csv('fc.rec.BU.ets.csv')[,-1]
BU.ols <- read_csv('fc.rec.BU.ols.csv')[,-1]
BU.stl <- read_csv('fc.rec.BU.stl.csv')[,-1]

## rec fc - BU - res
res.BU.arima <- net.test - BU.arima
res.BU.ets <- net.test - BU.ets
res.BU.ols <- net.test - BU.ols
res.BU.stl <- net.test - BU.stl

## rec fc - lambda weight
CG.lambda.arima <- read_csv('fc.rec.CG.lambda.arima.csv')[,-1]
CG.lambda.ets <- read_csv('fc.rec.CG.lambda.ets.csv')[,-1]
CG.lambda.ols <- read_csv('fc.rec.CG.lambda.ols.csv')[,-1]
CG.lambda.stl <- read_csv('fc.rec.CG.lambda.stl.csv')[,-1]

## rec fc - lambda weight - res
res.CG.lambda.arima <- net.test - CG.lambda.arima
res.CG.lambda.ets <- net.test - CG.lambda.ets
res.CG.lambda.ols <- net.test - CG.lambda.ols
res.CG.lambda.stl <- net.test - CG.lambda.stl

## rec fc - var weight
CG.var.arima <- read_csv('fc.rec.CG.var.arima.csv')[,-1]
CG.var.ets <- read_csv('fc.rec.CG.var.ets.csv')[,-1]
CG.var.ols <- read_csv('fc.rec.CG.var.ols.csv')[,-1]
CG.var.stl <- read_csv('fc.rec.CG.var.stl.csv')[,-1]

## rec fc - var weight - res
res.CG.var.arima <- net.test - CG.var.arima
res.CG.var.ets <- net.test - CG.var.ets
res.CG.var.ols <- net.test - CG.var.ols
res.CG.var.stl <- net.test - CG.var.stl

## rec fc - shrink weight
rec.CG.shrink.arima <- read_csv('fc.rec.CG.shrink.arima.csv')[,-1]
rec.CG.shrink.ets <- read_csv('fc.rec.CG.shrink.ets.csv')[,-1]
rec.CG.shrink.ols <- read_csv('fc.rec.CG.shrink.ols.csv')[,-1]
rec.CG.shrink.stl <- read_csv('fc.rec.CG.shrink.stl.csv')[,-1]

## rec fc - shrink weight - res
res.rec.CG.shrink.arima <- net.test - rec.CG.shrink.arima
res.rec.CG.shrink.ets <- net.test - rec.CG.shrink.ets
res.rec.CG.shrink.ols <- net.test - rec.CG.shrink.ols
res.rec.CG.shrink.stl <- net.test - rec.CG.shrink.stl

## rec fc - null weight
rec.CG.null.arima <- read_csv('fc.rec.CG.null.arima.csv')[,-1]
rec.CG.null.ets <- read_csv('fc.rec.CG.null.ets.csv')[,-1]
rec.CG.null.ols <- read_csv('fc.rec.CG.null.ols.csv')[,-1]
rec.CG.null.stl <- read_csv('fc.rec.CG.null.stl.csv')[,-1]


res.rec.CG.null.arima <- net.test - rec.CG.null.arima
res.rec.CG.null.ets <- net.test - rec.CG.null.ets
res.rec.CG.null.ols <- net.test - rec.CG.null.ols
res.rec.CG.null.stl <- net.test - rec.CG.null.stl

arima.unrec.melt <- reshape2::melt(arima.unrec)
ets.unrec.melt <- reshape2::melt(ets.unrec)
ols.unrec.melt <- reshape2::melt(ols.unrec)
stl.unrec.melt <- reshape2::melt(stl.unrec)

BU.arima.melt <- reshape2::melt(BU.arima)
BU.ets.melt <- reshape2::melt(BU.ets)
BU.ols.melt <- reshape2::melt(BU.ols)
BU.stl.melt <- reshape2::melt(BU.stl)

CG.lambda.arima.melt <- reshape2::melt(CG.lambda.arima)
CG.lambda.ets.melt <- reshape2::melt(CG.lambda.ets)
CG.lambda.ols.melt <- reshape2::melt(CG.lambda.ols)
CG.lambda.stl.melt <- reshape2::melt(CG.lambda.stl)

CG.var.arima.melt <- reshape2::melt(CG.var.arima)
CG.var.ets.melt <- reshape2::melt(CG.var.ets)
CG.var.ols.melt <- reshape2::melt(CG.var.ols)
CG.var.stl.melt <- reshape2::melt(CG.var.stl)

rec.CG.shrink.arima.melt <- reshape2::melt(rec.CG.shrink.arima)
rec.CG.shrink.ets.melt <- reshape2::melt(rec.CG.shrink.ets)
rec.CG.shrink.ols.melt <- reshape2::melt(rec.CG.shrink.ols)
rec.CG.shrink.stl.melt <- reshape2::melt(rec.CG.shrink.stl)

rec.CG.null.arima.melt <- reshape2::melt(rec.CG.null.arima)
rec.CG.null.ets.melt <- reshape2::melt(rec.CG.null.ets)
rec.CG.null.ols.melt <- reshape2::melt(rec.CG.null.ols)
rec.CG.null.stl.melt <- reshape2::melt(rec.CG.null.stl)

res.arima.unrec.melt <- reshape2::melt(res.arima.unrec)
res.ets.unrec.melt <- reshape2::melt(res.ets.unrec)
res.ols.unrec.melt <- reshape2::melt(res.ols.unrec)
res.stl.unrec.melt <- reshape2::melt(res.stl.unrec)

res.BU.arima.melt <- reshape2::melt(res.BU.arima)
res.BU.ets.melt <- reshape2::melt(res.BU.ets)
res.BU.ols.melt <- reshape2::melt(res.BU.ols)
res.BU.stl.melt <- reshape2::melt(res.BU.stl)

res.CG.lambda.arima.melt <- reshape2::melt(res.CG.lambda.arima)
res.CG.lambda.ets.melt <- reshape2::melt(res.CG.lambda.ets)
res.CG.lambda.ols.melt <- reshape2::melt(res.CG.lambda.ols)
res.CG.lambda.stl.melt <- reshape2::melt(res.CG.lambda.stl)

res.CG.var.arima.melt <- reshape2::melt(res.CG.var.arima)
res.CG.var.ets.melt <- reshape2::melt(res.CG.var.ets)
res.CG.var.ols.melt <- reshape2::melt(res.CG.var.ols)
res.CG.var.stl.melt <- reshape2::melt(res.CG.var.stl)

res.rec.CG.shrink.arima.melt <- reshape2::melt(res.rec.CG.shrink.arima)
res.rec.CG.shrink.ets.melt <- reshape2::melt(res.rec.CG.shrink.ets)
res.rec.CG.shrink.ols.melt <- reshape2::melt(res.rec.CG.shrink.ols)
res.rec.CG.shrink.stl.melt <- reshape2::melt(res.rec.CG.shrink.stl)

res.rec.CG.null.arima.melt <- reshape2::melt(res.rec.CG.null.arima)
res.rec.CG.null.ets.melt <- reshape2::melt(res.rec.CG.null.ets)
res.rec.CG.null.ols.melt <- reshape2::melt(res.rec.CG.null.ols)
res.rec.CG.null.stl.melt <- reshape2::melt(res.rec.CG.null.stl)

net.test.melt <- reshape2::melt(net.test)

fc.all <- bind_rows(bind_cols('fc' = arima.unrec.melt$value, 'error' = res.arima.unrec.melt$value,  
                'Method' = rep('arima', nrow(arima.unrec.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'unrec'), 
                bind_cols('fc' = ets.unrec.melt$value, 'error' = res.ets.unrec.melt$value, 
                'Method' = rep('ets', nrow(ets.unrec.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'unrec'),
                bind_cols('fc' = ols.unrec.melt$value, 'error' = res.ols.unrec.melt$value,  
                'Method' = rep('ols', nrow(ols.unrec.melt)),
                'Series' =arima.unrec.melt$variable, 'Rec' = 'unrec'),
                bind_cols('fc' = stl.unrec.melt$value, 'error' = res.stl.unrec.melt$value,  
                'Method' = rep('stl', nrow(stl.unrec.melt)),
                'Series' =arima.unrec.melt$variable, 'Rec' = 'unrec'),
                bind_cols('fc' = BU.arima.melt$value, 'error' = res.BU.arima.melt$value,  
                'Method' = rep('arima', nrow(BU.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.BU'), 
                bind_cols('fc' = BU.ets.melt$value, 'error' = res.BU.ets.melt$value,  
                'Method' = rep('ets', nrow(BU.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.BU'), 
                bind_cols('fc' = BU.ols.melt$value, 'error' = res.BU.ols.melt$value,  
                'Method' = rep('ols', nrow(BU.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.BU'), 
                bind_cols('fc' = BU.stl.melt$value, 'error' = res.BU.stl.melt$value,  
                'Method' = rep('stl', nrow(BU.stl.melt)),
                'Series' = arima.unrec.melt$BUiable, 'Rec' = 'rec.BU'),
                bind_cols('fc' = CG.lambda.arima.melt$value, 'error' = res.CG.lambda.arima.melt$value,  
                'Method' = rep('arima', nrow(CG.lambda.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.lambda'), 
                bind_cols('fc' = CG.lambda.ets.melt$value, 'error' = res.CG.lambda.ets.melt$value,  
                'Method' = rep('ets', nrow(CG.lambda.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.lambda'), 
                bind_cols('fc' = CG.lambda.ols.melt$value, 'error' = res.CG.lambda.ols.melt$value,  
                'Method' = rep('ols', nrow(CG.lambda.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.lambda'), 
                bind_cols('fc' = CG.lambda.stl.melt$value, 'error' = res.CG.lambda.stl.melt$value,  
                'Method' = rep('stl', nrow(CG.lambda.stl.melt)),
                'Series' = arima.unrec.melt$lambdaiable, 'Rec' = 'rec.lambda'),
                bind_cols('fc' = CG.var.arima.melt$value, 'error' = res.CG.var.arima.melt$value,  
                'Method' = rep('arima', nrow(CG.var.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.var'), 
                bind_cols('fc' = CG.var.ets.melt$value, 'error' = res.CG.var.ets.melt$value,  
                'Method' = rep('ets', nrow(CG.var.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.var'), 
                bind_cols('fc' = CG.var.ols.melt$value, 'error' = res.CG.var.ols.melt$value,  
                'Method' = rep('ols', nrow(CG.var.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.var'), 
                bind_cols('fc' = CG.var.stl.melt$value, 'error' = res.CG.var.stl.melt$value,  
                'Method' = rep('stl', nrow(CG.var.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.var'),
                bind_cols('fc' = rec.CG.shrink.arima.melt$value, 'error' = res.rec.CG.shrink.arima.melt$value,  
                'Method' = rep('arima', nrow(rec.CG.shrink.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'), 
                bind_cols('fc' = rec.CG.shrink.ets.melt$value, 'error' = res.rec.CG.shrink.ets.melt$value,  
                'Method' = rep('ets', nrow(rec.CG.shrink.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = rec.CG.shrink.ols.melt$value, 'error' = res.rec.CG.shrink.ols.melt$value,  
                'Method' = rep('ols', nrow(rec.CG.shrink.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = rec.CG.shrink.stl.melt$value, 'error' = res.rec.CG.shrink.stl.melt$value,  
                'Method' = rep('stl', nrow(rec.CG.shrink.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = rec.CG.null.arima.melt$value, 'error' = res.rec.CG.null.arima.melt$value,  
                'Method' = rep('arima', nrow(rec.CG.null.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'), 
                bind_cols('fc' = rec.CG.null.ets.melt$value, 'error' = res.rec.CG.null.ets.melt$value,  
                'Method' = rep('ets', nrow(rec.CG.null.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'),
                bind_cols('fc' = rec.CG.null.ols.melt$value, 'error' = res.rec.CG.null.ols.melt$value,  
                'Method' = rep('ols', nrow(rec.CG.null.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'),
                bind_cols('fc' = rec.CG.null.stl.melt$value, 'error' = res.rec.CG.null.stl.melt$value,  
                'Method' = rep('stl', nrow(rec.CG.null.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'),
                bind_cols('fc' = net.test.melt$value, 'error' = 0,
                'Method' = rep('actual', nrow(rec.CG.shrink.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'actual')
                )

fc.all <- bind_rows(fc.all %>%
                      filter(Series == 'Total.in') %>%
                      mutate (Level = 'Total.in'),
                    fc.all %>%
                      filter(Series == 'Total.out') %>%
                      mutate (Level = 'Total.out'),
                    fc.all %>%
                      filter(Series == 'Outer') %>%
                      mutate (Level = 'Outer'),
                    fc.all %>%
                      filter(grepl('\\.in$', Series) & Series != 'Total.in') %>%
                      mutate (Level = 'Inflow'), 
                    fc.all %>%
                      filter(grepl('\\.out$', Series) & Series != 'Total.out') %>%
                      mutate (Level = 'Outflow'), 
                    fc.all %>% filter(!grepl('\\.in$|\\.out$|Outer$', Series)) %>%
                      mutate (Level = 'Bottom level'))

fc.all <- bind_cols(fc.all, 'id' = rep(1:6, nrow(fc.all)/6))

# Group by Level, rec_unrec, and method, then calculate RMSE
rmse_results_all <- fc.all %>%
  group_by(Rec, Method) %>%
  summarise(rmse = sqrt(mean(error^2)))

rmse_results <- fc.all %>%
  group_by(Level, Rec, Method) %>%
  summarise(rmse = sqrt(mean(error^2)))

library(tidyverse)

# Reshape the data to long format for plotting

error.all <- fc.all %>%
  select(error, Method, Rec, Level) %>%
  #filter(Rec != "rec.lambda" & Method !="actual") %>%
  filter(Method !="actual") %>%
  mutate(id = factor(paste(Method, Rec, sep = "."),
                     levels = c("ets.rec.shrink", "ets.rec.null", "ets.rec.lambda", "ets.unrec", 
                                "arima.rec.shrink", "arima.rec.null","arima.rec.lambda","arima.unrec", 
                                "ols.rec.shrink", "ols.rec.null","ols.rec.lambda","ols.unrec", 
                                "stl.rec.shrink", "stl.rec.null","stl.rec.lambda","stl.unrec"),
                     labels = c("ets.rec.shrink", "ets.rec.null", "ets.rec.lambda", "ets.unrec", 
                                "arima.rec.shrink", "arima.rec.null","arima.rec.lambda","arima.unrec", 
                                "ols.rec.shrink", "ols.rec.null","ols.rec.lambda","ols.unrec", 
                                "stl.rec.shrink", "stl.rec.null","stl.rec.lambda","stl.unrec")))
                     #levels = c("ets.rec.shrink",  "ets.unrec", "arima.rec.shrink","arima.unrec", 
                      #                    "ols.rec.shrink","ols.unrec"),
                     #labels = c("ets.rec.shrink",  "ets.unrec", "arima.rec.shrink", "arima.unrec", 
                      #                  "ols.rec.shrink", "ols.unrec")))

filtered_data <- error.all %>%
  filter(Level != "Outer" & Level != "Total.in"& Level != "Total.out" )


# Create a box plot with facets

boxplot.stat <- function(x) {
  coef <- 1.5
  n <- sum(!is.na(x))
  # calculate quantiles
  stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
  names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
  iqr <- diff(stats[c(2, 4)])
  # set whiskers
  outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
  if (any(outliers)) {
    stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
  }
  return(stats)
}

ggplot(error.all, aes(x = id, y = error/100000, fill = id)) +
  #geom_boxplot() +
  stat_summary(fun.data = boxplot.stat, geom = "boxplot", alpha = 0.5) +
  facet_wrap(~ Level) +
  labs(y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

error.all.total <- fc.all %>%
  select(error, Method, Rec) %>%
  #filter(Rec != "rec.lambda" & Method !="actual") %>%
  filter(Method !="actual") %>%
  mutate(id = factor(paste(Method, Rec, sep = "."),
                     levels = c("ets.rec.shrink", "ets.rec.null", "ets.rec.lambda", "ets.unrec", 
                                "arima.rec.shrink", "arima.rec.null","arima.rec.lambda","arima.unrec", 
                                "ols.rec.shrink", "ols.rec.null","ols.rec.lambda","ols.unrec", 
                                "stl.rec.shrink", "stl.rec.null","stl.rec.lambda","stl.unrec"),
                     labels = c("ets.rec.shrink", "ets.rec.null", "ets.rec.lambda", "ets.unrec", 
                                "arima.rec.shrink", "arima.rec.null","arima.rec.lambda","arima.unrec", 
                                "ols.rec.shrink", "ols.rec.null","ols.rec.lambda","ols.unrec", 
                                "stl.rec.shrink", "stl.rec.null","stl.rec.lambda","stl.unrec")))

ggplot(error.all.total, aes(x = id, y = error/100000, fill = id)) +
  geom_boxplot() +
  #stat_summary(fun.data = boxplot.stat, geom = "boxplot", alpha = 0.5) +
  labs(y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(filtered_data, aes(x = scale(error), color = id)) +
  geom_density(alpha = 0.2) + 
  facet_wrap(~ Level) +
  labs(y = "Value")+
  xlim(c(-0.02,0.005))

filtered_data <- fc.all %>%
  filter(Series == 'Outer')

ggplot(filtered_data, aes(x = id, y = fc, color = Method, linetype = Rec)) +
  geom_line() +
  labs(title = paste("Forecast Comparison"),
       x = "Time", y = "Forecast", color = "Method", linetype = 'Rec') +
  theme_minimal()



