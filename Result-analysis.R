

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


## base fc
arima.unrec <- read_csv('fc.arima.unrec.csv')[,-1]
ets.unrec <- read_csv('fc.ets.unrec.csv')[,-1] 
ols.unrec <- read_csv('fc.ols.unrec.csv')[,-1] 

## base res
res.arima.unrec <- net.test - arima.unrec
res.ets.unrec <- net.test - ets.unrec
res.ols.unrec <- net.test - ols.unrec

## base train fc
fc.train.arima.unrec <- res.train.arima[-1,] + res.train.arima[-1,]
fc.train.ets.unrec <- res.train.ets[-1,] + res.train.ets[-1,]
fc.train.ols.unrec <- res.train.ols + res.train.ols


## rec fc - lambda weight
CG.lambda.arima <- read_csv('fc.rec.CG.lambda.arima.csv')[,-1]
CG.lambda.ets <- read_csv('fc.rec.CG.lambda.ets.csv')[,-1]
CG.lambda.ols <- read_csv('fc.rec.CG.lambda.ols.csv')[,-1]

## rec fc - lambda weight - res
res.CG.lambda.arima <- net.test - CG.lambda.arima
res.CG.lambda.ets <- net.test - CG.lambda.ets
res.CG.lambda.ols <- net.test - CG.lambda.ols


## rec fc - shrink weight
rec.CG.shrink.arima <- read_csv('fc.rec.CG.shrink.arima.csv')[,-1]
rec.CG.shrink.ets <- read_csv('fc.rec.CG.shrink.ets.csv')[,-1]
rec.CG.shrink.ols <- read_csv('fc.rec.CG.shrink.ols.csv')[,-1]

## rec fc - shrink weight - res
res.rec.CG.shrink.arima <- net.test - rec.CG.shrink.arima
res.rec.CG.shrink.ets <- net.test - rec.CG.shrink.ets
res.rec.CG.shrink.ols <- net.test - rec.CG.shrink.ols



arima.unrec.melt <- reshape2::melt(arima.unrec)
ets.unrec.melt <- reshape2::melt(ets.unrec)
ols.unrec.melt <- reshape2::melt(ols.unrec)

CG.lambda.arima.melt <- reshape2::melt(CG.lambda.arima)
CG.lambda.ets.melt <- reshape2::melt(CG.lambda.ets)
CG.lambda.ols.melt <- reshape2::melt(CG.lambda.ols)

rec.CG.shrink.arima.melt <- reshape2::melt(rec.CG.shrink.arima)
rec.CG.shrink.ets.melt <- reshape2::melt(rec.CG.shrink.ets)
rec.CG.shrink.ols.melt <- reshape2::melt(rec.CG.shrink.ols)



res.arima.unrec.melt <- reshape2::melt(res.arima.unrec)
res.ets.unrec.melt <- reshape2::melt(res.ets.unrec)
res.ols.unrec.melt <- reshape2::melt(res.ols.unrec)

res.CG.lambda.arima.melt <- reshape2::melt(res.CG.lambda.arima)
res.CG.lambda.ets.melt <- reshape2::melt(res.CG.lambda.ets)
res.CG.lambda.ols.melt <- reshape2::melt(res.CG.lambda.ols)

res.rec.CG.shrink.arima.melt <- reshape2::melt(res.rec.CG.shrink.arima)
res.rec.CG.shrink.ets.melt <- reshape2::melt(res.rec.CG.shrink.ets)
res.rec.CG.shrink.ols.melt <- reshape2::melt(res.rec.CG.shrink.ols)


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
                bind_cols('fc' = CG.lambda.arima.melt$value, 'error' = res.CG.lambda.arima.melt$value,  
                'Method' = rep('arima', nrow(CG.lambda.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.lambda'), 
                bind_cols('fc' = CG.lambda.ets.melt$value, 'error' = res.CG.lambda.ets.melt$value,  
                'Method' = rep('ets', nrow(CG.lambda.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.lambda'), 
                bind_cols('fc' = CG.lambda.ols.melt$value, 'error' = res.CG.lambda.ols.melt$value,  
                'Method' = rep('ols', nrow(CG.lambda.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.lambda'), 
                bind_cols('fc' = rec.CG.shrink.arima.melt$value, 'error' = res.rec.CG.shrink.arima.melt$value,  
                'Method' = rep('arima', nrow(rec.CG.shrink.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'), 
                bind_cols('fc' = rec.CG.shrink.ets.melt$value, 'error' = res.rec.CG.shrink.ets.melt$value,  
                'Method' = rep('ets', nrow(rec.CG.shrink.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = rec.CG.shrink.ols.melt$value, 'error' = res.rec.CG.shrink.ols.melt$value,  
                'Method' = rep('ols', nrow(rec.CG.shrink.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
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

fc.all <- bind_cols(fc.all, 'id' = rep(1:12, nrow(fc.all)/12))

# Group by Level, rec_unrec, and method, then calculate RMSE
rmse_results <- fc.all %>%
  group_by(Level, Rec, Method) %>%
  summarise(rmse = sqrt(mean(error^2)))

library(tidyverse)

# Reshape the data to long format for plotting

error.all <- fc.all %>%
  select(error, Method, Rec, Level) %>%
  filter(Rec != "rec.lambda" & Method !="actual") %>%
  mutate(id = factor(paste(Method, Rec, sep = "."),
                     levels = c("ets.rec.shrink", "ets.unrec", "arima.rec.shrink", "arima.unrec", 
                                "ols.rec.shrink","ols.unrec"),
                     labels = c("ets.rec.shrink", "ets.unrec", "arima.rec.shrink", "arima.unrec", 
                                "ols.rec.shrink","ols.unrec")))

filtered_data <- error.all %>%
  filter(Level != "Outer" & Level != "Total.in"& Level != "Total.out")


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

ggplot(filtered_data, aes(x = id, y = error, fill = id)) +
  #geom_boxplot() +
  stat_summary(fun.data = boxplot.stat, geom = "boxplot", alpha = 0.5) +
  facet_wrap(~ Level) +
  labs(y = "Value") 


filtered_data <- fc.all %>%
  filter(Series == 'other::World_Thinking_Day')

ggplot(filtered_data, aes(x = id, y = fc, color = Method, linetype = Rec)) +
  geom_line() +
  labs(title = paste("Forecast Comparison"),
       x = "Time", y = "Forecast", color = "Method", linetype = 'Rec') +
  theme_minimal()



