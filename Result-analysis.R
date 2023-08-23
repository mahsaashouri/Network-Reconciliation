

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
res.train.naive <- read_csv('res.train.naive.csv')[,-1]

## base fc
arima.unrec <- read_csv('fc.arima.unrec.csv')[,-1]
ets.unrec <- read_csv('fc.ets.unrec.csv')[,-1] 
ols.unrec <- read_csv('fc.ols.unrec.csv')[,-1] 
naive.unrec <- read_csv('fc.naive.unrec.csv')[,-1] 
## base res
res.arima.unrec <- net.test - arima.unrec
res.ets.unrec <- net.test - ets.unrec
res.ols.unrec <- net.test - ols.unrec
res.naive.unrec <- net.test - naive.unrec
## base train fc
fc.train.arima.unrec <- res.train.arima[-1,] + res.train.arima[-1,]
fc.train.ets.unrec <- res.train.ets[-1,] + res.train.ets[-1,]
fc.train.ols.unrec <- res.train.ols + res.train.ols
fc.train.naive.unrec <- res.train.naive + res.train.naive

## rec fc - null weight
CG.null.arima <- read_csv('fc.rec.CG.null.arima.csv')[,-1]
CG.null.ets <- read_csv('fc.rec.CG.null.ets.csv')[,-1]
CG.null.ols <- read_csv('fc.rec.CG.null.ols.csv')[,-1]
CG.null.naive <- read_csv('fc.rec.CG.null.naive.csv')[,-1]
## rec fc - null weight - res
res.CG.null.arima <- net.test - CG.null.arima
res.CG.null.ets <- net.test - CG.null.ets
res.CG.null.ols <- net.test - CG.null.ols
res.CG.null.naive <- net.test - CG.null.naive

## rec fc - shrink weight
rec.CG.shrink.arima <- read_csv('fc.rec.CG.shrink.arima.csv')[,-1]
rec.CG.shrink.ets <- read_csv('fc.rec.CG.shrink.ets.csv')[,-1]
rec.CG.shrink.ols <- read_csv('fc.rec.CG.shrink.ols.csv')[,-1]
rec.CG.shrink.naive <- read_csv('fc.rec.CG.shrink.naive.csv')[,-1]
## rec fc - shrink weight - res
res.rec.CG.shrink.arima <- net.test - rec.CG.shrink.arima
res.rec.CG.shrink.ets <- net.test - rec.CG.shrink.ets
res.rec.CG.shrink.ols <- net.test - rec.CG.shrink.ols
res.rec.CG.shrink.naive <- net.test - rec.CG.shrink.naive


arima.unrec.melt <- reshape2::melt(arima.unrec)
ets.unrec.melt <- reshape2::melt(ets.unrec)
ols.unrec.melt <- reshape2::melt(ols.unrec)
naive.unrec.melt <- reshape2::melt(naive.unrec)
CG.null.arima.melt <- reshape2::melt(CG.null.arima)
CG.null.ets.melt <- reshape2::melt(CG.null.ets)
CG.null.ols.melt <- reshape2::melt(CG.null.ols)
CG.null.naive.melt <- reshape2::melt(CG.null.naive)
rec.CG.shrink.arima.melt <- reshape2::melt(rec.CG.shrink.arima)
rec.CG.shrink.ets.melt <- reshape2::melt(rec.CG.shrink.ets)
rec.CG.shrink.ols.melt <- reshape2::melt(rec.CG.shrink.ols)
rec.CG.shrink.naive.melt <- reshape2::melt(rec.CG.shrink.naive)


res.arima.unrec.melt <- reshape2::melt(res.arima.unrec)
res.ets.unrec.melt <- reshape2::melt(res.ets.unrec)
res.ols.unrec.melt <- reshape2::melt(res.ols.unrec)
res.naive.unrec.melt <- reshape2::melt(res.naive.unrec)
res.CG.null.arima.melt <- reshape2::melt(res.CG.null.arima)
res.CG.null.ets.melt <- reshape2::melt(res.CG.null.ets)
res.CG.null.ols.melt <- reshape2::melt(res.CG.null.ols)
res.CG.null.naive.melt <- reshape2::melt(res.CG.null.naive)
res.rec.CG.shrink.arima.melt <- reshape2::melt(res.rec.CG.shrink.arima)
res.rec.CG.shrink.ets.melt <- reshape2::melt(res.rec.CG.shrink.ets)
res.rec.CG.shrink.ols.melt <- reshape2::melt(res.rec.CG.shrink.ols)
res.rec.CG.shrink.naive.melt <- reshape2::melt(res.rec.CG.shrink.naive)

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
                bind_cols('fc' = naive.unrec.melt$value, 'error' = res.naive.unrec.melt$value,  
                'Method' = rep('naive', nrow(naive.unrec.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'unrec'),
                bind_cols('fc' = CG.null.arima.melt$value, 'error' = res.CG.null.arima.melt$value,  
                'Method' = rep('arima', nrow(CG.null.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'), 
                bind_cols('fc' = CG.null.ets.melt$value, 'error' = res.CG.null.ets.melt$value,  
                'Method' = rep('ets', nrow(CG.null.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'), 
                bind_cols('fc' = CG.null.ols.melt$value, 'error' = res.CG.null.ols.melt$value,  
                'Method' = rep('ols', nrow(CG.null.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'), 
                bind_cols('fc' = CG.null.naive.melt$value, 'error' = res.CG.null.naive.melt$value,  
                'Method' = rep('naive', nrow(CG.null.naive.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.null'), 
                bind_cols('fc' = rec.CG.shrink.arima.melt$value, 'error' = res.rec.CG.shrink.arima.melt$value,  
                'Method' = rep('arima', nrow(rec.CG.shrink.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'), 
                bind_cols('fc' = rec.CG.shrink.ets.melt$value, 'error' = res.rec.CG.shrink.ets.melt$value,  
                'Method' = rep('ets', nrow(rec.CG.shrink.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = rec.CG.shrink.ols.melt$value, 'error' = res.rec.CG.shrink.ols.melt$value,  
                'Method' = rep('ols', nrow(rec.CG.shrink.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = rec.CG.shrink.naive.melt$value, 'error' = res.rec.CG.shrink.naive.melt$value,  
                'Method' = rep('naive', nrow(rec.CG.shrink.naive.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.shrink'),
                bind_cols('fc' = net.test.melt$value, 'error' = 0,
                'Method' = rep('actual', nrow(rec.CG.shrink.naive.melt)),
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

filtered_data <- fc.all %>%
  filter(Series == 'other::World_Thinking_Day')

ggplot(filtered_data, aes(x = id, y = fc, color = Method, linetype = Rec)) +
  geom_line() +
  labs(title = paste("Forecast Comparison"),
       x = "Time", y = "Forecast", color = "Method", linetype = 'Rec') +
  theme_minimal()



