

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
BU.arima <- t(read_csv('fc.rec.BU.arima.csv')[,-1])
BU.ets <- t(read_csv('fc.rec.BU.ets.csv')[,-1])
BU.ols <- t(read_csv('fc.rec.BU.ols.csv')[,-1])
BU.stl <- t(read_csv('fc.rec.BU.stl.csv')[,-1])

## rec fc - BU - res
res.BU.arima <- net.test - BU.arima
res.BU.ets <- net.test - BU.ets
res.BU.ols <- net.test - BU.ols
res.BU.stl <- net.test - BU.stl

## rec fc - lambda weight
CG.WLS.S.arima <- read_csv('fc.rec.CG.lambda.arima.csv')[,-1]
CG.WLS.S.ets <- read_csv('fc.rec.CG.lambda.ets.csv')[,-1]
CG.WLS.S.ols <- read_csv('fc.rec.CG.lambda.ols.csv')[,-1]
CG.WLS.S.stl <- read_csv('fc.rec.CG.lambda.stl.csv')[,-1]

## rec fc - lambda weight - res
res.CG.WLS.S.arima <- net.test - CG.WLS.S.arima
res.CG.WLS.S.ets <- net.test - CG.WLS.S.ets
res.CG.WLS.S.ols <- net.test - CG.WLS.S.ols
res.CG.WLS.S.stl <- net.test - CG.WLS.S.stl

## rec fc - var weight
CG.WLS.V.arima <- read_csv('fc.rec.CG.var.arima.csv')[,-1]
CG.WLS.V.ets <- read_csv('fc.rec.CG.var.ets.csv')[,-1]
CG.WLS.V.ols <- read_csv('fc.rec.CG.var.ols.csv')[,-1]
CG.WLS.V.stl <- read_csv('fc.rec.CG.var.stl.csv')[,-1]

## rec fc - var weight - res
res.CG.WLS.V.arima <- net.test - CG.WLS.V.arima
res.CG.WLS.V.ets <- net.test - CG.WLS.V.ets
res.CG.WLS.V.ols <- net.test - CG.WLS.V.ols
res.CG.WLS.V.stl <- net.test - CG.WLS.V.stl

## rec fc - shrink weight
rec.CG.MinT.arima <- read_csv('fc.rec.CG.shrink.arima.csv')[,-1]
rec.CG.MinT.ets <- read_csv('fc.rec.CG.shrink.ets.csv')[,-1]
rec.CG.MinT.ols <- read_csv('fc.rec.CG.shrink.ols.csv')[,-1]
rec.CG.MinT.stl <- read_csv('fc.rec.CG.shrink.stl.csv')[,-1]

## rec fc - shrink weight - res
res.rec.CG.MinT.arima <- net.test - rec.CG.MinT.arima
res.rec.CG.MinT.ets <- net.test - rec.CG.MinT.ets
res.rec.CG.MinT.ols <- net.test - rec.CG.MinT.ols
res.rec.CG.MinT.stl <- net.test - rec.CG.MinT.stl

## rec fc - null weight
rec.CG.OLS.arima <- read_csv('fc.rec.CG.null.arima.csv')[,-1]
rec.CG.OLS.ets <- read_csv('fc.rec.CG.null.ets.csv')[,-1]
rec.CG.OLS.ols <- read_csv('fc.rec.CG.null.ols.csv')[,-1]
rec.CG.OLS.stl <- read_csv('fc.rec.CG.null.stl.csv')[,-1]


res.rec.CG.OLS.arima <- net.test - rec.CG.OLS.arima
res.rec.CG.OLS.ets <- net.test - rec.CG.OLS.ets
res.rec.CG.OLS.ols <- net.test - rec.CG.OLS.ols
res.rec.CG.OLS.stl <- net.test - rec.CG.OLS.stl

arima.unrec.melt <- reshape2::melt(arima.unrec)
ets.unrec.melt <- reshape2::melt(ets.unrec)
ols.unrec.melt <- reshape2::melt(ols.unrec)
stl.unrec.melt <- reshape2::melt(stl.unrec)
####
colnames(BU.arima) <- colnames(net.test)
colnames(BU.ets) <- colnames(net.test)
colnames(BU.ols) <- colnames(net.test)
colnames(BU.stl) <- colnames(net.test)
BU.arima.melt <- reshape2::melt(BU.arima)
BU.ets.melt <- reshape2::melt(BU.ets)
BU.ols.melt <- reshape2::melt(BU.ols)
BU.stl.melt <- reshape2::melt(BU.stl)

CG.WLS.S.arima.melt <- reshape2::melt(CG.WLS.S.arima)
CG.WLS.S.ets.melt <- reshape2::melt(CG.WLS.S.ets)
CG.WLS.S.ols.melt <- reshape2::melt(CG.WLS.S.ols)
CG.WLS.S.stl.melt <- reshape2::melt(CG.WLS.S.stl)

CG.WLS.V.arima.melt <- reshape2::melt(CG.WLS.V.arima)
CG.WLS.V.ets.melt <- reshape2::melt(CG.WLS.V.ets)
CG.WLS.V.ols.melt <- reshape2::melt(CG.WLS.V.ols)
CG.WLS.V.stl.melt <- reshape2::melt(CG.WLS.V.stl)

rec.CG.MinT.arima.melt <- reshape2::melt(rec.CG.MinT.arima)
rec.CG.MinT.ets.melt <- reshape2::melt(rec.CG.MinT.ets)
rec.CG.MinT.ols.melt <- reshape2::melt(rec.CG.MinT.ols)
rec.CG.MinT.stl.melt <- reshape2::melt(rec.CG.MinT.stl)

rec.CG.OLS.arima.melt <- reshape2::melt(rec.CG.OLS.arima)
rec.CG.OLS.ets.melt <- reshape2::melt(rec.CG.OLS.ets)
rec.CG.OLS.ols.melt <- reshape2::melt(rec.CG.OLS.ols)
rec.CG.OLS.stl.melt <- reshape2::melt(rec.CG.OLS.stl)

res.arima.unrec.melt <- reshape2::melt(res.arima.unrec)
res.ets.unrec.melt <- reshape2::melt(res.ets.unrec)
res.ols.unrec.melt <- reshape2::melt(res.ols.unrec)
res.stl.unrec.melt <- reshape2::melt(res.stl.unrec)

res.BU.arima.melt <- reshape2::melt(res.BU.arima)
res.BU.ets.melt <- reshape2::melt(res.BU.ets)
res.BU.ols.melt <- reshape2::melt(res.BU.ols)
res.BU.stl.melt <- reshape2::melt(res.BU.stl)

res.CG.WLS.S.arima.melt <- reshape2::melt(res.CG.WLS.S.arima)
res.CG.WLS.S.ets.melt <- reshape2::melt(res.CG.WLS.S.ets)
res.CG.WLS.S.ols.melt <- reshape2::melt(res.CG.WLS.S.ols)
res.CG.WLS.S.stl.melt <- reshape2::melt(res.CG.WLS.S.stl)

res.CG.WLS.V.arima.melt <- reshape2::melt(res.CG.WLS.V.arima)
res.CG.WLS.V.ets.melt <- reshape2::melt(res.CG.WLS.V.ets)
res.CG.WLS.V.ols.melt <- reshape2::melt(res.CG.WLS.V.ols)
res.CG.WLS.V.stl.melt <- reshape2::melt(res.CG.WLS.V.stl)

res.rec.CG.MinT.arima.melt <- reshape2::melt(res.rec.CG.MinT.arima)
res.rec.CG.MinT.ets.melt <- reshape2::melt(res.rec.CG.MinT.ets)
res.rec.CG.MinT.ols.melt <- reshape2::melt(res.rec.CG.MinT.ols)
res.rec.CG.MinT.stl.melt <- reshape2::melt(res.rec.CG.MinT.stl)

res.rec.CG.OLS.arima.melt <- reshape2::melt(res.rec.CG.OLS.arima)
res.rec.CG.OLS.ets.melt <- reshape2::melt(res.rec.CG.OLS.ets)
res.rec.CG.OLS.ols.melt <- reshape2::melt(res.rec.CG.OLS.ols)
res.rec.CG.OLS.stl.melt <- reshape2::melt(res.rec.CG.OLS.stl)

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
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.BU'),
                bind_cols('fc' = CG.WLS.S.arima.melt$value, 'error' = res.CG.WLS.S.arima.melt$value,  
                'Method' = rep('arima', nrow(CG.WLS.S.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.S'), 
                bind_cols('fc' = CG.WLS.S.ets.melt$value, 'error' = res.CG.WLS.S.ets.melt$value,  
                'Method' = rep('ets', nrow(CG.WLS.S.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.S'), 
                bind_cols('fc' = CG.WLS.S.ols.melt$value, 'error' = res.CG.WLS.S.ols.melt$value,  
                'Method' = rep('ols', nrow(CG.WLS.S.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.S'), 
                bind_cols('fc' = CG.WLS.S.stl.melt$value, 'error' = res.CG.WLS.S.stl.melt$value,  
                'Method' = rep('stl', nrow(CG.WLS.S.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.S'),
                bind_cols('fc' = CG.WLS.V.arima.melt$value, 'error' = res.CG.WLS.V.arima.melt$value,  
                'Method' = rep('arima', nrow(CG.WLS.V.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.V'), 
                bind_cols('fc' = CG.WLS.V.ets.melt$value, 'error' = res.CG.WLS.V.ets.melt$value,  
                'Method' = rep('ets', nrow(CG.WLS.V.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.V'), 
                bind_cols('fc' = CG.WLS.V.ols.melt$value, 'error' = res.CG.WLS.V.ols.melt$value,  
                'Method' = rep('ols', nrow(CG.WLS.V.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.V'), 
                bind_cols('fc' = CG.WLS.V.stl.melt$value, 'error' = res.CG.WLS.V.stl.melt$value,  
                'Method' = rep('stl', nrow(CG.WLS.V.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.WLS.V'),
                bind_cols('fc' = rec.CG.MinT.arima.melt$value, 'error' = res.rec.CG.MinT.arima.melt$value,  
                'Method' = rep('arima', nrow(rec.CG.MinT.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.MinT'), 
                bind_cols('fc' = rec.CG.MinT.ets.melt$value, 'error' = res.rec.CG.MinT.ets.melt$value,  
                'Method' = rep('ets', nrow(rec.CG.MinT.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.MinT'),
                bind_cols('fc' = rec.CG.MinT.ols.melt$value, 'error' = res.rec.CG.MinT.ols.melt$value,  
                'Method' = rep('ols', nrow(rec.CG.MinT.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.MinT'),
                bind_cols('fc' = rec.CG.MinT.stl.melt$value, 'error' = res.rec.CG.MinT.stl.melt$value,  
                'Method' = rep('stl', nrow(rec.CG.MinT.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.MinT'),
                bind_cols('fc' = rec.CG.OLS.arima.melt$value, 'error' = res.rec.CG.OLS.arima.melt$value,  
                'Method' = rep('arima', nrow(rec.CG.OLS.arima.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.OLS'), 
                bind_cols('fc' = rec.CG.OLS.ets.melt$value, 'error' = res.rec.CG.OLS.ets.melt$value,  
                'Method' = rep('ets', nrow(rec.CG.OLS.ets.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.OLS'),
                bind_cols('fc' = rec.CG.OLS.ols.melt$value, 'error' = res.rec.CG.OLS.ols.melt$value,  
                'Method' = rep('ols', nrow(rec.CG.OLS.ols.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.OLS'),
                bind_cols('fc' = rec.CG.OLS.stl.melt$value, 'error' = res.rec.CG.OLS.stl.melt$value,  
                'Method' = rep('stl', nrow(rec.CG.OLS.stl.melt)),
                'Series' = arima.unrec.melt$variable, 'Rec' = 'rec.OLS'),
                bind_cols('fc' = net.test.melt$value, 'error' = 0,
                'Method' = rep('actual', nrow(rec.CG.MinT.ols.melt)),
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

fc.all <- bind_cols(fc.all, 'id' = rep(1:1, nrow(fc.all)/1))

# Group by Level, rec_unrec, and method, then calculate RMSE
rmse_results_all <- fc.all %>%
  group_by(Rec, Method) %>%
  summarise(rmse = sqrt(mean(error^2)))

rmse_results <- fc.all %>%
  group_by(Level, Rec, Method) %>%
  summarise(rmse = sqrt(mean(error^2)))

###### Plot series by different reconciliation methods


library(dplyr)
library(patchwork)

# Define a function to create the plot for a specific method

## for total series we divide the fc by 10000 - for the other only use fc
create_plot <- function(method, ylim, series) {
  fc.all %>%
    filter(Method %in% c(method, "actual"), Series == series) %>%
    ggplot(aes(x = id, y = fc/10000, colour = Rec)) +
    geom_line(size = 1) +
    xlab("Horizon") +
    ylab("Count") +
    ggtitle(method) +
    scale_color_manual(
      name = "Reconciled",
      values = c(
        actual = "black",
        unrec = "pink",
        rec.BU = "green",
        rec.OLS = "blue",
        rec.WLS.S = "red",
        rec.WLS.V = "darkorchid",
        rec.MinT = "cyan"
      )) +
    theme_bw() +
    theme(legend.position = "none", 
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 20),  
          plot.title = element_text(size = 25),
          legend.title = element_text(size = 20))  
}

# Define the methods and series
methods <- c("arima", "ets", "stl", "ols")
#series <- "United_States.out"
series <- "Total.out"
series <- "Blake_Lively.in"
#series <- "United_States.in"
#series <- "World_War_II.out"
#series <- "Guyana.in"
#series <- "John_F._Kennedy.in"

# Create a list of plots for each method
plots <- lapply(methods, function(method) create_plot(method, range(fc.all %>% filter(Method == "actual", Series == series) %>% pull(fc)), series))

# Combine the plots with a shared legend
combined_plot <- wrap_plots(plots, ncol = 2, guides = "collect")

# Manually create a legend for the combined plot
legend <- get_legend(plots[[2]])  # Use the legend from the first plot
combined_plot <- combined_plot + theme(legend.position = c(0.5, 0.02)) + theme(legend.text = element_text(size = 20))

# Display the combined plot with a shared legend
print(combined_plot)








library(tidyverse)

# Reshape the data to long format for plotting

error.all <- fc.all %>%
  select(error, Method, Rec, Level) %>%
  #filter(Rec != "rec.lambda" & Method !="actual") %>%
  filter(Method !="actual") %>%
  mutate(id = factor(paste(Method, Rec, sep = "."),
                     levels = c("ets.rec.MinT", "ets.rec.OLS", "ets.rec.WLS.S", "ets.unrec", 
                                "arima.rec.MinT", "arima.rec.OLS","arima.rec.WLS.S","arima.unrec", 
                                "ols.rec.MinT", "ols.rec.OLS","ols.rec.WLS.S","ols.unrec", 
                                "stl.rec.MinT", "stl.rec.OLS","stl.rec.WLS.S","stl.unrec"),
                     labels = c("ets.rec.MinT", "ets.rec.OLS", "ets.rec.WLS.S", "ets.unrec", 
                                "arima.rec.MinT", "arima.rec.OLS","arima.rec.WLS.S","arima.unrec", 
                                "ols.rec.MinT", "ols.rec.OLS","ols.rec.WLS.S","ols.unrec", 
                                "stl.rec.MinT", "stl.rec.OLS","stl.rec.WLS.S","stl.unrec")))
                     #levels = c("ets.rec.MinT",  "ets.unrec", "arima.rec.MinT","arima.unrec", 
                      #                    "ols.rec.MinT","ols.unrec"),
                     #labels = c("ets.rec.MinT",  "ets.unrec", "arima.rec.MinT", "arima.unrec", 
                      #                  "ols.rec.MinT", "ols.unrec")))

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
                     levels = c("ets.rec.MinT", "ets.rec.OLS", "ets.rec.WLS.S", "ets.unrec", 
                                "arima.rec.MinT", "arima.rec.OLS","arima.rec.WLS.S","arima.unrec", 
                                "ols.rec.MinT", "ols.rec.OLS","ols.rec.WLS.S","ols.unrec", 
                                "stl.rec.MinT", "stl.rec.OLS","stl.rec.WLS.S","stl.unrec"),
                     labels = c("ets.rec.MinT", "ets.rec.OLS", "ets.rec.WLS.S", "ets.unrec", 
                                "arima.rec.MinT", "arima.rec.OLS","arima.rec.WLS.S","arima.unrec", 
                                "ols.rec.MinT", "ols.rec.OLS","ols.rec.WLS.S","ols.unrec", 
                                "stl.rec.MinT", "stl.rec.OLS","stl.rec.WLS.S","stl.unrec")))

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



