olsfc.pwfc <- function(x, h, breakpoints = c(5, 10, 15, 20), maxlag = 0, nolag = NULL) {
  n <- length(x)
  num_segments <- length(breakpoints) + 1
  
  modeldata_pwfc <- data.frame(
    x = as.numeric(x),
    segment = factor(findInterval(seq_along(x), c(0, breakpoints, n))),
    trend = seq_along(x),
    season = factor(cycle(x))
  )
  
  modeldata_olsfc <- data.frame(
    x = as.numeric(x),
    trend1 = seq_along(x),
    trend2 = (seq_along(x))^(-1),
    season = factor(cycle(x))
  )
  
  if (maxlag > 0) {
    lagnames_pwfc <- paste0("lag", seq(maxlag))
    lagnames_olsfc <- paste0("lag", seq(maxlag))
    for (i in seq_along(lagnames_pwfc)) {
      modeldata_pwfc[[lagnames_pwfc[i]]] <- c(rep(NA, i), x[seq(n - i)])
      modeldata_olsfc[[lagnames_olsfc[i]]] <- c(rep(NA, i), x[seq(n - i)])
    }
  } else {
    lagnames_pwfc <- NULL
    lagnames_olsfc <- NULL
  }
  
  form_pwfc <- "x ~ segment + trend + season"
  form_olsfc <- "x ~ trend1 + trend2 + season"
  if (length(nolag) == 0)
    nolag <- seq(maxlag)
  for (i in nolag) {
    form_pwfc <- paste0(form_pwfc, " + ", "lag", i)
    form_olsfc <- paste0(form_olsfc, " + ", "lag", i)
  }
  form_pwfc <- as.formula(form_pwfc)
  form_olsfc <- as.formula(form_olsfc)
  
  fit_pwfc <- lm(form_pwfc, data = modeldata_pwfc)
  fit_olsfc <- lm(form_olsfc, data = modeldata_olsfc)
  fit_pwfc_scale <- fitted(fit_pwfc)
  fit_olsfc_scale <- fitted(fit_olsfc)
  
  fc_pwfc <- ts(numeric(h),
                frequency = frequency(x),
                start = tsp(x)[2] + 1 / frequency(x)
  )
  
  fc_olsfc <- ts(numeric(h),
                 frequency = frequency(x),
                 start = tsp(x)[2] + 1 / frequency(x)
  )
  
  trend <- length(x) + seq(h)
  trend1 <- length(x) + seq(h)
  trend2 <- (length(x) + seq(h))^(-1)
  season_pwfc <- factor(cycle(fc_pwfc))
  season_olsfc <- factor(cycle(fc_olsfc))
  
  for (i in seq_along(fc_pwfc)) {
    segment <- factor(num_segments)
    for (j in seq(num_segments - 1)) {
      if (trend[i] <= breakpoints[j]) {
        segment <- factor(j)
        break
      }
    }
    
    newdata_pwfc <- data.frame(segment = segment, trend = trend[i], season = season_pwfc[i])
    newdata_olsfc <- data.frame(trend1 = trend1[i], trend2 = trend2[i], season = season_olsfc[i])
    
    for (k in seq_along(lagnames_pwfc))
      newdata_pwfc[[lagnames_pwfc[k]]] <- tail(x, k)[1]
    for (k in seq_along(lagnames_olsfc))
      newdata_olsfc[[lagnames_olsfc[k]]] <- tail(x, k)[1]
    
    fc_pwfc[i] <- predict(fit_pwfc, newdata = newdata_pwfc)
    fc_olsfc[i] <- predict(fit_olsfc, newdata = newdata_olsfc)
    
    if (maxlag > 0) {
      newdata_pwfc[lagnames_pwfc[seq(maxlag)]] <- c(fc_pwfc[i], newdata_pwfc[lagnames_pwfc[1:(maxlag - 1)]])
      newdata_olsfc[lagnames_olsfc[seq(maxlag)]] <- c(fc_olsfc[i], newdata_olsfc[lagnames_olsfc[1:(maxlag - 1)]])
    }
    
    newdata_pwfc[['segment']] <- segment
    newdata_pwfc[['trend']] <- trend[i + 1]
    newdata_pwfc[['season']] <- season_pwfc[i + 1]
    newdata_olsfc[['trend1']] <- trend1[i + 1]
    newdata_olsfc[['trend2']] <- trend2[i + 1]
    newdata_olsfc[['season']] <- season_olsfc[i + 1]
  }
  
  if (sum(abs(fit_pwfc_scale-x[-nolag])) < sum(abs(fit_olsfc_scale - x[-nolag]))) {
    return(list(fc_pwfc, fit_pwfc_scale))
  } else {
    return(list(fc_olsfc, fit_olsfc_scale))
  }
}
