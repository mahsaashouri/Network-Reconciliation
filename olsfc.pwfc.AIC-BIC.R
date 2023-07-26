olsfc.pwfc <- function(x, h, breakpoints = c(5, 10, 15, 20), maxlag = 0, nolag = NULL, criterion = "AIC") {
  n <- length(x)
  num_segments <- length(breakpoints) + 1
  
  # Identify the segment for each data point
  segments <- findInterval(seq_along(x), c(0, breakpoints, n+1))
  
  modeldata_pwfc <- data.frame(
    x = as.numeric(x),
    segment = factor(segments),
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
  
  if (criterion == "AIC") {
    aic_pwfc <- AIC(fit_pwfc)
    aic_olsfc <- AIC(fit_olsfc)
    
    if (aic_pwfc < aic_olsfc) {
      selected_fit <- fit_pwfc
      selected_data <- modeldata_pwfc
    } else {
      selected_fit <- fit_olsfc
      selected_data <- modeldata_olsfc
    }
  } else if (criterion == "BIC") {
    bic_pwfc <- BIC(fit_pwfc)
    bic_olsfc <- BIC(fit_olsfc)
    
    if (bic_pwfc < bic_olsfc) {
      selected_fit <- fit_pwfc
      selected_data <- modeldata_pwfc
    } else {
      selected_fit <- fit_olsfc
      selected_data <- modeldata_olsfc
    }
  } else {
    stop("Invalid criterion. Please choose either 'AIC' or 'BIC'.")
  }
  
  #aic_pwfc <- AIC(fit_pwfc)
  #aic_olsfc <- AIC(fit_olsfc)
  
  fc_pwfc <- ts(numeric(h),
                frequency = frequency(x),
                start = tsp(x)[2] + 1 / frequency(x)
  )
  
  if ("segment" %in% colnames(selected_data)) {
    trend <- length(x) + seq(h)
    season_pwfc <- factor(cycle(fc_pwfc))
    
    for (i in seq_along(fc_pwfc)) {
      segment <- factor(num_segments)
      for (j in seq(num_segments - 1)) {
        if (trend[i] <= breakpoints[j]) {
          segment <- factor(j)
          break
        }
      }
      
      newdata_pwfc <- data.frame(segment = segment, trend = trend[i], season = season_pwfc[i])
      
      for (k in seq_along(lagnames_pwfc))
        newdata_pwfc[[lagnames_pwfc[k]]] <- tail(x, k)[1]
      
      fc_pwfc[i] <- predict(fit_pwfc, newdata = newdata_pwfc)
      
      if (maxlag > 0) {
        newdata_pwfc[lagnames_pwfc[seq(maxlag)]] <- c(fc_pwfc[i], newdata_pwfc[lagnames_pwfc[1:(maxlag - 1)]])
      }
      
      newdata_pwfc[['segment']] <- segment
      newdata_pwfc[['trend']] <- trend[i + 1]
      newdata_pwfc[['season']] <- season_pwfc[i + 1]
    }
    
    return(list(fc_pwfc, fitted(fit_pwfc)))
  } else {
    fc_olsfc <- ts(numeric(h),
                   frequency = frequency(x),
                   start = tsp(x)[2] + 1 / frequency(x)
    )
    
    trend1 <- length(x) + seq(h)
    trend2 <- (length(x) + seq(h))^(-1)
    season_olsfc <- factor(cycle(fc_olsfc))
    
    for (i in seq_along(fc_olsfc)) {
      segment <- factor(1)
      newdata_olsfc <- data.frame(trend1 = trend1[i], trend2 = trend2[i], season = season_olsfc[i])
      
      for (k in seq_along(lagnames_olsfc))
        newdata_olsfc[[lagnames_olsfc[k]]] <- tail(x, k)[1]
      
      fc_olsfc[i] <- predict(fit_olsfc, newdata = newdata_olsfc)
      
      if (maxlag > 0) {
        newdata_olsfc[lagnames_olsfc[seq(maxlag)]] <- c(fc_olsfc[i], newdata_olsfc[lagnames_olsfc[1:(maxlag - 1)]])
      }
      
      newdata_olsfc[['trend1']] <- trend1[i + 1]
      newdata_olsfc[['trend2']] <- trend2[i + 1]
      newdata_olsfc[['season']] <- season_olsfc[i + 1]
    }
    
    return(list(fc_olsfc, fitted(fit_olsfc)))
  }
}
