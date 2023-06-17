## scale data
scale_data <- function(train, test, feature_range = c(0, 1)) {
  scaled_train <- data.frame(matrix(NA, nrow = nrow(train), ncol = ncol(train)))
  scaled_test <- data.frame(matrix(NA, nrow = nrow(test), ncol = ncol(test)))
  
  for (i in 1:ncol(train)) {
    x <- train[, i]
    fr_min <- feature_range[1]
    fr_max <- feature_range[2]
    
    std_train <- (x - min(x)) / (max(x) - min(x))
    std_test <- (test[, i] - min(x)) / (max(x) - min(x))
    
    scaled_train[, i] <- std_train * (fr_max - fr_min) + fr_min
    scaled_test[, i] <- std_test * (fr_max - fr_min) + fr_min
  }
  
  return(list(scaled_train = scaled_train, scaled_test = scaled_test, scaler = apply(train, 2, function(x) c(min = min(x), max = max(x)))))
}