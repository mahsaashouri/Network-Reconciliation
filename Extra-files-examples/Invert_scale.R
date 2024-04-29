# Function to inverse-transform scaled data
invert_scaling <- function(scaled, scaler, feature_range = c(0, 1)) {
  mins <- feature_range[1]
  maxs <- feature_range[2]
  
  inverted_dfs <- data.frame(matrix(NA, nrow = nrow(scaled), ncol = ncol(scaled)))
  
  for (i in 1:ncol(scaled)) {
    min <- scaler["min", i]
    max <- scaler["max", i]
    X <- (scaled[, i] - mins) / (maxs - mins)
    rawValues <- X * (max - min) + min
    inverted_dfs[, i] <- rawValues
  }
  
  return(inverted_dfs)
}