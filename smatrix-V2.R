## Summing matrix Sparse using Matrix package
smatrix <- function(data.network) {
  # Preprocess data to avoid redundant computations
  char.before <- sub("::.*", "", data.network$cat)
  char.after <- sub(".*::", "", data.network$cat)
  unique_cat <- unique(data.network$cat)
  other_cat <- unique(filter(data.network, char.before == 'other')$cat)
  
  # Calculate required dimensions and preallocate memory
  number.row <- 1 + 1 + ifelse(length((filter(data.network, char.before == 'other'))$cat) != 0, 1, 0) +
    ifelse(sum(unique(char.before) %in% "other"), length(unique(char.before)) - 1, length(unique(char.before))) +
    length(unique(char.after)) + length(unique(data.network$cat))
  #smatrix.network <- Matrix(0, ncol = length(unique_cat), nrow = number.row, sparse = TRUE)
  smatrix.network <- sparseMatrix(
    i = numeric(0),
    j = numeric(0),
    x = numeric(0),
    dims = c(number.row, length(unique(data.network$cat)))
  )
  
  # total IN
  smatrix.network[1, ] <- 1
  
  # total OUT
  smatrix.network[2, ] <- c(rep(1, ncol(smatrix.network) - length(other_cat)),
                            rep(0, length(other_cat)))
  
  # total Outer series (other)
  if (sum(unique(char.before) %in% "other") == 1) {
    h <- 3
    smatrix.network[h, ] <- c(rep(0, ncol(smatrix.network) - length(other_cat)),
                              rep(1, length(other_cat)))
  } else {
    h <- 0
  }
  
  # IN series
  no.in.series <- length(unique(char.after))
  s.in <- unique(char.after)
  unique_cat_after <-sub(".*::", "", unique_cat)
  for (i in 1:no.in.series) {
    #smatrix.network[h + i, ] <- ifelse(unique_cat_after %in% s.in[i], 1, 0)
    smatrix.network[h + i, ] <- as.integer(unique_cat_after %in% s.in[i])
  }
  
  # OUT series
  no.out.series <- ifelse(sum(unique(char.before) %in% "other"), length(unique(char.before)) - 1,
                          length(unique(char.before)))
  s.out <- unique(char.before)[!unique(char.before) %in% "other"]
  unique_cat_before <- sub("::.*", "", unique_cat)
  for (i in 1:no.out.series) {
    #smatrix.network[h + no.in.series + i, ] <- ifelse(unique_cat_before %in% s.out[i], 1, 0)
    smatrix.network[h + no.in.series + i, ] <- as.integer(unique_cat_before %in% s.out[i])
  }
  # Bottom level series
  #smatrix.network[(h + no.in.series + no.out.series + 1):number.row, ] <- Diagonal(1, length(unique_cat))
  smatrix.network[(h + no.in.series + no.out.series + 1):number.row, ] <- sparseMatrix(i = 1:length(unique_cat),
                                                                                       j = 1:length(unique_cat),
                                                                                       x = 1,
                                                                                       dims = c(length(unique_cat), length(unique_cat)))
  return(smatrix.network)
}
