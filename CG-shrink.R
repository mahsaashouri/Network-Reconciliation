
# Target matrix for shrinking towards a diagonal matrix
lowerD <- function(x)
{
  n <- nrow(x)
  return(diag(apply(x, 2, crossprod) / n))
}

shrink.estim <- function(x, tar)
{
  if (is.matrix(x) == TRUE && is.numeric(x) == FALSE)
    stop("The data matrix must be numeric!", call. = FALSE)
  p <- ncol(x)
  n <- nrow(x)
  covm <- crossprod(x) / n
  corm <- cov2cor(covm)
  xs <- scale(x, center = FALSE, scale = sqrt(diag(covm)))
  v <- (1/(n * (n - 1))) * (crossprod(xs^2) - 1/n * (crossprod(xs))^2)
  diag(v) <- 0
  corapn <- cov2cor(tar)
  d <- (corm - corapn)^2
  lambda <- sum(v)/sum(d)
  lambda <- max(min(lambda, 1), 0)
  shrink.cov <- lambda * tar + (1 - lambda) * covm
  return(list(shrink.cov, c("The shrinkage intensity lambda is:",
                            round(lambda, digits = 4))))
}




CG <- function(fcasts, S, weights, allow.changes = FALSE) {
  nts <- nrow(S)
  nbts <- ncol(S)
  nagg <- nts - nbts
  seqagg <- 1L:nagg
  if (!allow.changes) {
    utmat <- cbind2(Matrix::sparseMatrix(i = seqagg, j = seqagg, x = 1),
                    -1 * S[1L:nagg, ])
  } else {
    # Identifying rows with one 1 element to make the Identity matrix in S
    indx <- rowSums(S)
    idx <- tail(which(indx == 1L), nbts) 
    
    # Permulation vector to rearrange rows of S, rows/col of W and forecasts
    pvec <- c(setdiff(1:nts, idx) , idx)
    S2 <- S[pvec, ]
    weights <- weights[pvec, pvec]
    fcasts <- fcasts[pvec, ]
    utmat <- cbind2(Matrix::sparseMatrix(i = seqagg, j = seqagg, x = 1), 
                    -1 * S2[1L:nagg, ])
  }
  jmat <- Matrix::sparseMatrix(i = 1L:nbts, j = (nagg + 1L):nts, x = rep(1L, nbts),
                               dims = c(nbts, nts))
  rhs.l <- as.matrix(utmat %*% t(as.matrix(fcasts)))
  if (is.null(weights)) {
    lhs.l <- utmat %*% t(utmat)
    lin.sol <- as.matrix(cgm_c(lhs.l, rhs.l)) # cgm_c is a C++ function
    p1 <- jmat %*% t(as.matrix(fcasts)) - (jmat %*% t(utmat) %*% lin.sol)
  } else {
    lhs.l <- utmat %*% weights %*% t(utmat)
    lin.sol <- as.matrix(cgm_c(lhs.l, rhs.l))
    p1 <- jmat %*% t(as.matrix(fcasts)) - (jmat %*% weights %*% t(utmat) %*% lin.sol)
  }
  if (!allow.changes) {
    comb <- as.matrix(S %*% p1)
  } else {
    comb <- numeric()
    comb[pvec] <- as.matrix(S2 %*% p1)
  }
  return(comb)
}

cgm_c <- function(As, bs) {
  .Call('_hts_cgm_c', PACKAGE = 'hts', As, bs)
}
