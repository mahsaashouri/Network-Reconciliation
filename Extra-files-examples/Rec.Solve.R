matrixInversion <- function(M){
  if (nrow(M) == 1){
    return(1/M)
  } else if (nrow(M) == 2){
    a <- M[1,1]
    b <- M[1,2]
    c <- M[2,1]
    d <- M[2,2]
    deter <- ((a*d)-(b*c))
    return((1/deter)*matrix(c(d,-c,-b,a),nrow=2,ncol=2))
  } else {
    x <- (floor(nrow(M) / 2))
    A <- M[1:x, 1:x, drop=F]
    B <- M[1:x, -1:-x, drop=F]
    C <- M[-1:-x, 1:x, drop=F]
    D <- M[-1:-x, -1:-x, drop=F]
    if(x>5000){
      Ainv <- matrixInversion(A)
      common <- matrixInversion(D - C %*% Ainv %*% B)
      newA <- Ainv+Ainv%*%B%*%common%*%C%*%Ainv
      newB <- (-Ainv)%*%B%*%common
      newC <- (-common)%*%C%*%Ainv
      newD <- common
    }
    else{
      Ainv <- solve(A)
      common <- solve(D - C %*% Ainv %*% B)
      newA <- Ainv+Ainv%*%B%*%common%*%C%*%Ainv
      newB <- (-Ainv)%*%B%*%common
      newC <- (-common)%*%C%*%Ainv
      newD <- common 
    }
    return(cbind(rbind(newA, newC), rbind(newB, newD)))
  }
}

## random seed not relevant here ...
m1 <- matrix(runif(20000^2), nr=20000)
matrixInversion(m1)
all.equal(solve(m1), matrixInversion(m1))
# [1] TRUE
any(is.nan(matrixInversion(m1))) # based on your comment
# [1] FALSE

m2 <- matrix(runif(151^2), nr=151)
all.equal(solve(m2), matrixInversion(m2))
# [1] TRUE
any(is.nan(matrixInversion(m2)))
