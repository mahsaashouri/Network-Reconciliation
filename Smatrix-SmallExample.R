
library(tidyverse)
## Generating sample series
A.B <- round(runif(10),2)
A.C <- round(runif(10),2)
B.A <- round(runif(10),2)
B.C <- round(runif(10),2)
C.A <- round(runif(10),2)
C.B <- round(runif(10),2)
O.A <- round(runif(10),2)
O.B <- round(runif(10),2)
O.C <- round(runif(10),2)

data.all <- dplyr::bind_rows(list(A.B = A.B, A.C = A.C, B.A = B.A, B.C = B.C, C.A = C.A, C.B = C.B, 
                                  O.A = O.A, O.B = O.B, O.C = O.C), .id = 'id')
data.all <- reshape2::melt(data.all)
colnames(data.all) <- c('cat', 'series')

number.row <- 1+1+ifelse(length((filter(data.all, sub("\\..$", "", data.all$cat)=='O'))$cat)!=0, 1, 0)+
         ifelse(sum(unique(sub("\\..$", "", data.all$cat)) %in% "O"),length(unique(sub("\\..$", "", data.all$cat)))-1, 
         length(unique(sub("\\..$", "", data.all$cat)))) + length(unique(sub("*..", "", data.all$cat))) + 
         length(unique(data.all$cat))

smatrix <- matrix(NA, ncol = length(unique(data.all$cat)), nrow = number.row)

# total IN 
smatrix[1,] <- rep(1, ncol(smatrix))
# total OUT
smatrix[2,] <- c(rep(1, ncol(smatrix)-length(unique(filter(data.all, sub("\\..$", "", data.all$cat)=='O')$cat))), 
                 rep(0, length(unique(filter(data.all, sub("\\..$", "", data.all$cat)=='O')$cat))))
# total Outer series (other)
smatrix[3,] <- c(rep(0, ncol(smatrix)-length(unique(filter(data.all, sub("\\..$", "", data.all$cat)=='O')$cat))), 
                 rep(1, length(unique(filter(data.all, sub("\\..$", "", data.all$cat)=='O')$cat))))
# IN series
no.in.series <- ifelse(sum(unique(sub("\\..$", "", data.all$cat)) %in% "O"),length(unique(sub("\\..$", "", data.all$cat)))-1, 
                       length(unique(sub("\\..$", "", data.all$cat))))
for(i in 1:no.in.series){
  s.in <- unique(data.all$cat)
  s.in.q <- unique(sub("*..", "", s.in))
  smatrix[3+i,] <- ifelse(sub("*..", "", s.in) %in% s.in.q[i], 1, 0)
}

# OUT series
no.out.series <- length(unique(sub("*..", "", data.all$cat)))
for(i in 1:no.out.series){
  s.out <- unique(data.all$cat)
  s.out.q.1 <- unique(sub("\\..$", "", data.all$cat))
  s.out.q <- s.out.q.1[!s.out.q.1 %in% "O"]
  smatrix[3+no.in.series+i,] <- ifelse(sub("\\..$", "", s.out) %in% s.out.q[i], 1, 0)
}


# Bottom level series

smatrix[(3+no.in.series+no.out.series+1):number.row,] <- diag(1, length(unique(data.all$cat)))
