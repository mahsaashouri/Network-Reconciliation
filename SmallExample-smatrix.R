
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

## calculate aggregates
# total series
Total <- data.all %>%
group_split(cat) %>%
  map(~.[['series']]) %>%
  reduce(`+`)
# outer series

Outer <- data.all %>%
  mutate( 'prev.id'= sub(".$", "", cat)) %>%
  group_split(cat) 


#test <- unique(sub(".$", "", unique(data.all$cat) ) )




