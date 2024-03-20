## Summing matrix Sparse using Matrix package
smatrix.group <- function(data.network) {
  ## all the series
  char.after.group <- sub(".*--", "", data.network$cat)
  char.before.t <- sub("::.*", "", char.after.group)
  char.after.t <- sub(".*::", "", char.after.group)
  unique_cat.t <- unique(char.after.group)
  other_cat.t <- unique(char.after.group[char.before.t == 'other'])
  
  ## group
  char.group <- sub("--.*", "", data.network$cat)
  group.num <- length(unique(char.group))
  # Calculate required dimensions and preallocate memory
  no.in.series <- length(unique(char.after.t))
  s.in <- unique(char.after.t)
  unique_cat_after <- sub(".*::", "", unique_cat.t)
  nrow.in <- 0
  for (i in 1:no.in.series) {
    if (sum(unique_cat_after %in% s.in[i]) > 1) {
      nrow.in <- nrow.in + 1 
    }
  }
if(length(other_cat.t) == length(unique_cat.t)){ ## while flows all come from other node
  if(group.num > 1){
    number.row <- 1 + (group.num*2) + nrow.in + length(unique(char.after.group))
  }
  else{
    number.row <- 1 + nrow.in + length(unique(char.after.group))
  }
}
else{
  no.out.series <- ifelse(sum(unique(char.before.t) %in% "other"), length(unique(char.before.t)) - 1,
                          length(unique(char.before.t)))
  s.out <- unique(char.before.t)[!unique(char.before.t) %in% "other"]
  unique_cat_before <- sub("::.*", "", unique_cat.t)
  nrow.out <- 0
  for (i in 1:no.out.series) {
    if (sum(unique_cat_before %in% s.out[i]) > 1) {
      nrow.out <- nrow.out + 1
    }
  }
  if(group.num > 1){
    number.row <- 1 + 1  + ifelse(length(unique(char.after.group[char.before.t == 'other']))!= 0, 1, 0) + (group.num*3) +
      nrow.in + nrow.out + length(unique(char.after.group))
  }
  else{
    number.row <- 1 + 1 + ifelse(length(unique(char.after.group[char.before.t == 'other'])) != 0, 1, 0) +
      nrow.in + nrow.out + length(unique(char.after.group))
  }
}

# smatrix.network
smatrix.network <- sparseMatrix(
  i = numeric(0),
  j = numeric(0),
  x = numeric(0),
  dims = c(number.row, length(unique(char.after.group)))
)

## each group as a dataframe in a list
prefixes <- unique(char.group)
# Create a list to store dataframes for each prefix
df_list <- list()
# Loop through each prefix and filter data
for (prefix in prefixes) {
  df_list[[prefix]] <-  data.network[grep(paste0("^", prefix),  data.network$cat), , drop = FALSE]
}
    # total OUT
    out.total <- list()
    outer <- list()
    for(k in 1:group.num){
      char.after.g <- sub(".*--", "", df_list[[k]]$cat)
      char.before <- sub("::.*", "", char.after.g)
      char.after <- sub(".*::", "", char.after.g)
      unique_cat <- unique(char.after.g)
      other_cat <- unique(char.after.g[char.before == 'other'])
      
      if(length(other_cat) == length(unique_cat)){ ## while flows all come from other node
        if(group.num > 1)
          h <- 1 + (group.num)
        else
          h <- 1
      ## Total IN
       smatrix.network[1, ] <- 1
      }
      else{
        ## Total IN
        smatrix.network[1, ] <- 1
        ## Total OUT
        out.total[[length(out.total)+1]] <- c(rep(1, length(unique_cat) - length(other_cat)),
                                              rep(0, length(other_cat)))
        ## Total OUTER
        if (sum(unique(char.before) %in% "other") == 1) {
          outer[[length(outer)+1]] <- c(rep(0, length(unique_cat) - length(other_cat)),
                                        rep(1, length(other_cat)))
          if(group.num > 1)
            h <- 3 + (group.num*3)
          else
            h <- 3
        }
        else{
          if(group.num > 1)
            h <- 2 + (group.num*2)
          else
            h <- 2
        }
      }

      if(group.num > 1){
        if(length(other_cat) == length(unique_cat)){ ## while flows all come from other node
          ## Group in
          g.in <- unique(df_list[[k]]$cat)
          smatrix.network[1 + k, ] <- ifelse(unique(data.network$cat) %in% g.in, 1, 0) 
        }
        else{
          ## Group in
          g.in <- unique(df_list[[k]]$cat)
          smatrix.network[3 + k, ] <- ifelse(unique(data.network$cat) %in% g.in, 1, 0)
          ## Group out
          g.out <- unique(df_list[[k]]$cat)
          g.out.t <- g.out[!grepl("other::", g.out)]
          smatrix.network[3 + group.num + k, ] <- ifelse(unique(data.network$cat) %in% g.out.t, 1, 0)
          ## Group outer
          g.outer <- unique(df_list[[k]]$cat)
          g.outer.t <- g.outer[grepl("other::", g.outer)]
          smatrix.network[3 + (2*group.num) + k, ] <- ifelse(unique(data.network$cat) %in% g.outer.t, 1, 0)
        }
      }

    }
    
  if(length(other_cat) != length(unique_cat)){ 
      smatrix.network[2, ] <- as.vector(unlist(out.total))
      smatrix.network[3, ] <- as.vector(unlist(outer))
      }
  # IN series
  if(nrow.in > 0){
    # Initialize a sparse matrix
    no.in.series <- length(unique(char.after.t))
    s.in <- unique(char.after.t)
    unique_cat_after <- sub(".*::", "", unique_cat.t)
    smatrix.network.IN <- sparseMatrix(
      i = numeric(0),
      j = numeric(0),
      x = numeric(0),
      dims = c(nrow.in, length(unique(data.network$cat)))
    )
    row_counter <- 1
    for (i in 1:no.in.series) {
      if (sum(unique_cat_after %in% s.in[i]) > 1) {
        smatrix.network.IN[row_counter, ] <- as.integer(unique_cat_after %in% s.in[i])
        row_counter <- row_counter + 1
      }
    }
    
    ## fix h
    smatrix.network[(h+1):(h+nrow.in), ] <- smatrix.network.IN
  }
  if(length(other_cat.t) == length(unique_cat.t)){ ## while flows all come from other node
    nrow.out <- 0
  }
  else{
    # OUT series
    if(nrow.out>0){
      no.out.series <- ifelse(sum(unique(char.before.t) %in% "other"), length(unique(char.before.t)) - 1,
                              length(unique(char.before.t)))
      s.out <- unique(char.before.t)[!unique(char.before.t) %in% "other"]
      unique_cat_before <- sub("::.*", "", unique_cat.t)
      smatrix.network.OUT <- sparseMatrix(
        i = numeric(0),
        j = numeric(0),
        x = numeric(0),
        dims = c(nrow.out, length(unique(data.network$cat)))
      )
      row_counter <- 1
      for (i in 1:no.out.series) {
        if (sum(unique_cat_before %in% s.out[i]) > 1) {
          smatrix.network.OUT[row_counter, ] <- as.integer(unique_cat_before %in% s.out[i])
          row_counter <- row_counter + 1
        }
      }
      
      smatrix.network[(h+nrow.in +1):(h+nrow.in+nrow.out), ] <- smatrix.network.OUT
    }
  }
  
  # Bottom level series
  smatrix.network[(h + nrow.in + nrow.out + 1):number.row, ] <- sparseMatrix(i = 1:length(unique_cat.t),
                                                                             j = 1:length(unique_cat.t),
                                                                             x = 1,
                                                                             dims = c(length(unique_cat.t), length(unique_cat.t)))
  return(smatrix.network)
}
