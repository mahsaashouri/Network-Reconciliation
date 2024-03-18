## aggregation function
Aggreg.func.group <- function(data.network){
  ## all the series
  char.after.group <- sub(".*--", "", data.network$cat)
  char.before.t <- sub("::.*", "", char.after.group)
  char.after.t <- sub(".*::", "", char.after.group)
  unique_cat.t <- unique(char.after.group)
  other_cat.t <- unique(char.after.group[char.before.t == 'other'])
  
  ## add the cat column without group
  data.network <- data.network %>%
    mutate('cat2' = char.after.group)
  
  ## group
  char.group <- sub("--.*", "", data.network$cat)
  group.num <- length(unique(char.group))
  
  # total IN 
  TotalIn <- data.network %>%
    group_split(cat2) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalIn <- Matrix(TotalIn, nrow = length(TotalIn), sparse = TRUE)
  colnames(TotalIn) <- 'Total.in'
  if(length(other_cat.t) == length(unique_cat.t)){ ## while flows all come from other node
    TotalOut <- NULL
    Outer <- NULL
  }
  else{
    # total OUT 
    TotalOut <- data.network %>%
      filter(char.before.t!='other') %>%
      group_split(cat2) %>%
      map(~.[['series']]) %>%
      reduce(`+`)
    TotalOut <- Matrix(TotalOut, nrow = length(TotalOut), sparse = TRUE)
    colnames(TotalOut) <- 'Total.out'
    
    # total Outer series (other)
    Outer <- data.network %>%
      filter(char.before.t =='other') %>%
      group_split(cat2) %>%
      map(~.[['series']]) %>%
      reduce(`+`)
    Outer <- Matrix(Outer, nrow = length(Outer), sparse = TRUE)
    colnames(Outer) <- 'Outer'
    
  }
  ## each group as a dataframe in a list
  prefixes <- unique(char.group)
  # Create a list to store dataframes for each prefix
  df_list <- list()
  # Loop through each prefix and filter data
  for (prefix in prefixes) {
    df_list[[prefix]] <-  data.network[grep(paste0("^", prefix),  data.network$cat), , drop = FALSE]
  }
  Matrix.group.in <- Matrix(0, nrow = length(TotalIn), ncol = 0, sparse = TRUE)
  Matrix.group.out <- Matrix(0, nrow = length(TotalIn), ncol = 0, sparse = TRUE)
  Matrix.group.outer <- Matrix(0, nrow = length(TotalIn), ncol = 0, sparse = TRUE)
  for(k in 1:group.num){
    char.after.g <- sub(".*--", "", df_list[[k]]$cat)
    char.before <- sub("::.*", "", char.after.g)
    char.after <- sub(".*::", "", char.after.g)
    unique_cat <- unique(char.after.g)
    other_cat <- unique(char.after.g[char.before == 'other'])
    ## In series - Group
    TotalIn.g <- df_list[[k]] %>%
      group_split(cat) %>%
      map(~.[['series']]) %>%
      reduce(`+`)
    TotalIn.g <- Matrix(TotalIn.g, nrow = length(TotalIn.g), sparse = TRUE)
    colnames(TotalIn.g) <- paste('In.group', k, sep = '.')
    Matrix.group.in <- cbind(Matrix.group.in, TotalIn.g)
    
    if(length(other_cat) == length(unique_cat)){ ## while flows all come from other node
      TotalOut <- NULL
      Outer <- NULL
    }
    else{
      # total OUT 
      TotalOut.g <- df_list[[k]] %>%
        filter(char.before!='other') %>%
        group_split(cat2) %>%
        map(~.[['series']]) %>%
        reduce(`+`)
      TotalOut.g <- Matrix(TotalOut.g, nrow = length(TotalOut.g), sparse = TRUE)
      colnames(TotalOut.g) <- paste('Out.group', k, sep = '.')
      Matrix.group.out <- cbind(Matrix.group.out, TotalOut.g)
      
      # total Outer series (other)
      Outer.g <- df_list[[k]] %>%
        filter(char.before =='other') %>%
        group_split(cat2) %>%
        map(~.[['series']]) %>%
        reduce(`+`)
      Outer.g <- Matrix(Outer.g, nrow = length(Outer.g), sparse = TRUE)
      colnames(Outer.g) <- paste('Outer.group', k, sep = '.')
      Matrix.group.outer <- cbind(Matrix.group.outer, TotalOut.g)
    }
    
  }
  # IN series
  DataIn <- data.network %>%
    mutate('curr.id' = factor(char.after.t, level = unique(char.after.t))) %>%
    group_split(curr.id) 
  
  no.in.series <- length(unique(char.after.t))
  s.in <- unique(char.after.t)
  unique_cat_after <- sub(".*::", "", unique_cat.t)
  
  SumIn <- Matrix(0, nrow = length(TotalIn), ncol = 0, sparse = TRUE)
  NameIn <- c()
  row_count_in <- 0
  for (i in 1:length(DataIn)) {
    name.in <- unique(as.character(DataIn[[i]]$curr.id))
    if (sum(unique_cat_after %in% s.in[i]) > 1) {  # Only include if there are multiple nodes
      row_count_in <- row_count_in + 1
      NameIn <- c(NameIn, name.in)
      SumIn <- cbind(SumIn, DataIn[[i]] %>%
                       group_split(cat2) %>%
                       map(~.[['series']]) %>%
                       reduce(`+`))
    }
  }
  if (ncol(SumIn) > 0) {
    colnames(SumIn) <- paste(NameIn, 'in', sep = '.')
  }
  if(length(other_cat.t) == length(unique_cat.t)){ ## while flows all come from other node
    SumOut <- NULL
  }
  else{
    # OUT series
    DataOut <- data.network %>%
      filter(char.before.t != 'other') %>%
      mutate('prev.id' = factor(sub("::.*", "", cat2), level = unique(sub("::.*", "", cat2)))) %>%
      group_split(prev.id) 
    
    no.out.series <- ifelse(sum(unique(char.before.t) %in% "other"), length(unique(char.before.t)) - 1,
                            length(unique(char.before.t)))
    s.out <- unique(char.before.t)[!unique(char.before.t) %in% "other"]
    unique_cat_before <- sub("::.*", "", unique_cat.t)
    
    SumOut <- Matrix(0, nrow = length(TotalOut), ncol = 0, sparse = TRUE)
    NameOut <- c()
    row_count_out <- 0
    
    for (i in 1:length(DataOut)) {
      name.out <- unique(as.character(DataOut[[i]]$prev.id))
      if (sum(unique_cat_before %in% s.out[i]) > 1) {  # Only include if there are multiple nodes
        row_count_out <- row_count_out + 1
        NameOut <- c(NameOut, name.out)
        SumOut <- cbind(SumOut, DataOut[[i]] %>%
                          group_split(cat2) %>%
                          map(~.[['series']]) %>%
                          reduce(`+`))
      }
    }
    if (ncol(SumOut) > 0) {
      colnames(SumOut) <- paste(NameOut, 'out', sep = '.')
    }
  }
  
  # Bottom level series
  BottomLevel <- Matrix(as.numeric(data.network$series), nrow = nrow(TotalIn), sparse = TRUE)
  colnames(BottomLevel) <- unique(data.network$cat)
  
  ## Final aggregated matrix
  
  AggregMat <- cbind(TotalIn, TotalOut, Outer, Matrix.group.in, Matrix.group.out, Matrix.group.outer, SumIn, SumOut, BottomLevel)
  
  return(AggregMat)
}
