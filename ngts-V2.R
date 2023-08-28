## aggregation function
Aggreg.func.v2 <- function(data.network){
  char.before <- sub("::.*", "", data.network$cat)
  char.after <- sub(".*::", "", data.network$cat)
  # total IN 
  TotalIn <- data.network %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalIn <- Matrix(TotalIn, nrow = length(TotalIn), sparse = TRUE)
  colnames(TotalIn) <- 'Total.in'
  
  # total OUT 
  TotalOut <- data.network %>%
    filter(char.before!='other') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalOut <- Matrix(TotalOut, nrow = length(TotalOut), sparse = TRUE)
  colnames(TotalOut) <- 'Total.out'
  
  # total Outer series (other)
  Outer <- data.network %>%
    filter(char.before =='other') %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  Outer <- Matrix(Outer, nrow = length(Outer), sparse = TRUE)
  colnames(Outer) <- 'Outer'
  
  # IN series
  DataIn <- data.network %>%
    mutate('curr.id' = factor(char.after, level = unique(char.after))) %>%
    group_split(curr.id) 
  
  SumIn <- Matrix(0, nrow = length(TotalIn), ncol = 0, sparse = TRUE)
  NameIn <- c()
  row_count_in <- 0
  
  for (i in 1:length(DataIn)) {
    name.in <- unique(as.character(DataIn[[i]]$curr.id))
    if (length(name.in) > 1) {  # Only include if there are multiple nodes
      row_count_in <- row_count_in + 1
      NameIn <- c(NameIn, name.in)
      SumIn <- cbind(SumIn, DataIn[[i]] %>%
                       group_split(cat) %>%
                       map(~.[['series']]) %>%
                       reduce(`+`))
    }
  }
  if (ncol(SumIn) > 0) {
  colnames(SumIn) <- paste(NameIn, 'in', sep = '.')
  }
  # OUT series
  DataOut <- data.network %>%
    filter(char.before != 'other') %>%
    mutate('prev.id' = factor(sub("::.*", "", cat), level = unique(sub("::.*", "", cat)))) %>%
    group_split(prev.id) 
  
  SumOut <- Matrix(0, nrow = length(TotalOut), ncol = 0, sparse = TRUE)
  NameOut <- c()
  row_count_out <- 0
  
  for (i in 1:length(DataOut)) {
    name.out <- unique(as.character(DataOut[[i]]$prev.id))
    if (length(name.out) > 1) {  # Only include if there are multiple nodes
      row_count_out <- row_count_out + 1
      NameOut <- c(NameOut, name.out)
      SumOut <- cbind(SumOut, DataOut[[i]] %>%
                        group_split(cat) %>%
                        map(~.[['series']]) %>%
                        reduce(`+`))
    }
  }
  if (ncol(SumOut) > 0) {
  colnames(SumOut) <- paste(NameOut, 'out', sep = '.')
  }
  # Bottom level series
  BottomLevel <- Matrix(as.numeric(data.network$series), nrow = nrow(TotalIn), sparse = TRUE)
  colnames(BottomLevel) <- unique(data.network$cat)
  
  ## Final aggregated matrix
  
  AggregMat <- cbind(TotalIn, TotalOut, Outer, SumIn, SumOut, BottomLevel)
  
  return(AggregMat)
}
