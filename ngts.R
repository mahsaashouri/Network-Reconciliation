## aggregation function
Aggreg.func <- function(data.network){
  char.before <- sub("::.*", "", data.network$cat)
  char.after <- sub(".*::", "", data.network$cat)
  
  # total IN 
  TotalIn <- data.network %>%
    group_split(cat) %>%
    map(~.[['series']]) %>%
    reduce(`+`)
  TotalIn <- Matrix(TotalIn, nrow = length(TotalIn), sparse = TRUE)
  colnames(TotalIn) <- 'Total.in'
  
  if(length(char.before) == nrow(data.network)){ ## while flows all come from other node
    
    TotalOut <- NULL
    Outer <- NULL
  }
  
else{
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
} 
  # IN series
  DataIn <- data.network %>%
    mutate('curr.id'= factor(char.after, level = unique(char.after))) %>%
    group_split(curr.id) 
  
  SumIn <- Matrix(0, nrow = length(TotalIn), ncol = length(DataIn), sparse = TRUE); NameIn <- c()
  
  for(i in 1:length(DataIn)){
    name.in <- unique(as.character(DataIn[[i]]$curr.id))
    NameIn <- c(NameIn, name.in)
  }
  colnames(SumIn) <- paste(NameIn,'in',sep='.')
  
  for (i in 1:length(DataIn)){
    SumIn[,i] <- DataIn[[i]]%>%
      group_split(cat) %>%
      map(~.[['series']]) %>%
      reduce(`+`)
  }
  
  if(length(char.before) == nrow(data.network)){ ## while flows all come from other node
    SumOut <- NULL
  }
  else{
    # OUT series
    DataOut <- data.network %>%
      filter(char.before!='other') %>%
      mutate( 'prev.id'= factor(sub("::.*", "", cat), level = unique(sub("::.*", "", cat)))) %>%
      group_split(prev.id) 
    
    SumOut <- Matrix(0, nrow = length(TotalOut), ncol = length(DataOut), sparse = TRUE); NameOut <- c()
    
    for(i in 1:length(DataOut)){
      name.out <- unique(as.character(DataOut[[i]]$prev.id))
      NameOut <- c(NameOut, name.out)
    }
    colnames(SumOut) <- paste(NameOut,'out',sep='.')
    
    for (i in 1:length(DataOut)){
      SumOut[,i] <- DataOut[[i]]%>%
        group_split(cat) %>%
        map(~.[['series']]) %>%
        reduce(`+`)
    }
  }
  # Bottom level series
  BottomLevel <- Matrix(as.numeric(data.network$series), nrow = nrow(TotalIn), sparse = TRUE)
  colnames(BottomLevel) <- unique(data.network$cat)
  
  ## Final aggregated matrix
  
  AggregMat <- cbind(TotalIn, TotalOut, Outer, SumIn, SumOut, BottomLevel)
  
  return(AggregMat)
}
