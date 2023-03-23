
library(tidyverse)
## reading data
library(data.table)
library(tm)
library(topicmodels)
library(tidytext)


## list of datasets to read
DatasetNames <- c("clickstream-enwiki-2017-11.tsv", "clickstream-enwiki-2017-12.tsv", "clickstream-enwiki-2018-01.tsv",
                  "clickstream-enwiki-2018-02.tsv", "clickstream-enwiki-2018-03.tsv", "clickstream-enwiki-2018-04.tsv", 
                  "clickstream-enwiki-2018-05.tsv", "clickstream-enwiki-2018-06.tsv", "clickstream-enwiki-2018-07.tsv", 
                  "clickstream-enwiki-2018-08.tsv", "clickstream-enwiki-2018-09.tsv", "clickstream-enwiki-2018-10.tsv",
                  "clickstream-enwiki-2018-11.tsv", "clickstream-enwiki-2018-12.tsv", "clickstream-enwiki-2019-01.tsv",
                  "clickstream-enwiki-2019-02.tsv", "clickstream-enwiki-2019-03.tsv", "clickstream-enwiki-2019-04.tsv", 
                  "clickstream-enwiki-2019-05.tsv", "clickstream-enwiki-2019-06.tsv", "clickstream-enwiki-2019-07.tsv", 
                  "clickstream-enwiki-2019-08.tsv", "clickstream-enwiki-2019-09.tsv", "clickstream-enwiki-2019-10.tsv",
                  "clickstream-enwiki-2019-11.tsv", "clickstream-enwiki-2019-12.tsv", "clickstream-enwiki-2020-01.tsv",
                  "clickstream-enwiki-2020-02.tsv", "clickstream-enwiki-2020-03.tsv", "clickstream-enwiki-2020-04.tsv", 
                  "clickstream-enwiki-2020-05.tsv", "clickstream-enwiki-2020-06.tsv", "clickstream-enwiki-2020-07.tsv", 
                  "clickstream-enwiki-2020-08.tsv", "clickstream-enwiki-2020-09.tsv", "clickstream-enwiki-2020-10.tsv",
                  "clickstream-enwiki-2020-11.tsv", "clickstream-enwiki-2020-12.tsv", "clickstream-enwiki-2021-01.tsv",
                  "clickstream-enwiki-2021-02.tsv", "clickstream-enwiki-2021-03.tsv", "clickstream-enwiki-2021-04.tsv", 
                  "clickstream-enwiki-2021-05.tsv", "clickstream-enwiki-2021-06.tsv", "clickstream-enwiki-2021-07.tsv", 
                  "clickstream-enwiki-2021-08.tsv", "clickstream-enwiki-2021-09.tsv", "clickstream-enwiki-2021-10.tsv",
                  "clickstream-enwiki-2021-11.tsv", "clickstream-enwiki-2021-12.tsv", "clickstream-enwiki-2022-01.tsv",
                  "clickstream-enwiki-2022-02.tsv", "clickstream-enwiki-2022-03.tsv", "clickstream-enwiki-2022-04.tsv", 
                  "clickstream-enwiki-2022-05.tsv", "clickstream-enwiki-2022-06.tsv", "clickstream-enwiki-2022-07.tsv", 
                  "clickstream-enwiki-2022-08.tsv", "clickstream-enwiki-2022-09.tsv", "clickstream-enwiki-2022-10.tsv",
                  "clickstream-enwiki-2022-11.tsv", "clickstream-enwiki-2022-12.tsv", "clickstream-enwiki-2023-01.tsv",
                  "clickstream-enwiki-2023-02.tsv")


## for loop to creat samples from big dataframes
prev <- list()
curr <- list()
for(i in 1:length(DatasetNames)){
  ## read dataset
  click.data <- as.data.frame(fread(DatasetNames[i]))
  ## specify column names
  colnames(click.data) <- c('prev', 'curr', 'type', 'freq')
  
  ## make all the outflows from one source
  click.data  <- data.frame(lapply(click.data, sub, pattern = "other-.*", replacement = "other"))
  
  prev[[length(prev)+1]] <- unique(click.data$prev)
  curr[[length(curr)+1]]  <- unique(click.data$curr)
  ## remove the dataframe (better memory usage)
  rm(click.data)
}


name.prev.curr <- unique(c(unlist(prev), unlist(curr)))
write_csv(as.data.frame(name.prev.curr), 'names.all.prev.curr.csv')

## add extra character at the begining and end of each string
name.prev.curr[-length(name.prev.curr)] <- paste0(paste0('the', name.prev.curr[-length(name.prev.curr)]), 'A')

# Create a corpus from the page titles - curr pages
df <- data.frame(doc_id = c(1:length(unique(name.prev.curr))), text = as.character(unique(name.prev.curr)), stringsAsFactors = FALSE)

corpus <- Corpus(DataframeSource(df))

# Preprocess the data
corpus <- corpus %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace) %>%
  tm_map(tolower) %>%
  tm_map(removeWords, stopwords("english"))


# Create a document-term matrix using TF-IDF
control=list(
  removeNumbers=TRUE, 
  removeStopwords=TRUE, 
  removePunctuation=TRUE, 
  bounds=list(local=c(1,10)))
dtm <- DocumentTermMatrix(corpus, control = control)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#test <- rowTotals[rowTotals== 0 ] 

# Perform topic modeling using LDA
k <- 10 # number of topics
lda <- LDA(dtm, k)

# Print the top terms for each topic
terms <- as.data.frame(inspect(dtm)) %>%
  colnames()
topic_terms <- lapply(1:k, function(i) {
  term_freq <- as.numeric(lda@beta[i,])
  names(term_freq) <- terms
  top_terms <- sort(term_freq, decreasing = TRUE)[1:10]
  return(top_terms)
})
names(topic_terms) <- paste0("Topic ", 1:k)
print(topic_terms)

# Get the topic probabilities for each document
doc_topics <- as.data.frame(lda@gamma)
names(doc_topics) <- paste0("Topic", 1:k)
doc_topics$char.after <- unique(char.after)

# Get the index of the highest probability for each document
doc_topics$Category <- apply(doc_topics[, paste0("Topic", 1:k)], 1, which.max)

# Print the data frame
print(doc_topics)


