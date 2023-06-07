library(tm)
library(topicmodels)
library(tidytext)
library(tidyverse)

# Load the Wikipedia Clickstream dataset
data.network.all <- read_csv('SampleClick3000.csv')[,-1]
data.network <- data.network.all[,c('id', 'freq')]
colnames(data.network) <- c('cat', 'series')

char.before <- sub(":.*", "", data.network$cat)
## add extra character at the begining and end of each string
char.before[-length(char.before)] <- paste0(paste0('the', char.before[-length(char.before)]), 'A')

char.after <- sub(".*:", "", data.network$cat)
## add extra character at the begining and end of each string
char.after[-length(char.after)] <- paste0(paste0('the', char.after[-length(char.after)]), 'A')

# Create a corpus from the page titles - curr pages
df <- data.frame(doc_id = c(1:length(unique(char.after))), text = as.character(unique(char.after)), stringsAsFactors = FALSE)

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
             bounds=list(local=c(1,4)))
dtm <- DocumentTermMatrix(corpus, control = control)

#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#test <- rowTotals[rowTotals== 0 ] 

# Perform topic modeling using LDA
k <- 5 # number of topics
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

