
library(tidyverse)
library(data.table)


Out_lg <- as.data.frame(fread('out_110022_names.all.prev.curr.utf.csv_en_core_web_lg.csv'))
#Out_lg <- Out_lg[-c(1:2),]
## set other group as other
Out_lg[1,'V3'] <- 'other'

click.2023.02 <- as.data.frame(fread("clickstream-enwiki-2023-02.tsv"))
## specify column names
colnames(click.2023.02) <- c('prev', 'curr', 'type', 'freq')

## make all the outflows from one source
click.2023.02  <- data.frame(lapply(click.2023.02, sub, pattern = "other-.*", replacement = "other"))

## add id column
click.2023.02 <- click.2023.02 %>%
  mutate('id' = paste(prev, curr, sep = '::'))

######### one-way
# Find the closest matching values between the two columns
#matches.prev <- stringdist::stringdistmatrix(click.2023.02$prev, Out_lg$`0`, method = "jaccard")
#closest_matches <- apply(matches, 1, which.min)

# Add the represented category from Out_lg to click.2023.02
#click.2023.02$prev.cat <- Out_lg$Categories[closest_matches]
###########

setDT(Out_lg)
setDT(click.2023.02)
### For prev column
# Create an empty column for the category
click.2023.02[, NLP_Cat_prev := NA_character_]
# Create a data.table key for faster joining
setkey(Out_lg, V1)
# Perform a left join and update NLP_Category
click.2023.02[Out_lg, NLP_Cat_prev := i.V3, on = c(prev = "V1")]

### For curr column
# Create an empty column for the category
click.2023.02[, NLP_Cat_curr := NA_character_]
# Create a data.table key for faster joining
setkey(Out_lg, V1)
# Perform a left join and update NLP_Category
click.2023.02[Out_lg, NLP_Cat_curr := i.V3, on = c(curr = "V1")]

## Select only other - product categories and product - product
Sample.product.2023.02.1 <- click.2023.02[click.2023.02$NLP_Cat_prev == "other" & click.2023.02$NLP_Cat_curr == "['PRODUCT']"]
Sample.product.2023.02.2 <- click.2023.02[click.2023.02$NLP_Cat_prev == "['PRODUCT']" & click.2023.02$NLP_Cat_curr == "['PRODUCT']"]
Sample.product.2023.02 <- bind_rows(Sample.product.2023.02.1, Sample.product.2023.02.2)
length(unique(Sample.product.2023.02$id)) ## 41495

write_csv(Sample.product.2023.02[,c(-6,-7)], 'Sample.product.2023.02.csv')


## test

#D1 <- data.frame(Col1 = 1:5, Col2 = c('A1', 'A2', 'A3', 'A4', 'A5'))
#D2 <- data.frame(ColA = c('A1', 'C2', 'A3', 'A4', 'B2', 'A2', 'C20', 'A5', 'A40', 'B20'), 
#                 ColB = c('[1]', '[]', '[5]', '[3]', '[2]', '[]', '[]', '[5]', '[3]', '[2]'))


# Find the closest matching values between the two columns
#matches <- stringdist::stringdistmatrix(D1$Col2, D2$ColA, method = "jaccard")
#closest_matches <- apply(matches, 1, which.min)

# Add the represented category from D2 to D1
#D1$D2_Category <- D2$ColB[closest_matches]


## test - large dataset

#D1 <- data.frame(Col1 = 1:5, Col2 = c('A1', 'A2', 'A3', 'A4', 'A5'))
#D2 <- data.frame(ColA = c('A1', 'C2', 'A3', 'A4', 'B2', 'A2', 'C20', 'A5', 'A40', 'B20'), 
#                ColB = c('[1]', '[]', '[5]', '[3]', '[2]', '[]', '[]', '[5]', '[3]', '[2]'))
#library(data.table)
#library(stringdist)


# Perform fuzzy join and select the closest match
#matches <- stringdist_inner_join(D1, D2, by = c("Col2" = "ColA"), method = "jaccard", distance_col = "dist") %>%
#  group_by(Col2) %>%
#  slice_min(order_by = dist) %>%
#  ungroup() %>%
#  select(Col2, D2_Category = ColB)

# Merge the results with D1
#D1 <- merge(D1, matches, by = "Col2", all.x = FALSE)

#add_category <- function(D1, D2) {
#  D1$D2_Category <- NA_character_  # Create an empty column for the category
  
#  for (i in seq_len(nrow(D1))) {
#    search_value <- D1[i, "Col2"]
#    match_index <- match(search_value, D2$ColA)
    
#    if (!is.na(match_index)) {
#      D1[i, "D2_Category"] <- D2$ColB[match_index]
#    }
#  }
  
#  return(D1)
#}
#result <- add_category(D1, D2)


