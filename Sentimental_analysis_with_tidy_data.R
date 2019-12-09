# evaluating opinions or emotions in text
library(tidytext)
library(textdata)
# sentiments datasets
sentiments
head(sentiments, 20)
tail(sentiments, 20)
# getting sentiments from the data
get_sentiments(lexicon = 'afinn')
get_sentiments(lexicon = 'bing')
get_sentiments(lexicon = 'loughran')
#get_sentiments(lexicon = 'nrc')

# sentiment analysis using inner join
# sentiment analysis is an inner join operation
# removing stop words is an anti join operation

library(janeaustenr)
library(dplyr)
library(stringr)

# the number of books
austen_books()
austen_books() %>% group_by(book) %>% count()
tidy_books = austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                  ignore_case = TRUE)))) %>%
  ungroup() %>% unnest_tokens(word, text)
tidy_books

# joy words
nrcjoy = get_sentiments('nrc') %>% filter(sentiment =='joy')
