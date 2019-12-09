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
get_sentiments(lexicon = 'nrc')

# sentiment analysis using inner join
# sentiment analysis is an inner join operation
# removing stop words is an anti join operation

library(janeaustenr)
library(dplyr)
library(stringr)

# counting sentiments
sentiments %>% group_by(sentiment) %>% count()

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

# joy words using bing sentiment

bing_positive = get_sentiments('bing') %>% 
  filter(sentiment=='positive')
bing_positive

tidy_books %>% filter(book =='Emma') %>% 
  inner_join(bing_positive, by='word') %>%
  count(word, sort = TRUE)

###
library(tidyr)
janeaustensentiment = tidy_books %>% 
  inner_join(get_sentiments('bing'), by='word') %>%
  count(book, index=linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(janeaustensentiment, aes(index, sentiment, fill=book)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~book, ncol = 2, scales = 'free_x')
