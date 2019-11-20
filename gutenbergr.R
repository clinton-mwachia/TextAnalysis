# gutenbergr is used o download books from the internet
library(gutenbergr)
library(tidyverse)
library(tidytext)

hgwells = gutenberg_download(c(35, 36, 5230, 152))
hgwells
# tokenizing and removing stop words
tidy_hgwells = hgwells %>%  unnest_tokens(word,text) %>% anti_join(stop_words)
tidy_hgwells

# most common words
tidy_hgwells %>% count(word, sort = TRUE)

# combining the word frequencies of hg wells, jane austen and bronte sisters
bronte = gutenberg_download(c(1260, 768, 969, 767))

tidy_bronte = bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)

library(tidyr)
library(janeaustenr)
library(stringr)

# jane austein books
original_books <- austen_books() %>%
  # adding two new columns linenumber and chapter
  group_by(book) %>% mutate(linenumber = row_number(),
                            chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                                    ignore_case = TRUE)))) %>%  ungroup()

#
tidy_books = original_books %>%
  unnest_tokens(output = word, input = text) %>% anti_join(stop_words)
frequency = bind_rows(mutate(tidy_bronte, author = 'Bronte Sisters'),
                      mutate(tidy_hgwells, author = 'H.G. Wells'),
                      mutate(tidy_books, author = 'Jane Austen')) %>% 
  mutate(word = str_extract(word, "[a-z]+")) %>% count(author, word) %>%
  group_by(author) %>% 
  mutate(proportion = n/sum(n)) %>% 
  select(-n) %>%
  spread(author, proportion) %>% 
  gather(author, proportion, `Bronte Sisters`: `H.G. Wells`)
frequency
