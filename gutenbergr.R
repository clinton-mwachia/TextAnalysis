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
bronte
tidy_bronte = bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)
tidy_bronte
library(tidyr)
library(janeaustenr)
library(stringr)

# jane austein books
original_books <- austen_books() %>%
  # adding two new columns linenumber and chapter
  group_by(book) %>% mutate(linenumber = row_number(),
                            chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                                    ignore_case = TRUE)))) %>%  ungroup()

original_books
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

# Plotting the frequensies
library(scales)
library(ggplot2)

ggplot(frequency, aes(x=proportion, y=`Jane Austen`,
                      color=abs(`Jane Austen` - proportion))) +  
  geom_abline(color='gray', lty=2) +
  geom_jitter(alpha=0.1, size=2.5, width = 0.3, height = 1.3) + 
  geom_text(aes(label = word), check_overlap = TRUE, vjust=1.5) + 
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits=c(0,0.001),
                       low='darkslategray4',high = 'gray75') +
facet_wrap(~author, ncol = 2) +
  theme(legend.position = 'none') +
  labs(y = "Jane Austen", x = NULL)
  