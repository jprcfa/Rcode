install.packages("pdftools")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("textdata")

library(pdftools)
library(tidyverse)
library(tidyr)         # For data cleaning
library(tidytext)      # For data cleaning of text corpus
library(ggplot2)
library(stringr)
library(textdata)

setwd("O:/Administration/Employee Folders/Jason's Folder/R Data")
files <- list.files() #No parameter necessary now since you're in the proper directory
f <- list()
for (i in 1:length(files)) {
  f[[i]] <- pdf_text(files[i])
}
setwd("O:/Administration/Employee Folders/Jason's Folder/R Data/")

class(f)

for (i in 1:length(f)) {
  f[[i]] <- paste(f[i], collapse = " ")
  if (is.data.frame(str_locate(f[i], "Disclaimer")) == TRUE ) {
    end <- str_locate(f[i], "Disclaimer")
    f[[i]] <- str_sub(f[i], (str_length(f[i] - end[,1]))) }
  else{
    f[[i]] <- str_sub(f[i], end=-3200)
  }
}

Q_df <- tibble(Qcall = seq_along(files), text = f)
Q_df <- Q_df %>%
  mutate(Qcall = files)

#AFin
tidy_letters <- Q_df %>% 
  unnest_tokens(word, text) %>%                           # split text into words
  anti_join(stop_words, by = "word") %>%                  # remove stop words
  filter(!grepl('[0-9]', word)) %>%                       # remove numbers
  left_join(get_sentiments("bing"), by = "word") %>%      # add sentiment scores to words
  group_by(Qcall) %>% 
  mutate(linenumber = row_number(),                       # add line numbers
         sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  ungroup

#Loughran
tidy_letters <- Q_df %>% 
  unnest_tokens(word, text) %>%                               # split text into words
  anti_join(stop_words, by = "word") %>%                      # remove stop words
  filter(!grepl('[0-9]', word)) %>%                           # remove numbers
  left_join(get_sentiments("loughran"), by = "word") %>%      # add sentiment scores to words
  group_by(Qcall) %>% 
  mutate(linenumber = row_number(),                       # add line numbers
         sentiment = ifelse(is.na(sentiment), 'neutral', sentiment)) %>%
  ungroup


tidy_letters %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ sentiment, ncol = 2) +
  ylab("Frequency of this word in Calls")

call_sentiment <- tidy_letters %>%  
  count(Qcall, sentiment) %>%
  spread(key = sentiment, value = n) %>%
  mutate(sentiment_pct = (positive - negative) / (positive + negative + neutral)) %>%
  select(Qcall, sentiment_pct)

#chart from BRK sentiment that isnt working righ now
ggplot(call_sentiment, aes(x = Qcall, y = sentiment_pct)) + 
  geom_bar(aes(fill = sentiment_pct < 0), stat = 'identity') + 
  geom_text(aes(label = call, hjust = ifelse(sentiment_pct >= 0, -0.15, 1.15)), vjust = 0.5) +
  scale_fill_manual(guide = F, values = c('#565b63', '#c40909')) +
  scale_x_reverse(name = '') +
  coord_flip() +
  labs(y='Net Sentiment Ratio',
       title='Text Sentiment of Berkshire Hathaway Letters to Shareholders',
       subtitle='Negative sentiment is strongly associated with recession years',
       caption='michaeltoth.me') + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

#simpler chart that is working
ggplot(call_sentiment, aes(x = Qcall, y = sentiment_pct)) + 
  geom_bar(aes(fill = sentiment_pct < 0), stat = 'identity') 

#intracall analysis
intracall <- tidy_letters %>%
  inner_join(get_sentiments("bing")) %>%
  count(Qcall, index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(data = intracall, mapping = aes(x = index, y = sentiment, fill = Qcall)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(facets = ~ Qcall, ncol = 2, scales = "free_x")

#Word counts  

tidy_letters %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  #filter(n > 5) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip()  
facet_wrap(~ sentiment, ncol = 2) +
  xlab(NULL) +
  
  #top positve and negative words
  
  bing_word_counts <- tidy_letters %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

top_sentiments <- bing_word_counts %>%
  filter(sentiment != 'neutral') %>%
  group_by(sentiment) %>%
  top_n(20, wt = n) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(word = reorder(word, n))

ggplot(top_sentiments, aes(x = word, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sentiment, ncol = 2)

