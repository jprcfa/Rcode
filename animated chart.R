install.packages("tidyquant")
install.packages("tidyverse")
install.packages("gganimate")


library(gganimate)
library(tidyquant)
library(tidyverse)

price_df <- tq_get(c('DIA', 'SPY', 'QQQ', 'IWM'),
                   from = "2016-11-1",
                   get = "stock.prices")

ret_df <- price_df %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = 'ret') %>%
  mutate(ret = if_else(row_number() == 1, 0, ret)) %>%
  mutate(cr = cumprod(1 + ret) - 1)
#plot static chart.

price_df %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = 'ret') %>%
  mutate(ret = if_else(row_number() == 1, 0, ret)) %>%
  mutate(cr = cumprod(1 + ret) - 1) %>%
  ggplot(aes(x = date, y = cr, group = symbol, color = symbol)) +
  geom_line() +
  geom_point() +
  geom_point(size = 2) + 
  scale_y_continuous(breaks = seq(-0.35,1, 0.1),
                     labels = scales::percent) +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Major Index Returns since Trump\'s Elections in 2016', y = 'Returns (%)') + 
  theme_minimal() 

#animate the chart

p <- ret_df %>%
  ggplot(aes(x = date, y = cr, group = symbol)) +
  geom_line() +
  geom_segment(aes(xend = ymd("2020-03-25"), yend = cr), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = ymd("2020-03-26"), label = symbol), hjust = 0) +
  scale_y_continuous(breaks = seq(-0.35,1, 0.1),
                     labels = scales::percent) +
  transition_reveal(date) +
  coord_cartesian(clip = 'off') + 
  labs(title = 'Major Index Returns since Trump\'s Elections in 2016', x = "Date", y = 'Returns (%)') + 
  theme_minimal() 

animate(p, nframe = 200, end_pause = 20)
anim_save("index_since_Trump_election.gif")
