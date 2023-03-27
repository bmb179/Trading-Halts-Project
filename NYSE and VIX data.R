library(tidyverse)
NYSE.halts.historic <- read.csv('https://www.nyse.com/api/trade-halts/historical/download?symbol=&reason=LULD%20pause&sourceExchange=&haltDateFrom=2000-01-01&haltDateTo=2024-01-01') %>% 
  mutate(Halt.Date = as.Date(Halt.Date, '%m/%d/%Y'),
         Resume.Date = as.Date(Resume.Date, '%m/%d/%Y')) %>%
  add_count(Halt.Date) %>% rename(daily.halts = n)
VIX_History <- read.csv('https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv') %>% 
  mutate(DATE = as.Date(DATE, '%m/%d/%Y'))
join1 <- (NYSE.halts.historic %>% select(Halt.Date, daily.halts) %>% distinct()) %>% inner_join(VIX_History, by = c('Halt.Date' = 'DATE'))

NYSE.halts.historic %>% filter(Symbol == 'FRC' | Symbol == 'SBNY' | Symbol == 'WAL' | Symbol == 'SIVB') %>% count()

NYSE.halts.historic %>% select(Halt.Date, daily.halts) %>% distinct() %>% filter(daily.halts > 100)

cor(x = join1$daily.halts, y = join1$HIGH)

blog_theme <- theme(
  plot.background = element_rect(fill = "#14101b", color = "#14101b"),
  panel.background = element_rect(fill = "#14101b", color = "#14101b"),
  legend.background = element_rect(fill = "#14101b", color = "#14101b"),
  legend.title = element_text(size = 15, family = "serif", color = "White", face = "bold"),
  legend.text = element_text(size = 15, family = "serif", color = "White"),
  axis.title = element_text(size = 15, family = "serif", color = "White", face = "bold"),
  axis.text = element_text(size = 15, family = "serif", color = "White"),
  plot.title= element_text(size = 15, family = "serif", color = "White", face = "bold"),
  plot.subtitle= element_text(size = 10, family = "serif", color = "White"))

join1 %>% ggplot(aes(daily.halts, HIGH)) + geom_jitter(color = 'light blue') + 
  geom_smooth(method = 'lm') +
  labs(x = 'Number of NYSE/Nasdaq Volatility Halts per Day',
       y = 'Daily High of the CBOE Volatility Index ($VIX)',
       title = 'Relationship Between Volatility Halts and the Daily High of $VIX',
       subtitle = 'Friday 08/23/2019 - Wednesday 03/15/2023') + blog_theme

join1 %>% filter(daily.halts < 100) %>% ggplot(aes(daily.halts, HIGH)) + geom_jitter(color = 'light blue') +
  geom_smooth(method = 'lm') +
  labs(x = 'Number of NYSE/Nasdaq Volatility Halts per Day',
       y = 'Daily High of the CBOE Volatility Index ($VIX)',
       title = 'Relationship Between Volatility Halts and the Daily High of $VIX',
       subtitle = 'Friday 08/23/2019 - Wednesday 03/15/2023 | Excluding Days with Over 100 Volatility Halts') + blog_theme