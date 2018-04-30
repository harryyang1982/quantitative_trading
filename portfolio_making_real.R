library(tqk)
library(tidyquant)

momentum_plus <- tqk_get(244620, from = "2017-01-01")
lowvol_plus <- tqk_get(279540, from = "2017-01-01")
us_treasury <- tqk_get(284430, from = "2017-01-01")
value_plus <- tqk_get(244670, from = "2017-01-01")
quality_plus <- tqk_get(244660, from = "2017-01-01")
kosdaq_leverage <- tqk_get(233740, from = "2017-01-01")

momentum_plus <- momentum_plus %>% 
  mutate(symbol = "momentum_plus")
lowvol_plus <- lowvol_plus %>% 
  mutate(symbol = "lowvol_plus")
us_treasury <- us_treasury %>% 
  mutate(symbol = "us_treasury")
value_plus <- value_plus %>% 
  mutate(symbol = "value_plus")
quality_plus <- quality_plus %>% 
  mutate(symbol = "quality_plus")
kosdaq_leverage <- kosdaq_leverage %>% 
  mutate(symbol = "kosdaq_leverage")

make_symbol <- function(df) {
  mutate(df, symbol = deparse(substitute(df)))
}

etfs <- bind_rows(momentum_plus, lowvol_plus, us_treasury, value_plus, quality_plus, kosdaq_leverage)

etfs %>% 
  ggplot(aes(date, close, group = symbol)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  facet_wrap(~symbol)

## date

month(etfs$date)
year(etfs$date)

etfs <- etfs %>% 
  mutate(year_month = str_c(year(etfs$date), month(etfs$date), sep = "-")) %>% 
  mutate(duration = ifelse(symbol == "lowvol_plus", 5, 
                           ifelse(symbol == "us_treasury", 4, 12)))

momentum_score <- etfs %>% 
  filter(symbol != "kosdaq_leverage") %>% 
  group_by(year_month, symbol, duration) %>% 
  summarise(first = min(date), second = max(date), profit = mean(close[date == first]) - mean(close[date == second])) %>% 
  arrange(year_month) %>% 
  filter(profit > 0) %>% 
  group_by(symbol, duration) %>% 
  summarise(momentum_score = n()) %>% 
  mutate(durated_score = momentum_score / duration)

investment <- 6480000 - 300000
sum_score <- sum(momentum_score$durated_score)

portfolio <- momentum_score %>% 
  mutate(portpolio = investment * durated_score / sum_score)
