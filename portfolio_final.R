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

due_date <- etfs %>% 
  filter(date == Sys.Date()-1) %>% 
  select(date, symbol, close)

momentum_score <- etfs %>% 
  filter(symbol != "kosdaq_leverage", date <= "2018-01-31") %>% 
  group_by(year_month, symbol, duration) %>% 
  summarise(close = last(close)) %>% 
  arrange(year_month) %>% 
  left_join(due_date, by = "symbol") %>% 
  mutate(score = close.y - close.x) %>% 
  select(year_month, symbol, duration, score) %>% 
  mutate(momentum_sign = ifelse(score > 0, 1, 0))

momentum_sign <- momentum_score %>% 
  group_by(symbol) %>% 
  summarise(momentum_sign = mean(momentum_sign))

investment <- 6480000 - 300000
bond <- investment * .3

portfolio <- momentum_sign %>% 
  mutate(portfolio = (investment-bond) * momentum_sign / sum(momentum_sign)) %>% 
  mutate(portfolio = ifelse(symbol == "us_treasury", bond, portfolio))

write_csv(portfolio, "portfolio_201803.csv")
