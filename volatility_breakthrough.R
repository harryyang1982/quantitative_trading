library(tqk)
library(tidyquant)

kosdaq_leverage <- tqk_get(233740)

breakthroughout <- kosdaq_leverage %>% 
  mutate(range = high - low,
         noise = 1 - abs(open - close) / (high - low),
         range_position = (close - low) / (high - low) * 100) 

bt_20 <- breakthroughout %>% 
  tail(20)

mean(bt_20$range)
