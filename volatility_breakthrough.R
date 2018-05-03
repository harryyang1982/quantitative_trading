library(tqk)
library(tidyquant)

kosdaq_leverage <- tqk_get(233740)

breakthroughout <- kosdaq_leverage %>% 
  mutate(range = high - low,
         noise = 1 - abs(open - close) / (high - low),
         range_position = (close - low) / (high - low) * 100) 

bt_20 <- breakthroughout %>% 
  tail(20)

bt_10 <- breakthroughout %>% 
  tail(10)

summary(bt_20)
summary(bt_10)

mean(bt_20$range)
mean(bt_10$range)
mean(bt_20$noise)
mean(bt_10$noise)

solution <- mean(bt_10$range) * mean(bt_10$noise)

# bt_10_new <- bt_10 %>% 
#   bind_rows(tibble(date = as.Date("2018-05-03"), open = 20550, high = 20840, low = 20270, close = 20270, volume = 5679295,
#             adjusted = 20270, range = NA, noise = NA,
#             range_position = NA)) %>% 
#   mutate(range = high - low,
#          noise = 1 - abs(open - close) / (high - low),
#          range_position = (close - low) / (high - low) * 100)
# 
# solution_new <- mean(bt_10_new$range) * mean(bt_10_new$noise)
