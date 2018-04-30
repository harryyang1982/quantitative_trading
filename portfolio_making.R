library(tidyquant)
library(tqk)

tq_get_options()
tq_get("^KS11")
tq_get("^KOSDAQ")

url <- "https://github.com/mrchypark/sejongFinData/raw/master/codeData.csv"
download.file(url, destfile = "./codeData.csv")
codeData <- read_csv("./codeData.csv")

head(codeData)

tar <- str_c(codeData[grep("^삼성전자$", codeData$종목명), 1], ".KS")
tq_get(tar)
tq_get(tar, from = "2016-01-01", to = "2016-05-05")
tq_get(tar, get = "dividends")

code <- code_get()
code

ss_price <- tqk_get(code[grep("삼성전자", code[,2]), 1], from = "2017-01-01")
ss_price

tq_transmute_fun_options() %>% str
tq_transmute_fun_options()$zoo
tq_transmute_fun_options()$xts
tq_transmute_fun_options()$quantmod
tq_transmute_fun_options()$TTR
tq_transmute_fun_options()$PerformanceAnalytics

# draw graphs

data(SHANK)
head(SHANK)

SS <- tqk_get(code[grep("^삼성전자$", code$name), 1], to = "2016-12-31")
NVR <- tqk_get(code[grep("^NHN$", code$name), 1], to = "2016-12-31")
end <- parse_date("2016-12-31")

SS %>% 
  ggplot(aes(date, close)) +
  geom_line() +
  labs(title = "Samsung Line Chart", y = "Closing Price", x = element_blank()) +
  theme_tq()

SS %>% 
  ggplot(aes(date, close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "SamSung Barchart", y = "Closing Price", x = element_blank()) +
  theme_tq()

SS %>% 
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "SamSung Barchart",
       subtitle = "Zoomed in using coord_x_date",
       y = "Closing Price", x = element_blank()) +
  coord_x_date(xlim = c(end - weeks(6), end), 
               ylim = c(1600000, 1800000)) +
  theme_tq()

SS %>% 
  ggplot(aes(x = date, y = close)) +
  geom_barchart(aes(open = open, high = high, low = low, close = close),
                color_up = "darkgreen", color_down = "darkred", size = 1) +
  labs(title = "SamSung Barchart",
       subtitle = "Zoomed in using coord_x_date",
       y = "Closing Price", x = element_blank()) +
  coord_x_date(xlim = c(end - weeks(6), end), 
               ylim = c(1600000, 1800000)) +
  theme_tq()

SS %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "Samsung Candlestick Chart", y = "Closing Price", x = element_blank()) +
  theme_tq()

SS %>% 
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close),
                   color_up = "darkgreen", color_down = "darkred",
                   fill_up = "darkgreen", fill_down = "darkred") +
  labs(title = "Samsung Candlestick Chart", subtitle = "Zoomed in, Experimenting with Formatting",
       y = "Closing Price", x = element_blank()) +
  coord_x_date(xlim = c(end - weeks(6), end),
               ylim = c(1600000, 1800000)) + 
  theme_tq()

start <- end - weeks(6)
SHANK %>% 
  filter(date >= start - days(2 * 15)) %>% 
  ggplot(x = date, y = close, group = symbol) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  labs(title = "SHANK Candlestick Chart",
       subtitle = "Experimenting with Multiple Stocks",
       y = "Closing Price", x = element_blank()) +
  coord_x_date(xlim = c(start, end)) +
  facet_wrap(~symbol, ncol = 2, scales = "free_y") +
  theme_tq()

names(SHANK)

# 60일 / 120일 단순 이동 평균 차트 작성

SS %>% 
  ggplot(aes(date, close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_ma(ma_fun = SMA, n = 60, linetype = 2, size = 1.25) +
  geom_ma(ma_fun = SMA, n = 120, color = "red", size = 1.25) +
  labs(title = "Samsung Candlestick Chart",
       subtitle = "60 and 120-day SMA",
       y = "Closing price", x = element_blank()) +
  coord_x_date(xlim = c(end - weeks(24), end), 
               ylim = c(1500000, 1850000)) +
  theme_tq()

# 60일 / 120일 지수 이동 평균 차트 작성

SS %>% 
  ggplot(aes(date, close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_ma(ma_fun = EMA, n = 60, wilder = T, linetype = 2, size = 1.25) +
  geom_ma(ma_fun = EMA, n = 120, wilder = T, color = "red", size = 1.25) +
  labs(title = "Samsung Candlestick Chart",
       subtitle = "60 and 120-day EMA",
       y = "Closing price", x = element_blank()) +
  coord_x_date(xlim = c(end - weeks(24), end), 
               ylim = c(1500000, 1850000)) +
  theme_tq()

code %>% 
  filter(name %in% str_detect(name, "^KODEX"))
etfs <- code[str_detect(code$name, "^KODEX"),]
