# Google closing stock price (GOOG in $USD) on 22 March 2021.
# Google closing stock price (GOOG in $USD) on 24 May 2021.


library(tidyquant)

goog_prices  <- tq_get("GOOG", get = "stock.prices", from = " 2000-01-01")


goog_prices <- goog_prices %>% 
  as_tsibble(index = date)

goog_prices %>% 
  autoplot(close)

goog_prices %>% 
  features(close, unitroot_ndiffs)

goog_prices <-goog_prices  %>% 
mutate(index = row_number()) %>% 
  update_tsibble(index = index) 

train <- goog_prices %>% 
  filter(year(date) < 2020)


fit <- train %>% 
  model(
    arima012011 = ARIMA(close ~ pdq(0, 1,2 ) + PDQ(0,1,1)),
    arima210011 = ARIMA(close ~ pdq(2,1,0) + PDQ(0,1,1)),
    ets = ETS(close ~ error('A') + trend('A') + season('N')),
    holt_ets = ETS(close ~ error('A') + trend('Ad') + season('N')),
    naive = NAIVE(close),
    rw = RW(close ~ drift())) %>% 
  mutate(
    combn = (arima012011 + arima210011 + ets + holt_ets + naive + rw)/6
  )


fit %>% 
  forecast(h = 300) %>% 
  accuracy(data = goog_prices, measures = list(crps = CRPS, rmse =RMSE)) %>% 
  arrange(rmse)
  
fit %>% 
  forecast(h = 300) %>% 
  autoplot(level = NULL) +
  autolayer(goog_prices, close)


fit %>% 
  forecast(h = 300) %>% 
  autoplot() +
  autolayer(goog_prices, close)





forec_fit <- goog_prices %>% 
  model(
    arima012011 = ARIMA(close ~ pdq(0, 1,2 ) + PDQ(0,1,1)),
    arima210011 = ARIMA(close ~ pdq(2,1,0) + PDQ(0,1,1)),
    ets = ETS(close ~ error('A') + trend('A') + season('N')),
    holt_ets = ETS(close ~ error('A') + trend('Ad') + season('N')),
    naive = NAIVE(close),
    rw = RW(close ~ drift())) %>% 
  mutate(
    combn = (arima012011 + arima210011 + ets + holt_ets + naive + rw)/6
  )


forec_googl <- forec_fit %>% 
  forecast(h = 300) 

# Google closing stock price (GOOG in $USD) on 22 March 2021.
# Google closing stock price (GOOG in $USD) on 24 May 2021.

forec_googl %>% 
  filter(index == 4176 | index == 4220) %>% 
  hilo()



forec_fit %>% 
  forecast(h = 300) %>% 
  autoplot() +
  autolayer(goog_prices, close)


### New 

forec_fit <- goog_prices %>% 
  model(
    arima012011 = ARIMA(close ~ pdq(0, 1,2 ) + PDQ(0,1,1)),
    arima210011 = ARIMA(close ~ pdq(2,1,0) + PDQ(0,1,1)),
    ets = ETS(close ~ error('A') + trend('A') + season('N')),
    holt_ets = ETS(close ~ error('A') + trend('Ad') + season('N')),
    naive = NAIVE(close),
    rw = RW(close ~ drift())) %>% 
  mutate(
    combn = (arima012011 + arima210011 + ets + holt_ets + naive + rw)/6
  )




forec_fit %>% 
  forecast(h = 150) %>% 
  autoplot(level = NULL) +
  autolayer(goog_prices, close)


forec_fit %>% 
  forecast(h = 150) %>% 
  filter(index == 4176 ) %>% 
  hilo()


forec_fit %>% 
  forecast(h = 150) %>% 
  filter(index == 4220 ) %>% 
  hilo()


