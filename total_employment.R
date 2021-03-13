# Forecast The seasonally adjusted estimate of total employment for April 2021. 
# ABS CAT 6202, to be released around mid May 2021.

library(fpp3)
library(patchwork) # combine plots

# Read data
## Not seasonaly adjusted
total_monthly_empl <- readr::read_csv(here::here('data', 'empl_total.csv'))

total_monthly_empl <- total_monthly_empl %>% 
  rename(Date = DATE,
         Employed.People = LFEMTTTTAUM647N) 

total_monthly_empl <- total_monthly_empl %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  filter(year(Date) > 1978) %>% #  for no missing values
  mutate(Employed.People = as.numeric(Employed.People))


# seasonaly adjusted data

total_monthly_empl_seas_adj <- readr::read_csv(here::here('data', 'empl_total_seas_adj.csv'))

total_monthly_empl_seas_adj <- total_monthly_empl_seas_adj %>% 
  rename(Date = DATE,
         Employed.People.Adj = LFEMTTTTAUM647S)

total_monthly_empl_seas_adj <- total_monthly_empl_seas_adj %>% 
  as_tsibble(index = Date) %>% 
  mutate(Date = yearmonth(Date)) %>% 
  filter(year(Date) > 1978) %>% 
  mutate(Employed.People.Adj = as.numeric(Employed.People.Adj))


  

total_monthly_empl %>% 
  autoplot() +
  total_monthly_empl_seas_adj %>% 
  autoplot()

total_monthly_empl_seas_adj %>% 
  mutate(Year = year(Date)) %>% 
  filter(month(Date) == 04) %>% 
  mutate(
    Decade = case_when(
      year(Date) >= 2020 ~ '20s',
      year(Date) >= 2010 ~ '10s',
      year(Date) >= 2000 ~ '00s',
      year(Date) >= 1990 ~ '90s',
      year(Date) >= 1980 ~ '80s',
      year(Date) < 1980 ~ '70s') 
  ) %>% 
  as_tibble() %>% 
  group_by(Decade) %>%
  summarise(min_emplyed_adj = min(Employed.People.Adj),
            max_emplyed_adj = max(Employed.People.Adj),
            mean_emplyed_adj = mean(Employed.People.Adj)) 

  

  

# Employment returns to pre-covid levels

total_monthly_empl %>% 
  gg_season(labels = 'right') +
  total_monthly_empl_seas_adj %>% 
  gg_season(labels = 'right')

## We observe a slight increase in April after March since 2000 except for 2020
total_monthly_empl %>% 
  gg_subseries() +
total_monthly_empl_seas_adj %>% 
  gg_subseries()


total_monthly_empl_seas_adj %>%
  gg_tsdisplay(difference(Employed.People.Adj, 12) %>% 
                 difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

## Significant autocorellation in yearly lag values

total_monthly_empl_seas_adj %>% 
  gg_lag()

library(fable.prophet)

fit <- total_monthly_empl %>% 
  filter(year(Date) > 1978,
         year(Date) < 2012) %>% 
  model(ets_dumped_add = ETS(Employed.People ~ error("A") + trend("Ad") + season("A")),
        ets_dumped_mult = ETS(Employed.People ~ error('A') + trend('Ad') + season("M")),
        prophet = prophet(Employed.People ~ season("year", 4, type = "multiplicative")),
        arima012011 = ARIMA(Employed.People ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima210011 = ARIMA(Employed.People ~ pdq(2,1,0) + PDQ(0,1,1))
  ) %>% 
  mutate(
    comb = (arima012011 + arima210011)/2
  )

empl_forecast <- fit %>% 
  forecast(h = 88) 
  
empl_forecast %>% 
  autoplot(level = NULL) +
  autolayer(total_monthly_empl)

empl_forecast %>% 
  accuracy(data = total_monthly_empl )

empl_forecast %>% 
  accuracy(data = total_monthly_empl, measures= list(crps = CRPS, rmse = RMSE)) %>% 
  arrange(crps)

## Fit only arima models and ets_dumped_mult


future_fit <- total_monthly_empl %>% 
  filter(year(Date) > 1978) %>% 
  model(
        arima012011 = ARIMA(Employed.People ~ pdq(0,1,2) + PDQ(0,1,1)),
        arima210011 = ARIMA(Employed.People ~ pdq(2,1,0) + PDQ(0,1,1)),
        prophet = prophet(Employed.People ~ season("year", 4, type = "multiplicative"))
  ) %>% 
  mutate(
    comb = (arima012011 + arima210011)/2
  )


future_forecast <- future_fit
  forecast(h = 5)
         


future_forecast %>% 
  autoplot(level = NULL) +
  autolayer(total_monthly_empl)

future_forecast %>%
  filter(month(Date) == 04) %>% 
  hilo()



future_fit_adj <- total_monthly_empl_seas_adj %>% 
  filter(year(Date) > 1978) %>% 
  model(
    arima012011 = ARIMA(Employed.People.Adj ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed.People.Adj ~ pdq(2,1,0) + PDQ(0,1,1)),
    prophet = prophet(Employed.People.Adj ~ season("year", 4, type = "multiplicative"))
  ) %>% 
  mutate(
    combn = (arima012011 + arima210011)/2
  )

future_forecast_adj <- future_fit_adj %>% 
  forecast(h = 5)



future_forecast_adj %>% 
  autoplot(level = NULL) +
  autolayer(total_monthly_empl)


  
future_forecast_adj %>% 
  filter(month(Date) == 04) %>% 
  hilo()

future_forecast %>%
  filter(month(Date) == 04) %>% 
  hilo()




