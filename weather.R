# Forecast maximum temperature at Melbourne airport on 12 April 2021.

library(fpp3) # for time-series analysis

# Load Data about maximum temperature in Melbourne Airport.
weather_raw <- readr::read_csv(here::here('data','weather.csv')) %>% 
  janitor::clean_names() %>% 
  select(-product_code, -bureau_of_meteorology_station_number) %>% 
  unite(Date, c("year","month","day"), sep = "-") %>% 
  mutate(Date = ymd(Date)) %>% 
  rename(max_temperature = maximum_temperature_degree_c)

# Convert to tsibble
weather <- weather_raw %>% 
  as_tsibble(index = Date)

## Check for missing valeus
naniar::vis_miss(weather)

# Only few observations from 1970 are missing which we can safely exclude
weather <- weather %>% 
  filter(!is.na(max_temperature)) 

# Time series plot
weather %>% 
  autoplot(max_temperature)

## Plot each year
weather %>% 
  tsibble::fill_gaps() %>% 
  gg_season(max_temperature, period = 'year')

# From a first look in all years the max temperature in April has been 
# between 12 and 34 degrees.

# Let's explore more April values

weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>% 
  as_tibble() %>% 
  summarise(max_all_time = max(max_temperature),
           min_all_time = min(max_temperature))

# Theoretically this could be a good approximation of the 100%  CI
# of maximum temperature in April in Melbourne Airport

# Now let's see a violin-plot of the values colored by year

weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>% 
  mutate(Year = year(Date)) %>% 
  ggplot(aes(Date, max_temperature, )) +
  geom_violin() +
  geom_point(aes(color = as.factor(Year))) +
  theme(
    legend.position = 'none'
  )
  
# Now let's see a violin-plot of the values colored by decade

weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>% 
  mutate(
    Decade = case_when(
      year(Date) >= 2020 ~ '20s',
      year(Date) >= 2010 ~ '10s',
      year(Date) >= 2000 ~ '00s',
      year(Date) >= 1990 ~ '90s',
      year(Date) >= 1980 ~ '80s',
      year(Date) < 1980 ~ '70s') 
    ) %>% 
  ggplot(aes(Date, max_temperature, )) +
  geom_violin() +
  geom_point(aes(color = as.factor(Decade)))

# As wee see the distribution for each decade is right skewed, and
# the outliers of 00's and 10's are not enough to indicate that the
# max temperature will be higher in the coming decade. For the 20's
# we only have observations for one year, so although the max temperature
# of 25 degrees is low we can't attribute any significance to it.

# A visualisation of the distributions of temperature by decade

weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>% 
  mutate(
    Decade = case_when(
      year(Date) >= 2020 ~ '20s',
      year(Date) >= 2010 ~ '10s',
      year(Date) >= 2000 ~ '00s',
      year(Date) >= 1990 ~ '90s',
      year(Date) >= 1980 ~ '80s',
      year(Date) < 1980 ~ '70s') 
  ) %>% 
  ggplot(aes(max_temperature, color = as.factor(Decade))) +
  geom_density(lwd = 2) 


# Now let's see if some values are more frequent by rounding

counts_max_weather <- weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>% 
  as_tibble() %>% 
  transmute(round_max = round(max_temperature)) %>% 
  group_by(round_max) %>% 
  count()

counts_max_weather %>% 
  ggplot(aes(round_max, n)) +
  geom_col()


weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>% 
  ggplot(aes(max_temperature)) +
  geom_histogram()
## Based on this we can assume a normal distribution and let's produce
# the 95% CI

april_weather <- weather %>% 
  filter(month(Date) == 04) %>% 
  select(Date, max_temperature) %>%
  as_tibble()

april_weather %>% 
  summarise(mean_max = mean(max_temperature),
            sd = sd(max_temperature),
            se = sd/sqrt(n()),
            upper_CI = mean_max + 1.96 * se,
            lower_CI = mean_max - 1.96 * se)



all_time_max_min_april <- weather %>% 
  mutate(Year = year(Date)) %>% 
  filter(month(Date) == 04) %>% 
  as_tibble() %>% 
  group_by(Year) %>% 
  summarise(max_max_temp = max(max_temperature, na.rm = F),
            min_max_temp = min(max_temperature, na.rm = F)) %>% 
  pivot_longer(
    cols = -Year, 
    names_to = 'Measurement',
    values_to = 'Temperature'
  ) 

all_time_max_min_april %>% 
  ggplot(aes(Temperature, color = Measurement)) +
  geom_histogram() +
  facet_wrap(~Measurement)

all_time_max_min_april %>% 
  group_by(Measurement) %>% 
  summarise(
    mean = mean(Temperature),
    sd = sd(Temperature),
    se = sd/sqrt(n()),
    upper_CI_80 = mean + 1.282 * se,
    lower_CI_80 = mean - 1.282 * se
  )
  

# We can say that our mean point forecast is the mean of the max 
# and the min temperature. And our 80% CI will combine the lower_CI of 
# the min_max temperature and the upper_CI of the max_max temperature.

# Thus  point forecast : 21.5 , 
# 80 CI [14.3 - 29]



## Now let's see if we can use any models basec on the book

weather %>% 
  filter(month(Date) == 04) %>% 
  features(max_temperature, ljung_box)

# It appears that the time-series is not white noise

# Model 

# First I will train a SNAIVE, ETS, Holt's Dumped Ets, and 
# a combined model to see how well they perform

## I will trasnform the max_temperature using box_cox to avoid negative CI's

lambda <- weather %>%
  features(max_temperature, features = guerrero) 

lambda


fit <-  weather %>% 
  filter(year(Date) >= 1973, # some gaps appear in the data before so we exclude them 
         year(Date) < 2014) %>%  # hold last 6 years out to see how well the model performs
  model(snaive = SNAIVE(box_cox(max_temperature, lambda)),
        ets = ETS(box_cox(max_temperature, lambda) ~ error('A') + trend('A') + season('A')),
        holt_ets = ETS(box_cox(max_temperature, lambda) ~ error('A') + trend('Ad') + season('A')),
        ) %>% 
  mutate(comb = (snaive + ets + holt_ets)/3)



forecasted_weather <- fit %>% 
  forecast::forecast(h = 2626) # days after 2014 in our data

# Let's see which model is more accurate

forecasted_weather %>% 
  accuracy(data = weather,
           measures = list(crps = CRPS, rmse = RMSE))

## Holt's ETS model provides both a better distributional and point forecast.

## Let's visualise the model predictions

forecasted_weather %>% 
  autoplot(lwd = 3) + 
  autolayer(weather, color = 'grey', alpha = 0.3) +
  facet_wrap(~.model, scales = 'free_y') 


## Let's visualise how they perform specifically in april

forecasted_weather %>% 
  filter(month(Date) == 04) %>% 
  autoplot()  +
  autolayer(weather %>% filter(month(Date) == 04), color = 'grey', alpha = 0.3) +
  facet_wrap(~.model, scales = 'free_y') 

## The snaive model produces extremely large CI's


# Let's combine the real values and the predicted
real_vs_forecast <- forecasted_weather %>% 
  hilo() %>% 
  inner_join(weather %>% select(max_temperature), 
             by = 'Date')

real_vs_forecast <- real_vs_forecast %>% 
  rename(predi_distr = max_temperature.x,
         max_temp_actual = max_temperature.y,
         max_temp_forecast = .mean)

tidy_forecast <- real_vs_forecast %>% 
  as_tibble() %>% 
  select(Date, max_temp_forecast, max_temp_actual) %>% 
  pivot_longer(
    cols = -Date, 
    names_to = 'Measurement',
    values_to = 'Temperature') 


tidy_forecast %>%  
  ggplot(aes(Date, Temperature, color = Measurement)) +
  geom_line()

## Our point forecast seems to neglect lower values

# let's see how well it performs in April

tidy_forecast %>% 
  filter(month(Date) == 04) %>% 
  ggplot(aes(Date, Temperature, color = Measurement)) +
  geom_line()

## Again the point forecast is off in colder days.
  
## And finally how well it performs on our day of interest

tidy_forecast %>% 
  filter(month(Date) == 04,
         day(Date) == 12) %>% 
  ggplot(aes(Date, Temperature, color = Measurement)) +
  geom_line()


# As wee see the past 6 years are colder than what our model predicted


# let's see if the 80 CI captures all the volatility for our dat of interest


  

tidy_forecast  %>% 
  filter(month(Date) == 04, day(Date) == 12) %>%
  hilo() 
  View()

  
############  

Predictions
  
  
  future_fit <-  weather %>% 
    filter(year(Date) >= 1973) %>%  # some gaps appear in the data before so we exclude them 
    model(
      holt_ets = ETS(box_cox(max_temperature, lambda) ~ error('A') + trend('Ad') + season('A'))
    )

  
  
forecasted_weather_april <- future_fit %>% 
    forecast::forecast(h = 49) # days after 2014 in our data
  
  
forecasted_weather_april %>% 
  filter(month(Date) == 04, day(Date) == 12) %>% 
  hilo(c( 60, 70, 80))


forecasted_weather_april %>% 
  autoplot(color = 'red') +
  autolayer(weather, max_temperature)





# max_all_time min_all_time
# <dbl>        <dbl>
#   1         34.5         11.7


# holt_ets  
#23.4 
#[15.79395, 29.87698]60 
#[14.8807, 32.80911]70 
#[13.84853, 37.22497]80


# Thus  point forecast : 21.5 , 
# 80 CI [14.3 - 29]

 

