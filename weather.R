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

## Now let's see if we can use any models base on the book

weather %>% 
  filter(month(Date) == 04) %>% 
  features(max_temperature, ljung_box)

# It appears that the time-series is not white noise

# Model 

# First I will train a SNAIVE, ETS, Holt's Dumped Ets, and 
# a combined model to see how well they perform

fit <-  weather %>% 
  filter(year(Date) >= 1973, # some gaps appear in the data before so we exclude them 
         year(Date) < 2014) %>%  # hold last 6 years out to see how well the model performs
  model(snaive = SNAIVE(max_temperature),
        ets = ETS(max_temperature ~ error('A') + trend('A') + season('A')),
        holt_ets = ETS(max_temperature ~ error('A') + trend('Ad') + season('A')),
        
        )

fit %>% 
  forecast(h = 40) %>% 
  hilo() %>% 
  filter(Date == '2021-04-12')


  
