# Forecast The difference in points (Collingwood minus Essendon) scored in the AFL match
# between Collingwood and Essendon for the Anzac Day clash. 25 April 2021.

library(fitzRoy) # Get the data
library(fpp3) # For time-series analysis

results <- get_match_results()

# Filter to only include matches betewen Collingwood and Essendon

col_ess <- results %>% 
  filter(Home.Team == 'Collingwood' & Away.Team == 'Essendon' | Home.Team == 'Essendon' & Away.Team == 'Collingwood' )

range(col_ess$Date)

# We have data for more than a century. Obviously distant observations are not useful in 
# making predictions. But, the margin of victory in recent years might be an indicator
# of the level of these two teams

col_ess %>% 
  filter(Home.Team == 'Collingwood', year(Date) > 2010) %>% 
  select(Home.Goals, Home.Behinds, Home.Points, 
         Away.Goals, Away.Behinds, Away.Points,
         Margin) %>% 
  summarise_if(is.numeric, max)

col_ess %>% 
  filter(Home.Team == 'Collingwood', year(Date) > 2010) %>% 
  select(Home.Goals, Home.Behinds, Home.Points, 
         Away.Goals, Away.Behinds, Away.Points,
         Margin) %>% 
  summarise_if(is.numeric, min)


col_ess %>% 
  filter(Home.Team == 'Collingwood', year(Date) > 2010) %>% 
  select(Home.Goals, Home.Behinds, Home.Points, 
         Away.Goals, Away.Behinds, Away.Points,
         Margin) %>% 
  summarise_if(is.numeric, mean)


col_ess %>% 
  ggplot(aes(Margin)) +
  geom_density()
 
col_ess %>% 
  ggplot(aes(Margin)) +
  geom_histogram()

col_ess %>% 
  filter(Home.Team == 'Collingwood') %>% 
  summarise(
    mean = mean(Margin),
    sd = sd(Margin),
    se = sd/sqrt(n()),
    upper_CI = mean + 1.282 * se,
    lower_CI = mean - 1.282 * se
    )
    


 col_ess %>% 
  filter(year(Date) > 2010) %>% 
  group_by(Home.Team) %>% 
  ggplot(aes(Date, Margin, group = Home.Team, color = Home.Team)) +
  geom_line() +
  facet_wrap(~Home.Team)
  

col_ess %>% 
  filter(year(Date) > 2010) %>% 
  group_by(Home.Team) %>% 
  summarise(mean_margin = mean(Margin))

series <- col_ess %>% 
  filter(Home.Team == 'Collingwood') %>% 
  as_tsibble(index = Date, key = c(Home.Team, Away.Team)) 

series %>% 
  autoplot(Margin)


series %>% 
  features(Margin , ljung_box)

 
col_ess %>% 
  filter(year(Date) > 2010) %>% 
  as_tsibble(index = Date, key = c(Home.Team, Away.Team)) %>% 
  features(Margin,  quantile)


dt <- col_ess %>% 
  filter(Home.Team == 'Collingwood') %>% 
  mutate(index = row_number()) %>% 
  as_tsibble(index = index, key = c(Home.Team)) 


  dt_md <- dt %>% 
    model(
    `Holt's method` = ETS(Margin ~ error("A") + 
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Margin ~ error("A") + 
                                   trend("Ad") + season("N")),
    `arima210` = ARIMA(Margin ~ pdq(2,1,0))
  ) 
  
  dt_forec <- dt_md %>% 
    forecast(h = 1) 
  
  dt_forec 
  accuracy(dt_md)
  
  dt_forec %>% 
    autoplot(level = NULL) +
    autolayer(dt, Margin)
  
dt_forec  %>% hilo()
  
## A trend might be visible in Total Points per team in the past years



col_games <- results %>% 
  filter(Home.Team == 'Collingwood' | Away.Team == 'Collingwood')

# APG for Col at home
col_games %>% 
  filter(Home.Team == 'Collingwood') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Home.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()

# APG for Col away
col_games %>% 
  filter(Away.Team == 'Collingwood') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Away.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()

# APG against Col at home
col_games %>% 
  filter(Home.Team == 'Collingwood') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Away.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()

# APG against Col away

col_games %>% 
  filter(Away.Team == 'Collingwood') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Home.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()




es_games <- results %>% 
  filter(Home.Team == 'Essendon' | Away.Team == 'Essendon')

# APG for es at home

es_games %>% 
  filter(Home.Team == 'Essendon') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Home.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()


# APG for es away

es_games %>% 
  filter(Away.Team == 'Essendon') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Away.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()


# APG against es at home
col_games %>% 
  filter(Home.Team == 'Essendon') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Away.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()

# APG against es away

col_games %>% 
  filter(Away.Team == 'Essendon') %>% 
  group_by(year(Date)) %>% 
  summarise(APG = mean(Home.Points)) %>% 
  ggplot(aes( `year(Date)`, APG)) +
  geom_line()


### Model points for both teams

col <- results %>% 
  filter(Home.Team == 'Collingwood') %>% 
  mutate(index = row_number()) %>% 
  as_tsibble(index = index, key = c(Home.Team)) 


col_points_fit <- col %>% 
  model(
    `Holt's method` = ETS(Home.Points ~ error("A") + 
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Home.Points ~ error("A") + 
                                   trend("Ad") + season("N")),
    `arima210` = ARIMA(Home.Points ~ pdq(2,1,0))
  ) 


col_points_fit %>% 
  forecast(h = 1) %>% 
  hilo()



es <- results %>% 
  filter(Away.Team == 'Essendon') %>% 
  mutate(index = row_number()) %>% 
  as_tsibble(index = index, key = c(Away.Team)) 


es_points_fit <- es %>% 
  model(
    `Holt's method` = ETS(Away.Points ~ error("A") + 
                            trend("A") + season("N")),
    `Damped Holt's method` = ETS(Away.Points ~ error("A") + 
                                   trend("Ad") + season("N")),
    `arima210` = ARIMA(Away.Points ~ pdq(2,1,0))
  ) 


es_points_fit %>% 
  forecast(h = 1) %>% 
  hilo(60)

# 70.0 [37.177516, 102.88016]80 # essendon
# 70.1 [37.293983, 102.98851]80 # essendon
# 38.9 [ 2.579867,  75.27455]80 #essendon


# 73.2 [38.21028, 108.20028]80# collingwood
# 73.8 [38.85141, 108.78462]80# collingwood
# 58.1 [19.17712,  96.96775]80 # collingwood

# Arima prediction point forecast : 19.2, [16.59725, 21.6932]


stats <- get_afltables_stats(start_date = "2018-01-01", end_date = "2021-01-01")

player_stats_col <- stats %>%
  filter(Playing.for == 'Collingwood') %>% 
  group_by(Season, First.name, Surname) %>% 
  summarise(AGPG = mean(Goals),
            ABPG  = mean(Behinds),
            APPG = AGPG * 6 + ABPG) 

## 3 years average

col_stats_last_3 <- player_stats_col %>% 
  group_by(First.name, Surname) %>% 
  summarise(AGPG3 = mean(AGPG),
            ABPG3  = mean(ABPG),
            APPG3 = ABPG3 * 6 + AGPG3) 

col_stats_last_3  %>% 
  inner_join(player_stats_col %>% filter(Season == 2020),
             by = c('First.name', 'Surname')) %>% 
  ungroup() %>% 
  summarise(team_avg_points =  sum(APPG3))
  

team_stats_col  <-  stats %>%
  filter(Playing.for == 'Collingwood') %>% 
  group_by(Season, First.name, Surname) %>% 
  summarise(AGPG = mean(Goals),
            ABPG  = mean(Behinds),
            APPG = AGPG * 6 + ABPG) %>% 
  group_by(Season) %>% 
  summarise(APPS = sum(APPG))

team_stats_col

# last three years average 121

player_stats_es <- stats %>%
  filter(Playing.for == 'Essendon') %>% 
  group_by(Season, First.name, Surname) %>% 
  summarise(AGPG = mean(Goals),
            ABPG  = mean(Behinds),
            APPG = AGPG * 6 + ABPG) 


## 3 years average

es_stats_last_3 <- player_stats_es %>% 
  group_by(First.name, Surname) %>% 
  summarise(AGPG3 = mean(AGPG),
            ABPG3  = mean(ABPG),
            APPG3 = ABPG3 * 6 + AGPG3) 

es_stats_last_3  %>% 
  inner_join(player_stats_es %>% filter(Season == 2020),
             by = c('First.name', 'Surname')) %>% 
  ungroup() %>% 
  summarise(team_avg_points =  sum(APPG3))




team_stats_es  <-  stats %>%
  filter(Playing.for == 'Essendon') %>% 
  group_by(Season, First.name, Surname) %>% 
  summarise(AGPG = mean(Goals),
            ABPG  = mean(Behinds),
            APPG = AGPG * 6 + ABPG) %>% 
  group_by(Season) %>% 
  summarise(APPS = sum(APPG))

team_stats_es




# last three years average 118.3333

## Average points agains them

stats %>%
  filter(Playing.for != 'Collingwood' &
           (Home.team == 'Collingwood' | Away.team == 'Collingwood')) %>% 
  group_by(Season, Playing.for, First.name, Surname) %>% 
  summarise(GPG = sum(Goals),
            BPG  = sum(Behinds),
            PPG = GPG * 6 + BPG) %>% 
  group_by(Season, Playing.for) %>% 
  summarise(PPS = sum(PPG)) %>% 
  tail(20)


col_games %>% 
  filter(year(Date) >=2018,
         Home.Team == 'Collingwood') %>% 
  group_by(Season, Away.Team) %>% 
  summarise(APFG = mean(Home.Points),
            APAG = mean(Away.Points)) %>% 
  pivot_longer(
    cols = c(APFG, APAG), names_to = 'measure', values_to = 'value'
    )  %>% 
  ungroup() %>% 
  ggplot(aes(value, color = measure)) +
  geom_density()
  
            


## Roster



## Injury List 

# Players who certainly won't play in the game as of today

injured_players <- tribble(
  ~Playing.for, ~First.name, ~Surname,
  'Collingwood' , 'Will' , 'Kelly',
  'Essendon', 'Irving', 'Mosquito',
  'Essendon', 'James', 'Stewart',
  'Essendon', 'Michael', 'Hurley')

injured_players



stats %>%
  filter(Playing.for == 'Collingwood') %>% 
  group_by(Season, First.name, Surname) %>% 
  summarise(AGPG = mean(Goals),
            ABPG  = mean(Behinds),
            APPG = AGPG * 6 + ABPG,
  ) %>% 
  group_by(Season) %>% 
  summarise(APPS = sum(APPG))


stats %>%
  group_by(Playing.for, First.name, Surname) %>% 
  summarise(GPG = mean(Goals, na.rm = T),
            BPG  = mean(Behinds, na.rm = T),
            PPG = GPG * 6 + BPG,
            Tackles = mean(Tackles),
            CP = mean(Contested.Possessions)) %>% 
  inner_join(injured_players, by = c('Playing.for', 'First.name', 'Surname'))



col_ess %>% 
  filter(Home.Team == 'Collingwood', year(Date) > 2017) %>% 
  select(Home.Points, Away.Points)


## Focus on last 10 years since roster will change drastically before,
## along with the trends in how the game is played.

# Calculate stats for Collingwood

# Home stats
col_ess_stats_home <- results %>% 
  filter(year(Date) > 2010,
         Home.Team == 'Collingwood' | Home.Team == 'Essendon' ) %>% 
  group_by(Season, Home.Team) %>% 
  summarise(avg_goals_per_game_for = mean(Home.Goals),
            avg_goals_per_game_against = mean(Away.Goals),
            avg_behinds_per_game_for = mean(Home.Behinds),
            avg_behinds_per_game_against = mean(Away.Behinds),
            avg_points_per_game_for = mean(Home.Points),
            avg_points_per_game_against = mean(Away.Points))





col_ess_stats_home %>% 
  as_tsibble(index = Season, key = Home.Team) %>% 
  autoplot(vars(avg_goals_per_game_for,
                avg_goals_per_game_against,
                avg_behinds_per_game_for,
                avg_behinds_per_game_against,
                avg_points_per_game_for,
                avg_points_per_game_against))



col_ess_stats_away <- results %>% 
  filter(year(Date) > 2010,
         Away.Team == 'Collingwood' | Away.Team == 'Essendon' ) %>% 
  group_by(Season, Away.Team) %>% 
  summarise(avg_goals_per_game_against = mean(Home.Goals),
            avg_goals_per_game_home = mean(Away.Goals),
            avg_behinds_per_game_against = mean(Home.Behinds),
            avg_behinds_per_game_home = mean(Away.Behinds),
            avg_points_per_game_against = mean(Home.Points),
            avg_points_per_game_home = mean(Away.Points))


col_ess_stats_away <- col_ess_stats %>% 
  as_tsibble(index = Season, key = Home.Team)


col_ess_stats_away %>% 
  autoplot(vars(avg_goals_per_game_for,
                avg_goals_per_game_against,
                avg_behinds_per_game_for,
                avg_behinds_per_game_against,
                avg_points_per_game_for,
                avg_points_per_game_against))

# Head to Head

col_vs_ess <- results %>% 
  filter(year(Date) > 2010,
         Home.Team == 'Collingwood' & Away.Team == 'Essendon' |
           Home.Team == 'Essendon' & Away.Team == 'Collingwood' ) %>% 
  group_by(Season, Home.Team) %>% 
  summarise(avg_goals_per_game_for = mean(Home.Goals),
            avg_goals_per_game_against = mean(Away.Goals),
            avg_behinds_per_game_for = mean(Home.Behinds),
            avg_behinds_per_game_against = mean(Away.Behinds),
            avg_points_per_game_for = mean(Home.Points),
            avg_points_per_game_against = mean(Away.Points))


col_vs_ess <- col_vs_ess %>% 
  as_tsibble(index = Season, key = Home.Team)


col_vs_ess %>% 
  autoplot(vars(avg_goals_per_game_for,
                avg_goals_per_game_against,
                avg_behinds_per_game_for,
                avg_behinds_per_game_against,
                avg_points_per_game_for,
                avg_points_per_game_against))
