library(tidyverse)
library(magrittr)
library(readr)
df <- read_csv(file = 'Data/train.csv', n_max = 10000)

df <- df %>% mutate(pickup_datetime = as.Date(as.POSIXct(pickup_datetime)))

courses_par_jours <- df[c("pickup_datetime", "passenger_count")] %>% 
                group_by(pickup_datetime) %>% 
                summarize(pop = max(0,mean(passenger_count))) %>%
                arrange(pickup_datetime)
                  
courses_par_jours_plot <- ggplot(courses_par_jours, aes(x = pickup_datetime, y = pop)) + 
                          geom_line() +
                          ggtitle("Nombres moyens de courses par jours en %")
courses_par_jours_plot  



df_train <- read_csv(file = 'Data/train.csv', n_max = 10000)
df_train <- df_train %>% mutate(pickup_datetime = as.Date(as.POSIXct(pickup_datetime)))

prix_moyen_par_jours <- df_train[c("pickup_datetime", "fare_amount")] %>% 
  group_by(pickup_datetime) %>% 
  summarize(fare = min(fare_amount)) %>%
  arrange(pickup_datetime)

prix_par_jours_plot <- ggplot(prix_moyen_par_jours, aes(x = pickup_datetime, y = fare)) + 
                          geom_line() +
                          ggtitle("prix moyen par jours")
prix_par_jours_plot

