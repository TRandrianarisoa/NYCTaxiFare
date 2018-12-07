#############################
######## Weather Data
#############################

###############Initialisation 
rm(list=objects())

###############packages
library(riem)
library(tidyverse)

# networks
networks <- riem_networks()
networks %>% filter(grepl("York", name)) # use the code NY_ASOS

# stations
stations <- riem_stations(network='NY_ASOS')

# Weather Data

riem_measures(station='ALB',
              date_start = "2014-01-01",
              date_end = "2014-01-02"
              )