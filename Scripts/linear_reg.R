library(tidyverse)
library(lubridate)
library(DBI)

## load data

ch <- dbConnect(RClickhouse::clickhouse(), host = "localhost", dbname = "nyctf")
data0 <- dbGetQuery(ch, 'SELECT * FROM nycfare ORDER BY rand() LIMIT 50000')
data0 <- data0 %>% select(-c(ozone, izone, meteo_date))

plot(data0$date, data0$fare)

bounding_box <- c(-74.5, -72.8, 40.5, 41.8)

#Airpots in NYC area
jfk <- c(-73.7822222222, 40.6441666667)
laguardia <- c(-73.8719444444, 40.7747222222)
newark <- c(-74.175, 40.6897222222)  

#Difference between the distance to an airport at pickup and dropoff
distance_to_airport <- function(x1, y1, x2, y2, airport) {
  return(abs((x1 - airport[1])^2 + (y1 - airport[2])^2 - (x2 - airport[1])^2 - (y2 - airport[2])^2))
}

data1 <- data0 %>% 
  filter(fare > 0) %>%
  filter(ilong > bounding_box[1], ilong < bounding_box[2]) %>%
  filter(olong > bounding_box[1], olong < bounding_box[2]) %>%
  filter(ilat > bounding_box[3], ilat < bounding_box[4]) %>%
  filter(olat > bounding_box[3], olat < bounding_box[4]) %>%
  mutate(log_fare = log(fare)) %>%
  drop_na() 

data1 <- data1 %>% select(-c(id, fare))
  
train <- data1[1:30000, ]
test <- data1[-(1:30000), ]

lm0 <- lm(log_fare ~ ., train)
fit <- predict(lm0, test)
  
  
  data2 <- data1 %>% 
mutate(log_trip_distance = log(sqrt((ilong - olong)^2 + (ilat - olat)^2)),
       trip_distance = sqrt((ilong - olong)^2 + (ilat - olat)^2)) %>% 
filter(log_trip_distance != -Inf) %>%
mutate(dist_to_jfk = distance_to_airport(ilong, ilat, olong, olat, jfk),
       dist_to_laguardia = distance_to_airport(ilong, ilat, olong, olat, laguardia),
       dist_to_newark = distance_to_airport(ilong, ilat, olong, olat, newark)) %>%
drop_na() %>%
as_tibble()

  train <- data2[1:30000, ]
  test <- data2[-(1:30000), ]
  
  lm1 <- lm(log_fare ~ ., train)
  fit1 <- predict(lm1, test)









































































































