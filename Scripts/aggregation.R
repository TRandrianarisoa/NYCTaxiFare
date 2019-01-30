library(tidyverse)
library(glmnet)
library(ranger)
library(opera)
library(DBI)

##Data processing

ch <- dbConnect(RClickhouse::clickhouse(), host = "localhost", dbname = "nyctf")
data0 <- dbGetQuery(ch, 'SELECT * FROM nycfare ORDER BY rand() LIMIT 50000')
data0 <- data0 %>% select(-c(ozone, izone, meteo_date, id))

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
  arrange(date) %>%
  filter(fare > 0) %>%
  filter(ilong > bounding_box[1], ilong < bounding_box[2]) %>%
  filter(olong > bounding_box[1], olong < bounding_box[2]) %>%
  filter(ilat > bounding_box[3], ilat < bounding_box[4]) %>%
  filter(olat > bounding_box[3], olat < bounding_box[4]) %>%
  mutate(log_fare = log(fare)) %>% 
  mutate(log_trip_distance = log(sqrt((ilong - olong)^2 + (ilat - olat)^2)),
         trip_distance = sqrt((ilong - olong)^2 + (ilat - olat)^2)) %>% 
  filter(log_trip_distance != -Inf) %>%
  mutate(dist_to_jfk = distance_to_airport(ilong, ilat, olong, olat, jfk),
         dist_to_laguardia = distance_to_airport(ilong, ilat, olong, olat, laguardia),
         dist_to_newark = distance_to_airport(ilong, ilat, olong, olat, newark)) %>%
  mutate(w_day = wday(date), hour = hour(date)) %>%
  drop_na() %>%
  as_tibble()

train <- data1[1:30000, ]
test <- data1[-(1:30000), ]


## Model 1: Lasso regression

X <- as.matrix(train %>% select(-fare, -log_fare, -date, -id) %>% drop_na())

lasso <- glmnet(X, train$log_fare, alpha = 1, lambda = 0.1) 
coef(lasso)

fit <- predict(lasso, as.matrix(test %>% select(-fare, -log_fare, -date, -id))) 

mean((test$fare - exp(fit))^2) %>% sqrt()


test %>% select(date, fare) %>% mutate(fit = exp(fit)) %>% arrange(date) %>%
  head(1000) %>%
  gather (key = type, value = value, -date, -id) %>%
  ggplot() +
  aes(x = date, y = value, color = type, group = type) + 
  geom_line() +
  ggtitle("Lasso")


## First aggregation: aggregates lots of elastic-net
fit <- matrix(rep(rep(0, nrow(test)), 20), nrow(test), 20)

for (i in 1:20) {
  x <- glmnet(X, train$log_fare, alpha = i/20, lambda = 0.1)
  fit[ ,i] <- predict(x, as.matrix(test %>% select(-fare, -log_fare, -date, -id)))
  fit[ ,i] <- exp(fit[ ,i])
}

experts <- fit

mixEWA <- mixture(Y = test$fare, experts = experts, model = 'EWA', loss.type = 'square')
summary(mixEWA)

## Second agg, mix of random forest and lasso
library(ranger)

optimal_rf <- ranger(formula = fare ~ . - log_fare - date - id,
                     data = train,
                     num.trees = 500,
                     mtry = 3,
                     min.node.size = 9,
                     sample.fraction = 0.550,
                     importance = 'impurity')

experts <- matrix(rep(rep(0, nrow(test)), 2), nrow(test), 2)

experts[,1] <- exp(predict(lasso, as.matrix(test %>% select(-fare, -log_fare, -date))))
experts[,2] <- predict(optimal_rf, test  %>% select(-fare, -log_fare, -date, -id))$predictions

mixEWA2 <- mixture(Y = test$fare, experts = experts, model = 'EWA', loss.type = 'square')

data.frame(t = 1:length(mixEWA2$weights), lasso = mixEWA2$weights[,1], rf = mixEWA2$weights[,2]) %>%
  head(1000) %>%
  gather (key = type, value = value, -t) %>%
  ggplot() +
  aes(x = t, y = value, color = type, group = type) + 
  geom_line() +
  ggtitle("weights")




