#########################################
######## First model - benchmark ########
#########################################

####### Initializatin #######
rm(list=objects())
getwd()

library(padr)
library(plyr)
library(dplyr)

library(plotly)
Sys.setenv("plotly_username"="ThibaultR7")
Sys.setenv("plotly_api_key"="OQTkqSvSm8GdBkldp1jY")
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidGhpYmF1bHRyIiwiYSI6ImNqb3llNWJubjBhbTIzcXA1MWtucWw5b2EifQ.NvXNEjdCyHl2-Au3CnhPvQ')

library(riem)
library(tidyverse)

# networks
networks <- riem_networks()
networks %>% filter(grepl("York", name)) # use the code NY_ASOS

# stations
stations <- riem_stations(network='NY_ASOS')

####### Data handling #######

# creating the file
library(data.table)
Data <- fread(
  file="Data/train.csv",
  nrows=1000
)

# Resampling
Data <- Data[,c("key","fare_amount")]
Data$key=as.POSIXct(Data$key, format="%Y-%m-%d %H:%M:%S")  # char to datetime
head(Data)

train <- Data %>% thicken("hour") %>% group_by(key_hour) %>% summarise(median(fare_amount))  # resampling
train <- plyr::rename(train,c("key_hour"="key","median(fare_amount)"="fare_amount"))

#train <- Data %>% thicken("hour") %>% group_by(key_hour) %>% summarise(mean(fare_amount))  # resampling
#train <- plyr::rename(train,c("key_hour"="key","mean(fare_amount)"="fare_amount"))
train[is.na(train)] <- tail(train$key, n=2)[1] + 3600

head(train)
tail(train)

# Weather Data

weather_data <- riem_measures(station='NYC',
                              date_start = "2009-01-01",
                              date_end = "2009-07-01"
)[, c('valid', 'tmpf', 'dwpf', 'relh', 'sknt', 'p01i', 'alti', 'mslp', 'vsby')]
weather_data <- rename(weather_data, key=valid)
cl <-  class(weather_data$key)
weather_data$key <- sapply(weather_data$key, round_date, unit="1 hour") - 3600
class(weather_data$key) <- cl  # gety a double column otherwise
train <- left_join(train, weather_data, by=c("key"))

head(train)
tail(train)

limit <- as.POSIXct("2015-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")

h <- nrow(train[train$key>=limit,])  # the horizon of prediction
test_aux <- train[train$key>=limit,]  # test for the time series prediction
train <- train[train$key<limit,]  # train for times series prediction
test <- Data[Data$key>=limit,]  # test on the whole data

# Fare plot
par(mfrow=c(1,1))

p <- plot_ly(x = ~train$key, y = ~train$fare_amount, mode = 'lines') %>%
  layout(yaxis=list(title='Fare amount') , xaxis=list(title='Date'))
p

# One can observe a change in the mean fare in late 2012. This is due to a new law in New York that took effect on the 4th of September 2012.
# For the remaining of the study, one set the whole prices on the same basis.

mean_before <- mean(train[train$key<as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"),][['fare_amount']])
mean_after <- mean(train[train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"),][['fare_amount']])
price_inc <- mean_after-mean_before  # mean price increase
(mean_after-mean_before)/mean_before


train[train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"), 'fare_amount'] <- train[train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"), 'fare_amount'] - price_inc
p <- plot_ly(x = ~train$key, y = ~train$fare_amount, mode = 'lines') %>%
  layout(yaxis=list(title='Fare amount') , xaxis=list(title='Date'))
p

# Autocorrelation plot
acf(train$fare_amount, lag.max = 500, main="Autocorrelation")
train[, c('key', 'fare_amount')] %>% thicken("year") %>% group_by(key_year) %>% summarise(mean(fare_amount))


####### Model fitting #######

library(forecast)

fare_ts <- ts(train[,2], freq=24, start=0)

# tbats fitting
faretbats <- tbats(fare_ts, num.cores = NULL)
fc2 <- forecast(faretbats, h=h)
plot(fc2)

# results analysis
train$fitted <- fc2$fitted
sqrt(mean((train$fitted-train$fare_amount)**2))
1 - (sum((train$fitted-train$fare_amount )^2)/sum((train$fare_amount-mean(train$fare_amount))^2))  # r2 on medians
p <- plot_ly(x = ~train$key, y = ~train$fare_amount, name = 'Actual medians', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 1)) %>%
  add_trace(y = ~train$fitted, name = 'TBATS fit', line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
  layout(title = "Model fitting",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Fare"))
p


test_aux$forecasts <- fc2$mean + price_inc
sqrt(mean((test_aux$forecasts-test_aux$fare_amount)**2))
1 - (sum((test_aux$forecasts-test_aux$fare_amount )^2)/sum((test_aux$fare_amount-mean(test_aux$fare_amount))^2))
p <- plot_ly(x = ~test_aux$key, y = ~test_aux$fare_amount, name = 'Actual medians', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 1)) %>%
  add_trace(y = ~test_aux$forecasts, name = 'TBATS pred', line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
  layout(title = "Model fitting",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Fare"))
p

# on the real data
library(lubridate)
test$time <- test$key
test$key <- floor_date(test$key, "hour")
newData <- left_join(test, test_aux, by=c("key"))

sapply(newData, function(x) sum(is.na(x)))  # no missing values

attr(newData$forecasts, 'tsp') <- NULL  # issue with tsp attribute following the left_join
sqrt(mean(((newData$forecasts-newData$fare_amount.x)**2)))
1 - (sum((newData$forecasts-newData$fare_amount.x)^2)/sum((newData$fare_amount.x-mean(newData$fare_amount.x))^2))

sqrt(mean(((newData$fare_amount.y-newData$fare_amount.x)**2)))
1 - (sum((newData$fare_amount.y-newData$fare_amount.x)^2)/sum((newData$fare_amount.x-mean(newData$fare_amount.x))^2))




