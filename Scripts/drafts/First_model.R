#########################################
######## First model - benchmark ########
#########################################

rm(list=objects())
getwd()
setwd("/Users/thibault/Desktop/Cours\ Master\ 2/1er\ Semestre/Projet\ Data\ Mining")

library(padr)
library(plyr)
library(dplyr)

# Getting the data
MyData <- read.csv(
  "Data/train.csv",
  header=TRUE,
  colClasses=c("key"="character","fare_amount"="numeric",
               "pickup_datetime"="POSIXct",
               "dropoff_longitude"="numeric","pickup_longitude"="numeric",
               "dropoff_latitude"="numeric","pickup_latitude"="numeric",
               "passenger_count"="integer"),
  #nrows=100000
  )

# Resampling
Data <- MyData[,c(1,2)]
Data$key=as.POSIXct(Data$key, format="%Y-%m-%d %H:%M:%S")  # char to datetime

Data <- Data %>% thicken("hour") %>% group_by(key_hour) %>% summarise(median(fare_amount))  # resampling
Data <- plyr::rename(Data,c("key_hour"="key","median(fare_amount)"="fare_amount"))

# Fare plot
par(mfrow=c(1,1))
plot(Data$key, Data$fare_amount, ylab = "Fare", xlab = "Date", type='l', col = 634, fg =139, bg = 139)

# Autocorrelation plot
acf(Data$fare_amount, lag.max = 200,main="Autocorrelation")