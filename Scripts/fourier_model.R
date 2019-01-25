library(tidyverse)
library(DBI)
library(glue)
library(lubridate)

ch <- dbConnect(RClickhouse::clickhouse(), host = "localhost", dbname = "nyctf")
dbGetQuery(ch, 'SELECT Max(date) as M, Min(date) as m FROM nycfare') -> dates

#Data
d <- dbGetQuery(ch, 'SELECT count(date) FROM nycfare')$`count(date)`

begin <- as.POSIXct("2011-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
end <- as.POSIXct("2011-07-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
req <- glue("select * from nycfare where toDate(date) between '{begin}' and '{end}' limit 1000000")
data0 <- dbGetQuery(ch, req)

data1 <- data0 %>% select(fare, ilat, ilong, olat, olong, date)
names(data1) <- c("fare_amount", "pickup_latitude", "pickup_longitude", "dropoff_latitude", "dropoff_longitude", "date")

data1$date <- (as.numeric(data1$date) - as.numeric(min(data1$date)))/(as.numeric(max(data1$date)) - as.numeric(min(data1$date)))

data_fourier <- data1 %>% select(fare_amount, date)

for (i in 1:21) {
  name1 <- paste0("cos", i)
  name2 <- paste0("sin", i)
  data1 <- data1 %>% mutate(!!name1 := cos(2 * pi * i * date), !!name2 := sin(2 * pi * i * date))
  data_fourier <- data_fourier %>% mutate(!!name1 := cos(2 * pi * i * date), !!name2 := sin(2 * pi * i * date))
}

# Create Training and Test data -
set.seed(100)  
trainingRowIndex <- sample(1:nrow(data1), 0.8*nrow(data1))  
trainingData <- data1[trainingRowIndex, ]
trainingData_f <- data_fourier[trainingRowIndex, ]
testData  <- data1[-trainingRowIndex, ]   
testData_f <- data_fourier[-trainingRowIndex, ]

## Linear regression on the Fourier basis only
lmf <- lm(fare_amount ~ ., trainingData_f)
pred_f <- predict(lmf, testData_f)

summary(lmf)

# stepwise variable selection

step.model <- MASS::stepAIC(lmf, direction = "both", trace = FALSE)
summary(step.model)

#lm(formula = fare_amount ~ date + sin1 + cos2 + sin2 + cos3 + 
#  sin3 + cos4 + cos5 + sin5 + cos6 + sin6 + cos7 + sin7 + cos8 + 
#  sin8 + cos9 + sin9 + cos11 + sin11 + cos12 + cos13 + sin14 + 
#  cos15 + sin15 + cos16 + cos17 + sin17 + cos18 + sin18 + sin19 + 
#  cos20 + sin20 + cos21, data = trainingData_f)

step.model.fwd <- MASS::stepAIC(lmf, direction = "backward", trace = FALSE)
summary(step.model.fwd)

  
#Basic linear regression with all the spatial data and the Fourier basis
lm0 <- lm(fare_amount ~ ., trainingData)
pred <- predict(lm0, testData)

summary(lm0)

















































