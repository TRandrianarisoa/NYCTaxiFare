####### Cross-validation #######

library(lubridate)
library(DBI)
library(glue)
library(pracma)
library(plyr)
library(dplyr)
library(data.table)
library(padr)

# database connection
ch <- dbConnect(RClickhouse::clickhouse(), host="localhost", dbname="nyctf")

# Data
dbGetQuery(ch, 'SELECT * FROM nycfare LIMIT 1')
dbGetQuery(ch, 'SELECT Count(date) FROM nycfare')

#Data <- dbGetQuery(ch, 'SELECT * FROM rand_nycfare')
Data <- dbGetQuery(ch, 'SELECT * FROM nycfare
                   ORDER BY rand()
                   LIMIT 1000000')
Data$key <- Data$date

# Price shift correction
mean_before <- mean(Data[Data$key<as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"),][['fare']])
mean_after <- mean(Data[Data$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"),][['fare']])
price_inc <- mean_after-mean_before  # mean price increase
price_inc
price_inc/mean_before
Data[Data$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"), 'fare'] <- 
  Data[Data$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"), 'fare'] - price_inc

# Resampled <- Data[,c("key","fare")]
whole<-dbGetQuery(ch, 'SELECT * FROM nycfare')
whole$key <- whole$date
Resampled <- whole[,c("key","fare")]

Resampled <- Resampled %>% thicken("hour") %>% group_by(key_hour) %>% summarise(median(fare))  # resampling
Resampled <- plyr::rename(Resampled,c("key_hour"="key","median(fare)"="fare"))

# get date range
min(Data[, 'date'])
max(Data[, 'date'])

minDateCV <- as.POSIXct("2009-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
maxDateCV <- as.POSIXct("2015-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")

# cross-val
crossValSpan <- months(3)
testSpan <- months(1)

blocks <- ceil((years(6)-crossValSpan)/testSpan)

### Creating folds for cross-validation
DataCV <- Data[Data$key<maxDateCV,]
ResampledCV <- Resampled[Resampled$key<maxDateCV,]

folds.list.out <- list()
folds.list <- list()

for (i in 1:blocks) {
  trainB <- minDateCV + (i-1)*testSpan
  trainE <- minDateCV + (i-1)*testSpan + crossValSpan
  ValE <- minDateCV + i*testSpan + crossValSpan
  
  folds.list[[i]] <- which((DataCV$key>=trainB)&(DataCV$key<trainE))
  folds.list.out[[i]] <- which((DataCV$key>=trainE)&(DataCV$key<ValE))
}


### Get residuals
library(forecast)

fare_ts <- ts(ResampledCV[,2], freq=24, start=0)

# tbats fitting
faretbats <- tbats(fare_ts, num.cores = NULL, use.parallel=TRUE)
fc2 <- forecast(faretbats, h=5*30*24)
plot(fc2)

DataCV$key <- floor_date(DataCV$key, "hour")
ResampledCV$fitted <- fc2$fitted
DataCV <- left_join(DataCV, ResampledCV[,c('key', 'fitted')], by=c("key"))

DataCV$res <- DataCV$fare - DataCV$fitted

# Extend caret
library(caret)

customRF <- list(type = "Regression", library = "ranger", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "num.trees", "min.node.size"),
                                  class = rep("numeric", 3),
                                  label = c("mtry", "num.trees", "min.node.size"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  ranger(x, y, mtry = param$mtry, num.trees=param$num.trees, min.node.size=param$min.node.size, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

#CV

control <- trainControl( 
  index=folds.list,
  indexOut=folds.list.out,
  verboseIter = T,
  returnData = T ,
  savePredictions = T 
) 

tunegrid <- expand.grid(.mtry=c(1:15), .num.trees=c(1000, 1500, 2000, 2500),
                        .min.node.size=c(1000, 1500, 2000, 2500, 5000, 10000))
# rf <- train(res~.,
#             data=DataCV[,!(names(DataCV) %in% c('id', 'fare', 'date', 'meteo_date', 'key', 'fitted'))],
#             method=customRF, metric='RMSE', tuneGrid=tunegrid, trControl=control)
tunegrid <- expand.grid(.mtry=c(1:15),
                        .splitrule = "variance",
                        .min.node.size=c(1000, 1500, 2000, 2500, 5000, 10000))

library(doParallel)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
rf <- train(res~.,
            data=DataCV[,!(names(DataCV) %in% c('id', 'fare', 'date', 'meteo_date', 'key', 'fitted'))],
            method='ranger', metric='RMSE', tuneGrid=tunegrid, trControl=control)
stopCluster(cl)

summary(rf)
plot(rf)
