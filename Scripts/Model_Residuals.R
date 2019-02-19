####### Cross-validation #######


rm(list=objects())

library(lubridate)
library(DBI)
library(glue)
library(pracma)
library(plyr)
library(dplyr)
library(data.table)
library(padr)
library(doParallel)
library(caret)


####### Data #######

library(data.table)
train_c <- fread(
  file="Data/train_res.csv",
  #  nrows=1000
)

test_c <- fread(
  file="Data/test_res.csv",
  #  nrows=1000
)

train_c$V1 <- test_c$V1 <- NULL

train_c[,-c('fare', 'res_fare', 'fitted')]

train_c$dat=as.POSIXct(train_c$date, format="%Y-%m-%d %H:%M:%S", tz='EST')  # char to datetime
test_c$dat=as.POSIXct(test_c$date, format="%Y-%m-%d %H:%M:%S", tz='EST')  # char to datetime

train_c$hour <- hour(train_c$dat)
train_c$weekday <- as.POSIXlt(train_c$dat)$wday
train_c$weekday <- train_c$weekday + 7 * (train_c$weekday<1) - 1
train_c$hour <- factor(train_c$hour)
train_c$weekday <- factor(train_c$weekday)

test_c$hour <- hour(test_c$dat)
test_c$weekday <- as.POSIXlt(test_c$dat)$wday
test_c$weekday <- test_c$weekday + 7 * (test_c$weekday<1) - 1
test_c$hour <- factor(test_c$hour)
test_c$weekday <- factor(test_c$weekday)

distance_from_coord <- function(row) {
  return(haversine(c(row[1], row[2]), c(row[3], row[4])))
}

train_c$distance <- apply(train_c[, c('ilat', 'ilong', 'olat', 'olong')], 1,
                        distance_from_coord)
test_c$distance <- apply(test_c[, c('ilat', 'ilong', 'olat', 'olong')], 1,
                       distance_from_coord)

test_c <- test_c[order(test_c$dat),]
train_c <- train_c[order(train_c$dat),]

train_c$JFK_pickup <- train_c$JFK_dropoff <- train_c$EWR_pickup <- train_c$EWR_dropoff <- train_c$LaGuardia_pickup <- train_c$LaGuardia_dropoff <-
  test_c$JFK_pickup <- test_c$JFK_dropoff <- test_c$EWR_pickup <- test_c$EWR_dropoff <- test_c$LaGuardia_pickup <- test_c$LaGuardia_dropoff <- 0

# airports
train_c[!(train_c$pickup_longitude<(-73.8352) | train_c$pickup_longitude>(-73.7401) |
          train_c$pickup_latitude<40.619  | train_c$pickup_latitude>40.6659), 'JFK_pickup'] <- 1
train_c[!(train_c$dropoff_longitude<(-73.8352) | train_c$dropoff_longitude>(-73.7401) |
          train_c$dropoff_latitude<40.619  | train_c$dropoff_latitude>40.6659), 'JFK_dropoff'] <- 1
train_c[!(train_c$pickup_longitude<(-74.1925) | train_c$pickup_longitude>(-74.1531) |
          train_c$pickup_latitude<40.6700  | train_c$pickup_latitude>40.7081), 'EWR_pickup'] <- 1
train_c[!(train_c$dropoff_longitude<(-74.1925) | train_c$dropoff_longitude>(-74.1531) |
          train_c$dropoff_latitude<40.6700  | train_c$dropoff_latitude>40.7081), 'EWR_dropoff'] <- 1
train_c[!(train_c$pickup_longitude<(-73.8895) | train_c$pickup_longitude>(-73.8550) |
          train_c$pickup_latitude<40.7664  | train_c$pickup_latitude>40.7931), 'LaGuardia_pickup'] <- 1
train_c[!(train_c$dropoff_longitude<(-73.8895) | train_c$dropoff_longitude>(-73.8550) |
          train_c$dropoff_latitude<40.7664  | train_c$dropoff_latitude>40.7931), 'LaGuardia_dropoff'] <- 1

test_c[!(test_c$pickup_longitude<(-73.8352) | test_c$pickup_longitude>(-73.7401) |
         test_c$pickup_latitude<40.619  | test_c$pickup_latitude>40.6659), 'JFK_pickup'] <- 1
test_c[!(test_c$dropoff_longitude<(-73.8352) | test_c$dropoff_longitude>(-73.7401) |
         test_c$dropoff_latitude<40.619  | test_c$dropoff_latitude>40.6659), 'JFK_dropoff'] <- 1
test_c[!(test_c$pickup_longitude<(-74.1925) | test_c$pickup_longitude>(-74.1531) |
         test_c$pickup_latitude<40.6700  | test_c$pickup_latitude>40.7081), 'EWR_pickup'] <- 1
test_c[!(test_c$dropoff_longitude<(-74.1925) | test_c$dropoff_longitude>(-74.1531) |
         test_c$dropoff_latitude<40.6700  | test_c$dropoff_latitude>40.7081), 'EWR_dropoff'] <- 1
test_c[!(test_c$pickup_longitude<(-73.8895) | test_c$pickup_longitude>(-73.8550) |
         test_c$pickup_latitude<40.7664  | test_c$pickup_latitude>40.7931), 'LaGuardia_pickup'] <- 1
test_c[!(test_c$dropoff_longitude<(-73.8895) | test_c$dropoff_longitude>(-73.8550) |
         test_c$dropoff_latitude<40.7664  | test_c$dropoff_latitude>40.7931), 'LaGuardia_dropoff'] <- 1

train <- train_c[,-c('fare', 'fitted')]
test <- test_c[,-c('fare', 'forecasts')]

####### Cross-Val scheme #######

minDateCV <- as.POSIXct("2009-01-01 00:00:00", format="%Y-%m-%d %H:%M:%S")
maxDateCV <- as.POSIXct("2014-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S")

y = interval(minDateCV, maxDateCV)/years(1)
# cross-val
crossValSpan <- months(3)
testSpan <- months(1)

blocks <- ceil((12*y-lubridate::month(crossValSpan))/lubridate::month(testSpan))

### Creating folds for cross-validation
folds.list.out <- list()
folds.list <- list()

for (i in 1:blocks) {
  trainB <- minDateCV + (i-1)*testSpan
  trainE <- minDateCV + (i-1)*testSpan + crossValSpan
  ValE <- minDateCV + i*testSpan + crossValSpan
  
  folds.list[[i]] <- which((train$dat>=trainB)&(train$dat<trainE))
  folds.list.out[[i]] <- which((train$dat>=trainE)&(train$dat<ValE))
}

#CV

control <- trainControl( 
  index=folds.list,
  indexOut=folds.list.out,
  verboseIter = T,
  returnData = T ,
  savePredictions = T 
) 


####### Test elasticnet #######
grid.gmlnet <- expand.grid(alpha=seq(0,1,length.out=100),
                           lambda=10^seq(-6,-2,.01))
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
elast <- caret::train(res_fare~.,
                      data=train[,-c('date', 'dat')],
                      method="glmnet",
                      trControl=control,
                      tuneGrid=grid.gmlnet,
                      metric='RMSE')
stopCluster(cl)
plot(elast)

getTrainPerf(elast)
RMSE(predict(elast, test), test$res_fare)
importance <- varImp(elast, scale=FALSE)
plot(importance)

elast$bestTune

# residuals vs fitted: res
res = train$res_fare - predict(elast, train)
plot(predict(elast, train), res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')

# residuals vs fitted: whole
res = train_c$fare - predict(elast, train) - train_c$fitted
plot(predict(elast, train) + train_c$fitted, res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')


test_elast <- test_c[ ,c("res_fare", "dat")] %>% arrange(dat) %>% mutate(predict(elast, test))

test_elast[1:1000,] %>%
  gather(key = type, value = value, -dat) %>%
  ggplot() +
  aes(x = dat, y = value, color = type, group = type, ylab = "fare") + 
  geom_line() +
  ggtitle("Random forest with cluster features")



####### Tree #######
grid.arbre <- data.frame(cp=c(1:100/1000))
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
arbre <- caret::train(res_fare~.,
                      data=train[,-c('date', 'dat')],method="rpart",trControl=control,tuneGrid=grid.arbre,metric='Rsquared')
plot(arbre)
stopCluster(cl)
getTrainPerf(arbre)
RMSE(predict(arbre, test), test$res_fare)
importance <- varImp(arbre, scale=FALSE)
plot(importance)

# residuals vs fitted
res = train_c$fare - predict(arbre, train) - train_c$fitted
plot(predict(arbre, train) + train_c$fitted, res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')

####### Adaboost #######
gbmGrid <-  expand.grid(interaction.depth = c(3,5,10), 
                        n.trees = (1:20)*100, 
                        shrinkage = c(0.1,.05),
                        n.minobsinnode = 20)
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
ada <- caret::train(res_fare~.,
                    data=train[,-c('date', 'dat')],
                    method="gbm",
                    trControl=control,
                    tuneGrid=gbmGrid,
                    metric='RMSE')
stopCluster(cl)
plot(ada)

getTrainPerf(ada)
RMSE(predict(ada, test), test$res_fare)
summary(ada)

# residuals vs fitted
res = train_c$fare - predict(ada, train) - train_c$fitted
plot(predict(ada, train) + train_c$fitted, res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')

####### XGBoost #######
grid.xgb <- expand.grid(nrounds=1:5*40,max_depth=c(3,5,8,10,15),
                        eta=c(.1,.4),gamma=c(0.01,.1,1),
                        colsample_bytree=c(.8,1),
                        min_child_weight=10,
                        subsample=c(.3,.5))
cl <- makePSOCKcluster(6)
registerDoParallel(cl)
xgTree <- caret::train(res_fare~.,
                       data=train[,-c('date', 'dat')],method="xgbTree",trControl=control,tuneGrid=grid.xgb,metric='Rsquared')

getTrainPerf(xgTree)
plot(xgTree)
stopCluster(cl)
RMSE(predict(xgTree, test), test$res_fare)
summary(xgTree)
importance <- varImp(ada, scale=FALSE)
plot(importance)

# residuals vs fitted
res = train_c$fare - predict(xgTree, train) - train_c$fitted
plot(predict(xgTree, train) + train_c$fitted, res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')


####### Random Forest #######
library(doMC)
registerDoMC(6)

grid.foret <- expand.grid(mtry=seq(4,ncol(train),5))
foret <- caret::train(res_fare~.,
                      data=train[,-c('date', 'dat')],method="rf",trControl=control,tuneGrid=grid.foret,metric='Rsquared')
plot(foret)

getTrainPerf(foret)
RMSE(predict(foret, test), test$res_fare)
summary(foret)
importance <- varImp(foret, scale=FALSE)
plot(importance)

# residuals vs fitted
res = train_c$fare - predict(foret, train) - train_c$fitted
plot(predict(foret, train) + train_c$fitted, res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')

####### Saving models #######

modeles.bin <- list(foret=foret, elast=elast, arbre=arbre, ada=ada, xgTree=xgTree)
save(modeles.bin,train,test,file="models.RData")



########### Dataframes Aggreg #############

train.agg = data.frame(data=train$date,
                       foret=predict(foret, train),
                       elast=predict(elast, train),
                       arbre=predict(arbre, train),
                       ada=predict(ada, train),
                       xgTree=predict(xgTree, train)
                       )

test.agg = data.frame(data=test$date,
                       foret=predict(foret, test),
                       elast=predict(elast, test),
                       arbre=predict(arbre, test),
                       ada=predict(ada, test),
                       xgTree=predict(xgTree, test)
)

write.csv(train.agg, file = "Data/train.agg.csv")
write.csv(test.agg, file = "Data/test.agg.csv")
