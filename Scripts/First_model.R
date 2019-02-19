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

library(tidyverse)
library(forecast)


######################################################################
######################### Data Engineering ###########################
######################################################################

# creating the dataframe
library(data.table)
Data <- fread(
  file="Data/train.csv",
#  nrows=1000
)

# Resampling
Data <- Data[,c("key","fare_amount")]
Data$key=as.POSIXct(Data$key, format="%Y-%m-%d %H:%M:%S", tz='EST')  # char to datetime

train <- Data %>% thicken("hour") %>% group_by(key_hour) %>% summarise(median(fare_amount))  # resampling
train <- plyr::rename(train,c("key_hour"="key","median(fare_amount)"="fare_amount"))

#train <- Data %>% thicken("hour") %>% group_by(key_hour) %>% summarise(mean(fare_amount))  # resampling
#train <- plyr::rename(train,c("key_hour"="key","mean(fare_amount)"="fare_amount"))
train[is.na(train)] <- tail(train$key, n=2)[1] + 3600

head(train)
tail(train)

limit <- as.POSIXct("2014-06-01 00:00:00", format="%Y-%m-%d %H:%M:%S", tz='EST')  ## train-test split

h <- nrow(train[train$key>=limit,])  # the horizon of prediction
test_aux <- train[train$key>=limit,]  # test for the time series prediction
train <- train[train$key<limit,]  # train for times series prediction
test <- Data[Data$key>=limit,]  # test on the whole data

##################### Data Preprocessing #####################

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

standard_dev_ratio <- sd(train[train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"),][['fare_amount']])/
  sd(train[train$key<as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"),][['fare_amount']])


train[train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"), 'fare_amount'] <- (train[train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S"), 'fare_amount'] - mean_after)/
  standard_dev_ratio + mean_before
test$fare_amount <- (test$fare_amount - mean_after)/standard_dev_ratio + mean_before
test_aux$fare_amount <- (test_aux$fare_amount - mean_after)/standard_dev_ratio + mean_before

p <- plot_ly(x = ~train$key, y = ~train$fare_amount, mode = 'lines') %>%
  layout(yaxis=list(title='Fare amount') , xaxis=list(title='Date'))
p

# Autocorrelation plot
acf(train$fare_amount, lag.max = 250, main="Autocorrelation")
train[, c('key', 'fare_amount')] %>% thicken("year") %>% group_by(key_year) %>% summarise(mean(fare_amount))

train[which(train$fare_amount==max(train$fare_amount)),]  ## Irene Hurricane 0n 28/08/2011: price increase two hours before
train[which(train$fare_amount==min(train$fare_amount)),]  ## price drop one hour after

dat <- list(train=train, test=test, test_aux=test_aux, price_inc=price_inc, mean_before=mean_before,
            mean_after=mean_after, standard_dev_ratio=standard_dev_ratio)
save(dat,file="Data/aggData.RData")


load(file="Data/aggData.RData")


######################################################################
################################ TBATS ###############################
######################################################################


##################### Model Fitting #####################

fare_ts <- msts(dat$train[,2], seasonal.periods=c(24,168, 8766))

# tbats fitting
#faretbats <- tbats(fare_ts, num.cores = 7, use.parallel = TRUE,
#                   max.p=24, max.q=24, stepwise=FALSE, max.order=2*24
#                   )

h <- nrow(dat$test_aux)
faretbats <- tbats(fare_ts, num.cores = 7, use.parallel = TRUE,
                   max.p=8, max.q=8,
#                   max.P=8, max.Q=8,
                   max.order=8,
                   stepwise=FALSE)
fc2 <- forecast(faretbats, h=h)
plot(fc2)

# load(file="tbats.RData")

plot(faretbats)
faretbats$likelihood
faretbats$variance

plot(faretbats$errors)
acf(faretbats$errors, lag.max = 500, main="Autocorrelation")
pacf(faretbats$errors, lag.max = 2000, main="Partial Autocorrelation")

faretbats$ar.coefficients
faretbats$ma.coefficients
faretbats$parameters


save(faretbats,file="Data/tbats.RData")

# fiterr <- arma(faretbats$errors, lag=list(ar=2000, ma=2500))

##################### Results analysis #####################

load(file="Data/tbats.RData")

fc2 <- forecast(faretbats, h=nrow(dat$test_aux))

# Train aggreg
sqrt(mean((faretbats$fitted.values-dat$train$fare_amount)**2))  #RMSE
1 - (sum((faretbats$fitted.values-dat$train$fare_amount )^2)/sum((dat$train$fare_amount-mean(dat$train$fare_amount))^2))  # R2
p <- plot_ly(x = ~dat$train$key, y = ~dat$train$fare_amount, name = 'Actual medians', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 1)) %>%
  add_trace(y = ~faretbats$fitted.values, name = 'TBATS fit', line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
  layout(title = "Model fitting",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Fare"))
p

# Test aggreg
sqrt(mean((fc2$mean-dat$test_aux$fare_amount)**2)) #RMSE
1 - (sum((fc2$mean-dat$test_aux$fare_amount )^2)/sum((dat$test_aux$fare_amount-mean(dat$test_aux$fare_amount))^2)) # R2
p <- plot_ly(x = ~dat$test_aux$key, y = ~dat$test_aux$fare_amount, name = 'Actual medians', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 1)) %>%
  add_trace(y = ~fc2$mean, name = 'TBATS pred', line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
  layout(title = "Model fitting",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Fare"))
p

# On the real data (non aggreg)
library(lubridate)
helper <- dat$test_aux
helper$forecast <- fc2$mean
dat$test$time <- dat$test$key
dat$test$key <- floor_date(dat$test$key, "hour")
newData <- left_join(dat$test, helper, by=c("key"))

sapply(newData, function(x) sum(is.na(x)))  # no missing values

attr(newData$forecast, 'tsp') <- NULL  # issue with tsp attribute following the left_join
sqrt(mean(((newData$forecast-newData$fare_amount.x)**2)))  # RMSE tbats
1 - (sum((newData$forecast-newData$fare_amount.x)^2)/sum((newData$fare_amount.x-mean(newData$fare_amount.x))^2))  # R2 tbats

sqrt(mean(((newData$fare_amount.y-newData$fare_amount.x)**2)))  # RMSE real median
1 - (sum((newData$fare_amount.y-newData$fare_amount.x)^2)/sum((newData$fare_amount.x-mean(newData$fare_amount.x))^2))  # R2 real median


# residuals vs true value
res = dat$train$fare_amount - fc2$fitted

plot(dat$train$fare_amount, res, 
     ylab="Residuals", xlab="True fare", 
     main="Residuals plot")
abline(v = mean(dat$train$fare_amount), col='red')
abline(h = 0, col='red')

cent <- dat$train$fare_amount-mean(dat$train$fare_amount)
lm(res ~ cent)

# residuals vs fitted
plot(fc2$fitted, res,
     ylab="Residuals", xlab="Fitted", 
     main="Residuals plot")  # heteroscedasticity
abline(v = 0, col='red')
abline(h = 0, col='red')

plot(dat$train$fare_amount,fc2$fitted, 
     ylab="Fitted", xlab="True", 
     main="Residuals plot")

lin <- lm(dat$train$fare_amount ~ fc2$fitted)
plot(dat$train$fare_amount, dat$train$fare_amount - (fc2$fitted-lin$coefficients['(Intercept)'])/lin$coefficients["fc2$fitted"], 
     ylab="Residuals", xlab="True fare", 
     main="Residuals plot")

hist(dat$train$fare_amount, breaks=100, freq = FALSE)

#### The error grows linearly with the fare to be predicted

# try with the log

fare_ts <- msts(log(dat$train[,2]), seasonal.periods=c(24,168, 8766))

faretbats <- tbats(fare_ts, num.cores = 7, use.parallel = TRUE)
fc3 <- forecast(faretbats, h=nrow(dat$test_aux))


plot(dat$train$fare_amount, dat$train$fare_amount - exp(fc3$fitted), 
     ylab="Residuals", xlab="True fare", 
     main="Residuals plot")  # no change



######################################################################
######################## Fourier + regressors ########################
######################################################################


##################### Weather data #####################
library(riem)
weather <- riem_measures(station = 'NYC', date_start = '2008-12-31', date_end = '2015-07-02')
head(weather, 1)
tail(weather, 1)
weather$key <- as.POSIXct(weather$valid, format="%Y-%m-%d %H:%M:%S")
weather <- weather[,c('key', 'tmpf', 'relh', 'sknt', 'vsby')]
weather <- weather %>% thicken("hour") %>% group_by(key_hour) %>% summarise(tmpf=mean(tmpf),
                                                                            relh=mean(relh),
                                                                            sknt=mean(sknt),
                                                                            vsby=mean(vsby))  # resampling

weather <- plyr::rename(weather,c("key_hour"="key"))
weather[,-c(1)] <- data.frame(lapply(weather[,-c(1)], function(x) scale(x, center = TRUE, scale = TRUE)))

summary(weather)  # some missing variables

attr(weather$key, "tzone") <- 'EST'

train <- left_join(dat$train[,c('key', 'fare_amount')], weather, by=c("key"))
test <- left_join(dat$test_aux[,c('key', 'fare_amount')], weather, by=c("key"))

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train,2,pMiss)  # less than 5% of each variables are missing -> we can imput them
apply(test,2,pMiss)

library(mice)
md.pattern(train)       

library(VIM)
aggr_plot <- aggr(train, col=c('navyblue','red'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

weather_train <- mice(train,m=1,maxit=10,meth='rf',seed=500)
densityplot(weather_train)
weather_train <- complete(weather_train,1)
weather_train$fare_amount <- weather_train$key <- NULL

weather_test <- mice(test,m=1,maxit=10,meth='rf',seed=500)
densityplot(weather_test)
weather_test <- complete(weather_test,1)
weather_test$fare_amount <- weather_test$key <- NULL

##################### ARIMA fitting #####################

z <- fourier(fare_ts, K=c(10,10,10))
zf <- fourier(fare_ts, K=c(10,10,10), h=nrow(dat$test_aux))

fit <- auto.arima(fare_ts, xreg=cbind(z,weather_train), seasonal=FALSE, num.cores = 7, parallel = TRUE,
                  max.p=8, max.q=8,
#                  max.P=8, max.Q=8,
                  max.order=8,
                  stepwise = FALSE
                  )

save(fit,file="Data/arima.RData")
load(file="Data/arima.RData")

fit$coef
fit$arma

fc <- forecast(fit, xreg=cbind(zf,weather_test), h=nrow(dat$test_aux))

##################### ARIMA Analysis #####################

sqrt(mean((fit$fitted-dat$train$fare_amount)**2))
1 - (sum((fit$fitted-dat$train$fare_amount )^2)/sum((dat$train$fare_amount-mean(dat$train$fare_amount))^2))  # r2 on medians
p <- plot_ly(x = ~dat$train$key, y = ~dat$train$fare_amount, name = 'Actual medians', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 1)) %>%
  add_trace(y = ~fc$fitted, name = 'ARIMA fit', line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
  layout(title = "Model fitting",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Fare"))
p

sqrt(mean((fc$mean-dat$test_aux$fare_amount)**2))
1 - (sum((fc$mean-dat$test_aux$fare_amount )^2)/sum((dat$test_aux$fare_amount-mean(dat$test_aux$fare_amount))^2))
p <- plot_ly(x = ~dat$test_aux$key, y = ~dat$test_aux$fare_amount, name = 'Actual medians', type = 'scatter', mode = 'lines',
             line = list(color = 'rgb(205, 12, 24)', width = 1)) %>%
  add_trace(y = ~fc$mean, name = 'ARIMA pred', line = list(color = 'rgb(22, 96, 167)', width = 1)) %>%
  layout(title = "Model fitting",
         xaxis = list(title = "Date"),
         yaxis = list (title = "Fare"))
p


# residuals vs true value
res = dat$train$fare_amount - fc$fitted

plot(dat$train$fare_amount, res, 
     ylab="Residuals", xlab="True fare", 
     main="Residuals plot")
abline(v = mean(dat$train$fare_amount), col='red')
abline(h = 0, col='red')

cent <- dat$train$fare_amount-mean(dat$train$fare_amount)
lm(res ~ cent)

# residuals vs fitted
plot(fc$fitted, res,
     ylab="Residuals", xlab="Prediction", 
     main="Residuals plot")  # heteroscedasticity
abline(h = 0, col='red')


plot(dat$train$fare_amount,fc$fitted, 
     ylab="Fitted", xlab="True", 
     main="Residuals plot")

lin <- lm(dat$train$fare_amount ~ fc$fitted)
plot(dat$train$fare_amount, dat$train$fare_amount - (fc$fitted-lin$coefficients['(Intercept)'])/lin$coefficients["dat$train$fitted"], 
     ylab="Residuals", xlab="True fare", 
     main="Residuals plot")

hist(dat$train$fare_amount, breaks=100, freq = FALSE)


######################################################################
##################### Residuals for further fit ######################
######################################################################

library(lubridate)
Data0 <- fread(
  file="Data/Data0.csv",
)


Data0$date=as.POSIXct(Data0$date, format="%Y-%m-%dT%H:%M:%SZ", tz='EST')  # char to datetime
Data0$key <- Data0$date
Data0$key <- floor_date(Data0$key, "hour")

Data_test <- Data0[Data0$key>=limit,]
Data_train <- Data0[Data0$key<limit,]

dat$test_aux$forecasts <- (fc$mean - dat$mean_before) * dat$standard_dev_ratio + dat$mean_after

dat$train$fitted <- fit$fitted
dat$train[dat$train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S", tz='EST'),
      'fitted'] <- (dat$train[dat$train$key>=as.POSIXct("2012-09-04 00:00:00", format="%Y-%m-%d %H:%M:%S", tz='EST'),
                             'fitted'] - dat$mean_before) * dat$standard_dev_ratio + dat$mean_after

newData_test <- left_join(Data_test, dat$test_aux[,c('key', 'forecasts')], by=c("key"))
attr(newData_test$forecasts, 'tsp') <- NULL  # issue with tsp attribute following the left_join
newData_test$res_fare <- newData_test$fare-newData_test$forecasts
newData_test$key <- NULL

newData_train <- left_join(Data_train, dat$train[,c('key', 'fitted')], by=c("key"))
attr(newData_train$fitted, 'tsp') <- NULL  # issue with tsp attribute following the left_join
newData_train$res_fare <- newData_train$fare-newData_train$fitted
newData_train$key <- NULL

write.csv(newData_train, file = "Data/train_res.csv")
write.csv(newData_test, file = "Data/test_res.csv")




