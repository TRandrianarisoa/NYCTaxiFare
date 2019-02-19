#########################################
######### Exploratory analysis ##########
#########################################

rm(list=objects())

library(padr)
library(plyr)
library(dplyr)

library(plotly)
Sys.setenv("plotly_username"="ThibaultR7")
Sys.setenv("plotly_api_key"="OQTkqSvSm8GdBkldp1jY")
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoidGhpYmF1bHRyIiwiYSI6ImNqb3llNWJubjBhbTIzcXA1MWtucWw5b2EifQ.NvXNEjdCyHl2-Au3CnhPvQ')

################################# Data file creation #################################
library(data.table)
Data <- fread(
  file="Data/train.csv",
  nrows = 3000000
)

# summary
head(Data, n = 5)
tail(Data, n= 5)
summary(Data)
nrow(Data)

Data$pickup_datetime=as.POSIXct(Data$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz = 'GMT')

# Remove some incoherent data
# a look in the test set shows that the following values are the accurate boundaries
offset <- 0.1 # otherwise some test data are on the edge of the domain studied
min_lng <- -74.263242 - offset
min_lat <- 40.573143 - offset
max_lng <- -72.986532 + offset
max_lat <- 41.709555 + offset

Data <- Data[!(Data$fare_amount<0 |
               Data$pickup_longitude<min_lng | Data$pickup_longitude>max_lng |
               Data$pickup_latitude<min_lat  | Data$pickup_latitude>max_lat|
               Data$dropoff_longitude<min_lng | Data$dropoff_longitude>max_lng |
               Data$dropoff_latitude<min_lat  | Data$dropoff_latitude>max_lat |
               Data$passenger_count>10),]
nrow(Data)

# Expand datetime features
library(lubridate)
Data$year <- year(Data$pickup_datetime)
Data$month <- month(Data$pickup_datetime)
Data$hour <- hour(Data$pickup_datetime)
Data$weekday_name <- weekdays(Data$pickup_datetime)
Data$weekday <- as.POSIXlt(Data$pickup_datetime)$wday
Data$weekday <- Data$weekday + 7 * (Data$weekday<1) - 1

head(Data)
summary(Data)

# Missing values
sapply(Data, function(x) sum(is.na(x)))  # no missing values

################################### Prelim dataviz ###################################
# Location
p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$pickup_longitude, name='pickup', nbinsx=150) %>%
  add_histogram(x = ~Data$dropoff_longitude, name='dropoff', nbinsx=150) %>%
  layout(barmode = "overlay", title='Longitude histograms', xaxis=list(title='Longitude'))
p  # longitude distribution

p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$pickup_latitude, name='pickup', nbinsx=150) %>%
  add_histogram(x = ~Data$dropoff_latitude, name='dropoff', nbinsx=150) %>%
  layout(barmode = "overlay", title='Latitude histograms', xaxis=list(title='Latitude'))
p  # latitude distribution

# fare histograms
p <- plot_ly(x = ~Data$fare_amount, type = "histogram", nbinsx=150) %>%
  layout(title='Fare histogram', xaxis=list(title='Fare'))
p  # very skewed

p <- plot_ly(x = ~log(Data$fare_amount), type = "histogram", nbinsx=100) %>%
  layout(title='log Fare histogram', xaxis=list(title='Fare'))
p #log

# boxplots
p <- plot_ly(y = Data$passenger_count, type = "box", name = '') %>%
  layout(title='Passenger count boxplot', yaxis=list(title='Count'))
p

Dat <- plyr::count(Data, c('passenger_count'))
p <- plot_ly(
  x = Dat$passenger_count,
  y = Dat$freq,
  type = "bar"
) %>% layout(yaxis = list(title = 'Trips count'), xaxis = list(title = 'Passenger count'))
p

p <- plot_ly(y = Data$fare_amount, type = "box", name = '') %>%
  layout(title='Fare boxplot', yaxis=list(title='Fare'))
p

p <- plot_ly(y = log(Data$fare_amount), type = "box", name = '') %>%
  layout(title='log Fare boxplot', yaxis=list(title='Fare'))
p #log

###################################### Heatmap #######################################

library(tidyr)
agg <- Data[Data$fare_amount<=60,
            c('fare_amount', 'pickup_longitude', 'dropoff_longitude', 'dropoff_latitude', 'pickup_latitude')]
cols <- names(agg)[2:5]
agg[,(cols) := round(.SD,3), .SDcols=cols]

# pickup
agg1 <- aggregate(fare_amount ~ pickup_latitude+pickup_longitude, data=agg, median, na.rm=TRUE)
agg1_matrix <- xtabs(fare_amount ~ pickup_latitude+pickup_longitude, data=agg1)
agg1_matrix[agg1_matrix==0]<-NA

p <- plot_ly(z = agg1_matrix, colors = colorRamp(c("green", "red")), type = "heatmap")
p

#dropoff
agg2 <- aggregate(fare_amount ~ dropoff_latitude+dropoff_longitude, data=agg, median, na.rm=TRUE)
agg2_matrix <- xtabs(fare_amount ~ dropoff_latitude+dropoff_longitude, data=agg2)
agg2_matrix[agg2_matrix==0]<-NA

p <- plot_ly(z = agg2_matrix, colors = colorRamp(c("green", "red")), type = "heatmap")
p

###################################### Geoplots ######################################


## Number of trips ##
library(scales)
Dat <- Data[,c('pickup_latitude', 'pickup_longitude')]
Dat$pickup_latitude <- floor(Dat$pickup_latitude*200)/200
Dat$pickup_longitude <- floor(Dat$pickup_longitude*200)/200
Dat <- plyr::count(Dat, c('pickup_latitude', 'pickup_longitude'))
summary(Dat)
Dat$freq_norm <- rescale(log(Dat$freq), to = c(1, 5),
                         from = range(log(Dat$freq), na.rm =TRUE, finite = TRUE))

# pickup data
p <- Dat %>%
  plot_mapbox(lat = ~pickup_latitude, lon = ~pickup_longitude, text = ~freq,
              marker = list(size=~freq_norm, color = ~freq, opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Pickup locations in New York',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=13,
           center= list(lat=40.721319, lon=-73.987130),
           style= "dark"
           ),
         autosize=FALSE
         )
p

# Drop off data
Dat <- Data[,c('dropoff_latitude', 'dropoff_longitude')]
Dat$dropoff_latitude <- floor(Dat$dropoff_latitude*200)/200
Dat$dropoff_longitude <- floor(Dat$dropoff_longitude*200)/200
Dat <- plyr::count(Dat, c('dropoff_latitude', 'dropoff_longitude'))
summary(Dat)
Dat$freq_norm <- rescale(log(Dat$freq), to = c(1, 5),
                         from = range(log(Dat$freq), na.rm =TRUE, finite = TRUE))

p <- Dat %>%
  plot_mapbox(lat = ~dropoff_latitude, lon = ~dropoff_longitude, text = ~freq,
              marker = list(size=~freq_norm, color = ~freq, opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Dropoff locations in New York',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=13,
           center= list(lat=40.721319, lon=-73.987130),
           style= "dark"
         ),
         autosize=FALSE
  )
p

## Fare ##

# pickup data
Dat <- Data[,c('fare_amount', 'pickup_latitude', 'pickup_longitude')]
Dat$pickup_latitude <- floor(Dat$pickup_latitude*200)/200
Dat$pickup_longitude <- floor(Dat$pickup_longitude*200)/200
Datb <- plyr::count(Dat, c('pickup_latitude', 'pickup_longitude'))
Datb <- Datb[with(Datb, order(pickup_latitude, pickup_longitude)),]
Dat <- aggregate(fare_amount~pickup_latitude+pickup_longitude,Dat,mean)
Dat <- Dat[with(Dat, order(pickup_latitude, pickup_longitude)),]
Dat$count <- Datb$freq
Dat <- Dat[Dat$count>=30,]
summary(Dat)
Dat$fare_amount_norm <- rescale(Dat$fare_amount, to = c(1, 10),
                         from = range(Dat$fare_amount, na.rm =TRUE, finite = TRUE))


p <- Dat %>%
  plot_mapbox(lat = ~pickup_latitude, lon = ~pickup_longitude, text = ~fare_amount,
              marker = list(size=~fare_amount_norm, color = ~fare_amount_norm, opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Mean fare in New York',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=13,
           center= list(lat=40.721319, lon=-73.987130),
           style= "dark"
         ),
         autosize=FALSE
  )
p

# Dropoff data
Dat <- Data[,c('fare_amount', 'dropoff_latitude', 'dropoff_longitude')]
Dat$dropoff_latitude <- floor(Dat$dropoff_latitude*200)/200
Dat$dropoff_longitude <- floor(Dat$dropoff_longitude*200)/200
Datb <- plyr::count(Dat, c('dropoff_latitude', 'dropoff_longitude'))
Datb <- Datb[with(Datb, order(dropoff_latitude, dropoff_longitude)),]
Dat <- aggregate(fare_amount~dropoff_latitude+dropoff_longitude,Dat,mean)
Dat <- Dat[with(Dat, order(dropoff_latitude, dropoff_longitude)),]
Dat$count <- Datb$freq
Dat <- Dat[Dat$count>=30,]
summary(Dat)
Dat$fare_amount_norm <- rescale(Dat$fare_amount, to = c(1, 10),
                                from = range(Dat$fare_amount, na.rm =TRUE, finite = TRUE))

p <- Dat %>%
  plot_mapbox(lat = ~dropoff_latitude, lon = ~dropoff_longitude, text = ~fare_amount,
              marker = list(size=~fare_amount_norm, color = ~fare_amount_norm, opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Mean fare in New York',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=13,
           center= list(lat=40.721319, lon=-73.987130),
           style= "dark"
         ),
         autosize=FALSE
  )
p

# Business days
Bus_Data <- Data[(Data$weekday<=5),]
Bus_Data$day_moment <- ifelse((Bus_Data$hour < 10 & Bus_Data$hour > 5), "Early_Business", NA)
Bus_Data <- within(Bus_Data, day_moment <- ifelse(is.na(day_moment),
                                                  ifelse(Bus_Data$hour > 18, "Late_Business", NA),
                                                  day_moment))
head(Bus_Data)

p <- Bus_Data[complete.cases(Bus_Data), ][1:10000,] %>%
  plot_mapbox(lat = ~pickup_latitude, lon = ~pickup_longitude, split = ~day_moment, hoverinfo='name',
              marker = list(size=4, opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Pickup locations in New York - Business',
         font = list(color='white'),
         autosize=FALSE,
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=11,
           center= list(lat=40.721319, lon=-73.987130),
           style = 'mapbox://styles/thibaultr/cjoyogk4401cc2qljd2syhhql'
           ),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p

# Weekend
WD_Data <- Data[(Data$weekday>5),]
WD_Data$day_moment <- ifelse((WD_Data$hour < 10 & WD_Data$hour > 5), "Early_WD", NA)
WD_Data <- within(WD_Data, day_moment <- ifelse(is.na(day_moment),
                                                  ifelse(WD_Data$hour > 18, "Late_WD", NA),
                                                  day_moment))
head(WD_Data)

p <- WD_Data[complete.cases(WD_Data), ][1:10000,] %>%
  plot_mapbox(lat = ~pickup_latitude, lon = ~pickup_longitude, split = ~day_moment, hoverinfo='name',
              marker = list(size=4, opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Pickup locations in New York - Weekend',
         font = list(color='white'),
         autosize=FALSE,
         plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=11,
           center= list(lat=40.721319, lon=-73.987130),
           style = 'mapbox://styles/thibaultr/cjoyogk4401cc2qljd2syhhql'
         ),
         legend = list(orientation = 'h',
                       font = list(size = 8)),
         margin = list(l = 25, r = 25,
                       b = 25, t = 25,
                       pad = 2))
p

# Highfare locations

High_fares <- Data[(Data$fare_amount > mean(Data$fare_amount) + 3*sqrt(var(Data$fare_amount))),]
High_fares <- High_fares[High_fares$weekday>=5,]
nrow(High_fares)

p <- plot_mapbox(mode = 'scattermapbox') %>%
  add_markers(
    data = High_fares, x = ~pickup_longitude, y = ~pickup_latitude, text=~pickup_datetime, color=I("red"),
    size = ~fare_amount, hoverinfo = "text", alpha = 0.5) %>%
  add_markers(
    data = High_fares, x = ~dropoff_longitude, y = ~dropoff_latitude, text=~pickup_datetime, color=I("cyan"),
    size = ~fare_amount, hoverinfo = "text", alpha = 0.5) %>%
  add_segments(
    data = High_fares,
    x = ~pickup_longitude, xend = ~dropoff_longitude,
    y = ~pickup_latitude, yend = ~dropoff_latitude,
    alpha = 0.3, size = I(1), hoverinfo = "none", opacity = 0.2,
    color=I("red")) %>%
  layout(
    title = 'High fares journeys',
    plot_bgcolor = '#191A1A', paper_bgcolor = '#191A1A',
    mapbox = list(
      bearing=10,
      pitch=60,
      zoom=11,
      center= list(lat = median(High_fares$pickup_latitude),
                   lon = median(High_fares$pickup_longitude)),
      style= "mapbox://styles/shaz13/cjiog1iqa1vkd2soeu5eocy4i"
    ),
    margin = list(l = 0, r = 0,
                  b = 0, t = 0,
                  pad = 0),
    showlegend=FALSE)
p


###################################### Airports ######################################
Data$JFK_pickup <- Data$JFK_dropoff <- Data$EWR_pickup <- Data$EWR_dropoff <- Data$LaGuardia_pickup <- Data$LaGuardia_dropoff <- 0
# New column for airports
Data[!(Data$pickup_longitude<(-73.8352) | Data$pickup_longitude>(-73.7401) |
             Data$pickup_latitude<40.619  | Data$pickup_latitude>40.6659), 'JFK_pickup'] <- 1

summary(Data[Data$JFK_pickup==1,])

Data[!(Data$dropoff_longitude<(-73.8352) | Data$dropoff_longitude>(-73.7401) |
         Data$dropoff_latitude<40.619  | Data$dropoff_latitude>40.6659), 'JFK_dropoff'] <- 1

summary(Data[Data$JFK_dropoff==1,])

Data[!(Data$pickup_longitude<(-74.1925) | Data$pickup_longitude>(-74.1531) |
         Data$pickup_latitude<40.6700  | Data$pickup_latitude>40.7081), 'EWR_pickup'] <- 1

summary(Data[Data$EWR_pickup==1,])

Data[!(Data$dropoff_longitude<(-74.1925) | Data$dropoff_longitude>(-74.1531) |
         Data$dropoff_latitude<40.6700  | Data$dropoff_latitude>40.7081), 'EWR_dropoff'] <- 1

summary(Data[Data$EWR_dropoff==1,])

Data[!(Data$pickup_longitude<(-73.8895) | Data$pickup_longitude>(-73.8550) |
         Data$pickup_latitude<40.7664  | Data$pickup_latitude>40.7931), 'LaGuardia_pickup'] <- 1

summary(Data[Data$LaGuardia_pickup==1,])

Data[!(Data$dropoff_longitude<(-73.8895) | Data$dropoff_longitude>(-73.8550) |
         Data$dropoff_latitude<40.7664  | Data$dropoff_latitude>40.7931), 'LaGuardia_dropoff'] <- 1

summary(Data[Data$LaGuardia_dropoff==1,])


p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$fare_amount, name='All trips', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~Data[Data$JFK_dropoff==1,]$fare_amount, name='JFK dropoffs', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~Data[Data$EWR_dropoff==1,]$fare_amount, name='EWR dropoffs', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~Data[Data$LaGuardia_dropoff==1,]$fare_amount, name='LaGuardia dropoffs', histnorm='probability', nbinsx=100) %>%
  layout(barmode = "overlay", title='Fare histograms (dropoff loc)', xaxis=list(title='Fares', range=list(0,100)))
p

p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$fare_amount, name='All trips', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~Data[Data$JFK_pickup==1,]$fare_amount, name='JFK dropoffs', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~Data[Data$EWR_pickup==1,]$fare_amount, name='EWR dropoffs', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~Data[Data$LaGuardia_pickup==1,]$fare_amount, name='LaGuardia dropoffs', histnorm='probability', nbinsx=100) %>%
  layout(barmode = "overlay", title='Fare histograms (dropoff loc)', xaxis=list(title='Fares', range=list(0,100)))
p

###################################### Distance ######################################
library(pracma)
distance_from_coord <- function(row) {
  return(haversine(c(row[1], row[2]), c(row[3], row[4])))
}

Data$distance <- apply(Data[, c('pickup_latitude', 'pickup_longitude', 'dropoff_latitude', 'dropoff_longitude')], 1,
                       distance_from_coord)

summary(Data$distance)

p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$distance, histnorm='probability', nbinsx=300) %>%
  layout(title='Distances', xaxis=list(title='Kilometers', range=list(0,30)))
p


p <- plot_ly(data = Data, x = ~distance, y = ~fare_amount, type='scatter',
             marker = list(size = 2,
                           color = 'rgba(255, 182, 193, .9)')) %>%
  layout(title='Distances VS Price', xaxis=list(title='Distance (Km)'), yaxis=list(title='Fare (dollars'))
p

Dat = Data[!((Data$JFK_pickup==1)|(Data$EWR_pickup==1)|(Data$LaGuardia_pickup==1)|
             (Data$JFK_dropoff==1)|(Data$EWR_dropoff==1)|(Data$LaGuardia_dropoff==1)),]
p <- plot_ly(data = Dat, x = ~distance, y = ~fare_amount, type='scatter',
             marker = list(size = 2,
                           color = 'rgba(255, 182, 193, .9)')) %>%
  layout(title='Distances VS Price (without airports)', xaxis=list(title='Distance (Km)'), yaxis=list(title='Fare (dollars'))
p

summary(Data[Data$passenger_count==0,c('distance', 'fare_amount')])
summary(Data[Data$distance==0,c('passenger_count', 'fare_amount')])


###################################### Temporal repartition ######################################

# Count
Dat <- plyr::count(Data, c('year'))
p <- plot_ly(
  x = Dat$year,
  y = Dat$freq,
  name = "Trips per year",
  type = "bar"
) %>% layout(yaxis = list(title = 'Trips count'), xaxis = list(title = 'year'), title='Trips per year')
p

Dat <- plyr::count(Data, c('year', 'month'))
Dat$Date <- paste(Dat$year, Dat$month)
p <- plot_ly(
  x = Dat$Date,
  y = Dat$freq,
  name = "Trips per month",
  type = "bar"
) %>% layout(yaxis = list(title = 'Trips count'),
             xaxis = list(title = 'year',
                          categoryorder = "array",
                          categoryarray = 1:nrow(Dat)),
             title='Trips per month')
p

Dat <- plyr::count(Data, c('weekday_name'))
p <- plot_ly(
  x = Dat$weekday_name,
  y = Dat$freq,
  name = "Trips per Day",
  type = "bar"
) %>% layout(yaxis = list(title = 'Trips count'),
             xaxis = list(title = 'Day',
                          categoryorder = "array",
                          categoryarray = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
             title='Trips perDay')
p

Dat <- plyr::count(Data, c('hour'))
p <- plot_ly(
  x = Dat$hour,
  y = Dat$freq,
  name = "Trips per hour",
  type = "bar"
) %>% layout(yaxis = list(title = 'Trips count'), xaxis = list(title = 'hour'), title='Trips per hour')
p


# Mean
Dat <- Data %>% group_by(year) %>% summarise(mean(fare_amount))
Dat$fare <- Dat$`mean(fare_amount)`
p <- plot_ly(
  x = Dat$year,
  y = Dat$fare,
  name = "Mean fare per year",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'year'), title='Mean fare per year')
p

Dat <- Data %>% group_by(year, month) %>% summarise(mean(fare_amount))
Dat$fare <- Dat$`mean(fare_amount)`
Dat$Date <- paste(Dat$year, Dat$month)
p <- plot_ly(
  x = Dat$Date,
  y = Dat$fare,
  name = "Mean fare per Month",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'),
             xaxis = list(title = 'month',
                          categoryorder = "array",
                          categoryarray = 1:nrow(Dat)),
             title='Mean fare per Month')
p

Dat <- Data %>% group_by(weekday_name) %>% summarise(mean(fare_amount))
Dat$fare <- Dat$`mean(fare_amount)`
p <- plot_ly(
  x = Dat$weekday_name,
  y = Dat$fare,
  name = "Mean fare per day",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'),
             xaxis = list(title = 'Day',
                          categoryorder = "array",
                          categoryarray = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')),
             title='Mean fare per day')
p

Dat <- Data %>% group_by(hour) %>% summarise(mean(fare_amount))
Dat$fare <- Dat$`mean(fare_amount)`
p <- plot_ly(
  x = Dat$hour,
  y = Dat$fare,
  name = "Mean fare per hour",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'hour'), title='Mean fare per hour')
p


# Median
Dat <- Data %>% group_by(year) %>% summarise(median(fare_amount))
Dat$fare <- Dat$`median(fare_amount)`
p <- plot_ly(
  x = Dat$year,
  y = Dat$fare,
  name = "Median fare per year",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'year'), title='Median fare per year')
p

Dat <- Data %>% group_by(year, month) %>% summarise(median(fare_amount))
Dat$fare <- Dat$`median(fare_amount)`
Dat$Date <- paste(Dat$year, Dat$month)
p <- plot_ly(
  x = Dat$Date,
  y = Dat$fare,
  name = "Median fare per year",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'),
             xaxis = list(title = 'year',
                          categoryorder = "array",
                          categoryarray = 1:nrow(Dat)),
             title='Median fare per year')
p

Dat <- Data %>% group_by(weekday_name) %>% summarise(median(fare_amount))
Dat$fare <- Dat$`median(fare_amount)`
p <- plot_ly(
  x = Dat$weekday_name,
  y = Dat$fare,
  name = "Median fare per day",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'Day'), title='Median fare per day')
p

Dat <- Data %>% group_by(hour) %>% summarise(median(fare_amount))
Dat$fare <- Dat$`median(fare_amount)`
p <- plot_ly(
  x = Dat$hour,
  y = Dat$fare,
  name = "Median fare per hour",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'hour'), title='Median fare per hour')
p


###################################### Passenger_count ######################################
Dat <- Data %>% group_by(passenger_count) %>% summarise(median(fare_amount))
Dat$fare <- Dat$`median(fare_amount)`
p <- plot_ly(
  x = Dat$passenger_count,
  y = Dat$fare,
  name = "Median fare per passenger count",
  type = "bar"
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'passenger count'), title='Median fare per passenger count')
p

Dat <- Data %>% group_by(passenger_count) %>% summarise(mean(fare_amount), n(), sd(fare_amount))
Dat <- Dat[Dat$`n()`>30,]
Dat$fare <- Dat$`mean(fare_amount)`
p <- plot_ly(
  x = Dat$passenger_count,
  y = Dat$fare,
  name = "Mean fare per passenger count",
  type = "bar",
  error_y = ~list(array = Dat$`sd(fare_amount)`,
                  color = '#000000')
) %>% layout(yaxis = list(title = 'Fare'), xaxis = list(title = 'passenger count'), title='Mean fare per passenger count')
p

