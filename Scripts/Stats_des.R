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
  add_histogram(x = ~Data$pickup_longitude, name='pickup') %>%
  add_histogram(x = ~Data$dropoff_longitude, name='dropoff') %>%
  layout(barmode = "overlay", title='Longitude histograms', xaxis=list(title='Longitude'))
p

p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$pickup_latitude, name='pickup') %>%
  add_histogram(x = ~Data$dropoff_latitude, name='dropoff') %>%
  layout(barmode = "overlay", title='Latitude histograms', xaxis=list(title='Latitude'))
p

# fare histograms
p <- plot_ly(x = ~Data$fare_amount, type = "histogram", nbinsx=150) %>%
  layout(title='Fare histogram', xaxis=list(title='Fare'))
p  # very skewed

p <- plot_ly(x = ~log(Data$fare_amount), type = "histogram", nbinsx=30) %>%
  layout(title='Fare histogram', xaxis=list(title='Fare'))
p #log

# boxplots
p <- plot_ly(y = Data$passenger_count, type = "box", name = '') %>%
  layout(title='Passenger count boxplot', yaxis=list(title='Count'))
p

p <- plot_ly(y = Data$fare_amount, type = "box", name = '') %>%
  layout(title='Fare boxplot', yaxis=list(title='Fare'))
p

p <- plot_ly(y = log(Data$fare_amount), type = "box", name = '') %>%
  layout(title='Fare boxplot', yaxis=list(title='Fare'))
p #log

###################################### Heatmap #######################################

library(tidyr)
agg <- Data[,c('fare_amount', 'pickup_longitude', 'dropoff_longitude', 'dropoff_latitude', 'pickup_latitude')]
cols <- names(agg)[2:5]
agg[,(cols) := round(.SD,3), .SDcols=cols]

agg1 <- aggregate(fare_amount ~ pickup_latitude+pickup_longitude, data=agg, median, na.rm=TRUE)
agg1_matrix <- xtabs(fare_amount ~ pickup_latitude+pickup_longitude, data=agg1)
agg1_matrix[agg1_matrix==0]<-NA

p <- plot_ly(z = agg1_matrix, colors = colorRamp(c("green", "red")), type = "heatmap")
p

agg2 <- aggregate(fare_amount ~ dropoff_latitude+dropoff_longitude, data=agg, median, na.rm=TRUE)
agg2_matrix <- xtabs(fare_amount ~ dropoff_latitude+dropoff_longitude, data=agg2)
agg2_matrix[agg2_matrix==0]<-NA

p <- plot_ly(z = agg2_matrix, colors = colorRamp(c("red", "green")), type = "heatmap")
p
###################################### Geoplots ######################################


# pickup data
p <- Data[1:30000] %>%
  plot_mapbox(lat = ~pickup_latitude, lon = ~pickup_longitude,
              marker = list(size=4, color = 'gold', opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Pickup locations in New York',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=13,
           center= list(lat=40.721319, lon=-73.987130),
           style= "mapbox://styles/shaz13/cjiog1iqa1vkd2soeu5eocy4i"
           ),
         autosize=FALSE
         )
p

# Drop off data
p <- Data[1:10000] %>%
  plot_mapbox(lat = ~dropoff_latitude, lon = ~dropoff_longitude,
              marker = list(size=4, color = 'cyan', opacity = 0.8),
              type = 'scattermapbox', width=900, height=600,
              mode= 'markers') %>%
  layout(title = 'Pickup locations in New York',
         mapbox = list(
           bearing=10,
           pitch=60,
           zoom=13,
           center= list(lat=40.721319, lon=-73.987130),
           style= "mapbox://styles/thibaultr/cjoyogk4401cc2qljd2syhhql"
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
High_fares <- High_fares[High_fares$weekday==0,]
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

JFK_data_pickup = Data[!(Data$pickup_longitude<(-73.8352) | Data$pickup_longitude>(-73.7401) |
                           Data$pickup_latitude<40.619  | Data$pickup_latitude>40.6659),]
summary(JFK_data_pickup)

JFK_data_dropoff = Data[!(Data$dropoff_longitude<(-73.8352) | Data$dropoff_longitude>(-73.7401) |
                           Data$dropoff_latitude<40.619  | Data$dropoff_latitude>40.6659),]
summary(JFK_data_dropoff)


p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$fare_amount, name='All trips', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~JFK_data_dropoff$fare_amount, name='JFK dropoffs', histnorm='probability', nbinsx=100) %>%
  add_histogram(x = ~JFK_data_pickup$fare_amount, name='JFK pickups', histnorm='probability', nbinsx=100) %>%
  layout(barmode = "overlay", title='Fare histograms', xaxis=list(title='Fares', range=list(0,100)))
p

# New column for airports
Data$airport_dropoff <- ''
Data$airport_pickup <- ''

Data[!(Data$pickup_longitude<(-73.8352) | Data$pickup_longitude>(-73.7401) |
         Data$pickup_latitude<40.619  | Data$pickup_latitude>40.6659), 'airport_dropoff'] <- 'JFK'
Data[!(Data$dropoff_longitude<(-73.8352) | Data$dropoff_longitude>(-73.7401) |
         Data$dropoff_latitude<40.619  | Data$dropoff_latitude>40.6659), 'airport_dropoff'] <- 'JFK'

Data[!(Data$pickup_longitude<(-74.1925) | Data$pickup_longitude>(-74.1531) |
         Data$pickup_latitude<40.6700  | Data$pickup_latitude>40.7081), 'airport_dropoff'] <- 'EWR'
Data[!(Data$dropoff_longitude<(-74.1925) | Data$dropoff_longitude>(-74.1531) |
         Data$dropoff_latitude<40.6700  | Data$dropoff_latitude>40.7081), 'airport_dropoff'] <- 'EWR'

Data[!(Data$pickup_longitude<(-73.8895) | Data$pickup_longitude>(-73.8550) |
         Data$pickup_latitude<40.7664  | Data$pickup_latitude>40.7931), 'airport_dropoff'] <- 'LaGuardia'
Data[!(Data$dropoff_longitude<(-73.8895) | Data$dropoff_longitude>(-73.8550) |
         Data$dropoff_latitude<40.7664  | Data$dropoff_latitude>40.7931), 'airport_dropoff'] <- 'LaGuardia'

# distance analysis
library(pracma)
distance_from_coord <- function(row) {
  return(haversine(c(row[1], row[2]), c(row[3], row[4])))
}

Data$distance <- apply(Data[, c('pickup_latitude', 'pickup_longitude', 'dropoff_latitude', 'dropoff_longitude')], 1,
                       distance_from_coord)

summary(Data$distance)
p <- plot_ly(alpha = 0.5) %>%
  add_histogram(x = ~Data$distance, histnorm='probability', nbinsx=100) %>%
  layout(title='Distances', xaxis=list(title='Kilometers', range=list(0,30)))
p


p <- plot_ly(data = Data, x = ~distance, y = ~fare_amount, type='scatter',
             marker = list(size = 1.5,
                           color = 'rgba(255, 182, 193, .9)')) %>%
  layout(title='Distances VS Price', xaxis=list(title='Distance (Km)'), yaxis=list(title='Fare (dollars'))
p

p <- plot_ly(data = Data[(Data$airport_dropoff=='')&(Data$airport_pickup==''),], x = ~distance, y = ~fare_amount, type='scatter',
             marker = list(size = 1.5,
                           color = 'rgba(255, 182, 193, .9)')) %>%
  layout(title='Distances VS Price (without airports)', xaxis=list(title='Distance (Km)'), yaxis=list(title='Fare (dollars'))
p
