library(tidyverse)
library(sf)
library(DBI)

## Utiliser pgpass pour ne pas se retrouver avec des
## identifiants dans le code 
con <- dbConnect(RPostgres::Postgres(), host="localhost", dbname="nyctf", user="nyctf")

fare_db <- tbl(con, "fare")
nym <- st_read("~/ampere/taxi_fare/nym.shp")

## fare_db %>% filter(ilong > -74.5, ilong < -73.5, ilat < 41, ilat > 40.4) %>%
##     ggplot() +  geom_sf(data=nym, fill="#222222", color="#222222") + 
##     geom_point(aes(x=ilong, y=ilat), alpha=0.1, size=0.1, color="red") +
##     coord_sf(ylim=c(40.5, 40.9), xlim=c(-74.2, -73.7)



for(i in 0:24)
{
    fare_db  %>% filter(ilong > -74.5, ilong < -73.5, ilat < 41, ilat > 40.4)  %>%
        filter(extract(day %from% date) == 1 & extract(hour %from% date) == i) %>%
        ggplot() +  geom_sf(data=nym, fill="#222222", color="#222222") +
        geom_point(aes(x=ilong, y=ilat), alpha=0.5, size=0.01, color="red") +
        coord_sf(ylim=c(40.5, 40.9), xlim=c(-74.2, -73.7))
    ggsave(sprintf("fare%03d.png", i))
}

fare_db %>% filter(extract(day %from% date) == 1) %>%
    ggplot() + geom_col(aes(x=date, y=fare))

ft <- fare_db %>% filter(extract(day %from% date) == 1)

ft  %>% group_by(hour=extract(hour %from% date)) %>% ggplot(aes(group=hour, y=fare)) + geom_boxplot()

ft %>% ggplot() +  geom_sf(data=nym, fill="#222222", color="#222222") +
    geom_point(aes(x=ilong, y=ilat), alpha=0.5, size=0.01, color="red") +
    coord_sf(ylim=c(40.5, 40.9), xlim=c(-74.2, -73.7))
