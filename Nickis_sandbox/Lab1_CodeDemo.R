elk_gps <- read.csv("data/Elk_GPS_data.csv")
str(elk_gps)

save(elk_gps, file="./data/elk_gps.rda")

load("./data/elk_gps.rda")
str(elk_gps)

library(move)
require(move)

mylogin <- movebankLogin(username = 'NickiB', password = 'Ne*5ne@ZrfT5QtS')

elk_move <- getMovebankData(study = "Ya Ha Tinda elk project, Banff National Park, 2001-2023 (females)", login = mylogin, removeDuplicatedTimestamps=TRUE)

head(elk_move)

plot(elk_move)

elk_df <- as.data.frame(elk_move)
str(elk_df)

elk_gps <- read.csv("data/Elk_GPS_data.csv")

names(elk_gps)

elk_gps2 <- elk_gps[, -c(5,6)]

names(elk_gps2)

elk_gps2 <- elk_gps[, c("X","timestamp",
                        "location.long", "location.lat",
                        "individual.local.identifier")]
names(elk_gps2)

require(plyr)
elk_gps2 <- elk_gps |> mutate(X = NULL,
                              sensor.type = NULL,
                              migration.stage = NULL)
str(elk_gps2)

elk_gps3 <- elk_gps2
names(elk_gps3) <- c("datetime", "lon", "lat", "id")

str(elk_gps3)

require(plyr)
elk_gps3 <- elk_gps2 |>
  plyr::rename(c(timestamp = "datetime",
                 location.long = "lon", location.lat = "lat",
                 individual.local.identifier = "id"))

elk_gps3$datetime[1]

elk_gps4 <- elk_gps3
elk_gps4$datetime <- as.POSIXct(elk_gps3$datetime, format="%m/%d/%Y %H:%M:%S")
str(elk_gps4)

require(lubridate)
elk_gps4 <- elk_gps3 |> mutate(datetime = mdy_hms(datetime),
                               id = factor(id))

str(elk_gps4)

as.numeric(elk_gps4$datetime[2] - elk_gps4$datetime[1])

as.numeric(difftime(elk_gps4$datetime[2], elk_gps4$datetime[1], units = "mins"))

elk_gps5 <- subset(elk_gps4, !is.na(datetime) & !is.na(lon) & !is.na(lat))
str(elk_gps5)

rm(list=ls())
require(plyr); require(lubridate)
elk_gps <- read.csv("data/Elk_GPS_data.csv") |>
  # 1. keep only columns we want
  mutate(X = NULL,  sensor.type = NULL, migration.stage = NULL) |>
  # 2. rename the columsn with names we like
  plyr::rename(c(timestamp = "datetime",
                 location.long = "lon", location.lat = "lat",
                 individual.local.identifier = "id")) |>
  # 3. fix the data and time
  mutate(datetime = mdy_hms(datetime)) |>
  # 4. remove missing data
  subset(!is.na(datetime) & !is.na(lon) & !is.na(lat))

str(elk_gps)

library(dplyr)

require(dplyr)
elk_gps2 <- elk_gps |>
  group_by(id) |>
  filter(!duplicated(datetime)) |>
  arrange(datetime) |>
  ungroup() |>
  data.frame()

head(elk_gps2)

removeDuplicated <- function(df){
  # this function takes a data frame and returns the same
  # data frame but after filtering away shared values
  # of ID and Time - to remove duplicates
  df |>
    group_by(id) |>
    filter(!duplicated(datetime)) |>
    arrange(datetime) |>
    ungroup() |>
    data.frame()
}

elk_gps2 <- removeDuplicated(elk_gps)

library(sf)

elk_sf <- elk_gps2 |>
  st_as_sf(coords = c("lon","lat"), crs=4326)
elk_sf

require(mapview)
mapview(elk_sf |> subset(id == id[[1]]))

mapview(elk_sf, zcol="id")

require(plyr); require(lubridate); require(dplyr); require(sf)
elk_gps <- read.csv("data/Elk_GPS_data.csv") |>
  mutate(X = NULL,  sensor.type = NULL, migration.stage = NULL) |>
  plyr::rename(c(timestamp = "datetime",
                 location.long = "lon", location.lat = "lat",
                 individual.local.identifier = "id")) |>
  mutate(datetime = mdy_hms(datetime),
         id = factor(id)) |>
  subset(!is.na(datetime) & !is.na(lon) & !is.na(lat)) |>
  removeDuplicated()

elk_sf <- st_as_sf(elk_gps, coords = c("lon", "lat"), crs = 4326)

save(elk_gps, elk_sf, file = "data/elk_processed.rda")


