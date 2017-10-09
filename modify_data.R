library(dplyr)
library(pracma)
library(stringr)
library(sp)
library(rgdal)

load("data.RData")
data <- tbl_df(data)

## Select relevant columns

data_short <- data %>% select(price, zipcode, latitude, longitude,
                              number_of_reviews, review_scores_rating, review_scores_accuracy,
                              review_scores_cleanliness, review_scores_checkin, review_scores_communication,
                              review_scores_location, review_scores_value,
                              property_type, room_type, accommodates, bathrooms, bedrooms, beds, amenities,
                              minimum_nights, instant_bookable, cancellation_policy)

## Apply Initial filters

data_short <- filter(data_short, data_short$number_of_reviews >= 3 &
                                 data_short$property_type == "Apartment" &
                                 data_short$room_type == "Private room" &
                                 data_short$cancellation_policy != "super_strict_30")

## Mutate price per person & Filter Price PP <= 100

data_short$price <- as.double(substr(paste(data_short$price), 2, 500))
data_short <- data_short %>% filter(!is.na(price)) %>% mutate(price_pp = price / accommodates)
#data_short <- data_short %>% filter(!is.na(price_pp)) %>% filter(price <= 100)

## Mutate Zipcode as string

data_short$zipcode <- as.character(data_short$zipcode)
data_short$zip_first <- str_extract(data_short$zipcode, "[[:alpha:]]{1,2}[[:digit:]]{1,2}")
data_short$zip_first <- toupper(data_short$zip_first)

## Add rent data 

rent_data <- read.csv("Price_rent_0927.csv")
rent_data <- rent_data %>% group_by(zip_first = toupper(zipcode)) %>% summarise(n = n(), mean_rent = mean(rent))
data_short <- merge(data_short, rent_data_short, by="zip_first")

## Mutate distance from Picadilly Circus

my_haversine <- function(long, lat) {
  longlat = c()
  for (i in c(1:length(lat))) {
    longlat <- append(longlat, haversine(c(long[i], lat[i]), c(-0.133869, 51.510067)))
  }
  return(longlat)
}
longlatlist <- my_haversine(data_short$longitude, data_short$latitude)
data_short$distance <- longlatlist

rm(i, key, longlatlist)

## Mutating Easting and Northing Data

cord_data <- data.frame(long = data_short$longitude, lat = data_short$latitude)
cord.dec = SpatialPoints(cbind(cord_data$long, cord_data$lat), proj4string = CRS("+proj=longlat"))
cord.dec = SpatialPoints(cbind(data$long, data$lat), proj4string = CRS("+proj=longlat"))
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:27700"))
data_short$east <- cord.UTM$coords.x1
data_short$north <- cord.UTM$coords.x2

rm(cord_data, cord.dec, cord.UTM)

## Mutate Amenities Dummy variables

amen_str <- paste0(data_short$amenities, collapse = ",")
amen_str <- str_replace_all(amen_str, "\\{", "")
amen_str <- str_replace_all(amen_str, "\\}", "")
amen_str <- gsub('\"', "", amen_str, fixed = TRUE)
amen_list <- strsplit(amen_str, ",")
amen_list_uniq <- unique(unlist(amen_list))

for (i in c(1:length(amen_list_uniq))) {
  key <- paste0(c("amen", str_replace_all(amen_list_uniq[i]," ","_")), collapse = "_")
  data_short[key] <- grepl(amen_list_uniq[i], paste(data_short$amenities))
}

rm(amen_str, amen_list, amen_list_uniq, i, key)

## Mutate Amenities Count

amen_data <- data_short[ , grepl( "amen_" , names( data_short ) ) ]
data_short$amenities_count <- "rowSums"(amen_data)

rm(amen_data)

## Rename instant bookable

data_short$instant_bookable <- factor(data_short$instant_bookable, levels = c("t", "f"), labels = c(TRUE, FALSE))

## Final cleaning and sorting of the data

data_short <- na.omit(data_short)
data_short <- data_short %>% select(price,
                                    zip_first, mean_rent, distance, east, north,
                                    starts_with("review"), number_of_reviews,
                                    property_type, room_type, accommodates, bathrooms, bedrooms, beds, starts_with("amen"),
                                    minimum_nights, instant_bookable, cancellation_policy,
                                    price_pp)

save(data_short, file = "data_short.RData")
