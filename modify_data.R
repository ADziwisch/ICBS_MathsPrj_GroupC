library(dplyr)
library(pracma)
library(stringr)

library(sp)
library(rgdal)

load("data.RData")
data <- tbl_df(data)

## Select columns

data_short <- data %>% select(price, zipcode, latitude, longitude, is_location_exact,
                              property_type, room_type, accommodates, bathrooms, bedrooms, beds, amenities,
                              number_of_reviews, review_scores_rating, review_scores_accuracy,
                              review_scores_cleanliness, review_scores_checkin, review_scores_communication,
                              review_scores_location, review_scores_value, minimum_nights, instant_bookable,
                              cancellation_policy)

## Initial filter

data_short <- filter(data_short, data_short$number_of_reviews >= 3 & data_short$property_type == "Apartment" & data_short$room_type == "Private room")

## Mutate price per person

data_short$price <- as.double(substr(paste(data_short$price), 2, 500))
data_short <- data_short %>% filter(!is.na(price))
data_short <- data_short %>% mutate(price_pp = price / accommodates)
data_short <- data_short %>% filter(!is.na(price_pp))

## Mutate Zipcode as string

data_short$zipcode <- as.character(data_short$zipcode)
data_short$zip_first <- str_extract(data_short$zipcode, "[[:alpha:]]{1,2}[[:digit:]]{1,2}")
data_short$zip_first <- toupper(data_short$zip_first)

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

## Mutate Amenities Count

count_amenities <- function(amenities){
  count_list = c()
  for (i in c(1:length(amenities))) {
    count_list <- append(count_list, length(strsplit(as.character(amenities[i]), ",")[[1]]))
  }
  return(count_list)
}
count_list <- count_amenities(data_short$amenities)
data_short$amenities_count <- count_list

## Mutating coor data

cord_data <- data.frame(long = data_short$longitude, lat = data_short$latitude)
cord.dec = SpatialPoints(cbind(cord_data$long, cord_data$lat), proj4string = CRS("+proj=longlat"))

# Setting existing coordinate as lat-long system
cord.dec = SpatialPoints(cbind(data$long, data$lat), proj4string = CRS("+proj=longlat"))

# Transforming coordinate to UTM using EPSG=32748 for WGS=84, UTM Zone=48M,
# Southern Hemisphere)
cord.UTM <- spTransform(cord.dec, CRS("+init=epsg:27700"))

data_short$east <- cord.UTM$coords.x1
data_short$north <- cord.UTM$coords.x2

## Filter the data

# data_short <- data_short %>% filter(price_pp <= 100)
# data_short <- data_short %>% filter(number_of_reviews > 3)
# data_short <- data_short %>% filter(is_location_exact == "t")
# data_short <- data_short %>% filter(!is.na(review_scores_accuracy))
# data_short <- data_short %>% filter(cancellation_policy == "flexible" | cancellation_policy == "moderate" | cancellation_policy == "strict")
# data_short <- data_short %>% filter(!is.na(bathrooms))
# data_short <- data_short %>% filter(!is.na(bedrooms))
# data_short <- data_short %>% filter(!is.na(beds))

data_short <- na.omit(data_short)

save(data_short, file = "data_short.RData")
