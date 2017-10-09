# works well to explain variation across room types
fit3 <- lm(data$price ~ 
             data$bathrooms + 
             data$bedrooms + 
             data$room_type + 
             data$accommodates)
summary(fit3)

# doesn't work well for explaining variation across just single rooms 
# or whatever it was that we selected as the only type to look at
fit4 <- lm(data_short$price ~ 
             data_short$bathrooms + 
             data_short$bedrooms + 
             data_short$accommodates)
summary(fit4)
