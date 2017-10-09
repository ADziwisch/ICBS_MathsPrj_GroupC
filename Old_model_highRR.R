# works well to explain variation across room types, R^2 of 0.52
fit3 <- lm(data$price ~ 
             data$bathrooms + 
             data$bedrooms + 
             data$room_type + 
             data$accommodates)
summary(fit3)

# doesn't work well for explaining variation across just single rooms 
# or whatever it was that we selected as the only type to look at
# R^2 of 0.15
fit4 <- lm(data_short$price ~ 
             data_short$bathrooms + 
             data_short$bedrooms + 
             data_short$accommodates)
summary(fit4)
