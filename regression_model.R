m0 <- lm(price ~ mean_rent +
           distance +
           accommodates + 
           review_scores_rating +
           review_scores_cleanliness +
           review_scores_location +
           review_scores_value +
           beds +
           `amen_Family/kid_friendly` +
           amen_TV +
           amen_Elevator_in_building +
           amen_Dryer +
           amen_Kitchen +
           amen_Washer +
           amen_Lock_on_bedroom_door +
           instant_bookable +
           cancellation_policy,
         data = data_short)

summary(m0)

m1 <- lm(log10(price) ~ mean_rent +
                          distance +
                          accommodates + 
                          review_scores_rating +
                          review_scores_cleanliness +
                          review_scores_location +
                          review_scores_value +
                          beds +
                          `amen_Family/kid_friendly` +
                          amen_TV +
                          amen_Elevator_in_building +
                          amen_Dryer +
                          amen_Kitchen +
                          amen_Washer +
                          amen_Lock_on_bedroom_door +
                          instant_bookable +
                          cancellation_policy,
         data = data_short)

summary(m1)

m2 <- lm(log10(price) ~ mean_rent +
           distance +
           accommodates + 
           review_scores_rating +
           review_scores_cleanliness +
           review_scores_location +
           review_scores_value +
           amen_TV +
           amen_Elevator_in_building +
           amen_Dryer +
           amen_Washer +
           amen_Lock_on_bedroom_door,
         data = data_short)

summary(m2)