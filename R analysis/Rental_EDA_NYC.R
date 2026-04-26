library("readr")

library("dplyr")
library("glmnet")

library("stats")


full_rentalDataset_2016_FILTERED_updated <- read_csv("/Users/youktarai/Documents/RStudio_savingDirectory/secondCapstoneProject_Housing/GroupProject(Youkta, Peter, William)/dataset_USING/full_rentalDataset_2016_FILTERED_updated.csv")
temp <- full_rentalDataset_2016_FILTERED_updated

summary(full_rentalDataset_2016_FILTERED_updated)


### Small Data Cleaning 
 temp <- full_rentalDataset_2016_FILTERED_updated %>%
   filter(price < 175000) %>%
   filter(price > 80)
   
temp <- temp %>%
   mutate(bathbeddiff = (bathrooms - bedrooms)) 
 
temp <- temp %>%
   filter(bathbeddiff < 2) %>%
   select(-bathbeddiff)

summary(temp)
 
 
 ### RIDGE / LASSO Method
  #finds optimal lambda to use to get the best coefficient(weights) -> "lambda.min"
    # Lambda helps to tilt the slope in hopes to find possible best fits for a linear model.
      # An attempt to minimize the residual, thus increasing the accuracy of prediction 
 set.seed(1)
 
 #data split
 train_idx <- sample(1:nrow(temp), 0.8 * nrow(temp))
 
 train <- temp[train_idx, ]
 test  <- temp[-train_idx, ]
 
 #cool thing about model.matrix --> does standardization for me!!
 X_train <- model.matrix(price ~ bathrooms + bedrooms + interest_level + 
                           `outdoor space` + dishwasher + elevator + pool + furnished + `in unit wash/dryer` +
                           `in building wash/dryer` + `fitness center` + `parking spot` + `pet policy` +
                           doorman + near_restaurants_1_5_miles + moderate_restaurants_3_miles +
                           far_restaurants_7_5_miles , data = train)[, -1]
 y_train <- train$price
 
 X_test  <- model.matrix(price ~ bathrooms + bedrooms + interest_level + 
                           `outdoor space` + dishwasher + elevator + pool + furnished + `in unit wash/dryer` +
                           `in building wash/dryer` + `fitness center` + `parking spot` + `pet policy` +
                           doorman + near_restaurants_1_5_miles + moderate_restaurants_3_miles +
                           far_restaurants_7_5_miles, data = test)[, -1]
 y_test  <- test$price
 
 
 ## RIDGE
 
 #Used to "train" the model
 cv_model_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
 #the coefficients aka "weights" of the features
 coef(cv_model_ridge, s = "lambda.min")
 
 plot(glmnet(X_train, y_train, alpha = 0))
 plot(cv_model_ridge)
 
 pred_price_using_ridge <- predict(cv_model_ridge, newx = X_test, s = "lambda.min")

 
## LASSO 
 
#used to "train" the model
cv_model_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
#the coefficients aka "weights" of the features
coef(cv_model_lasso, s = "lambda.min")

plot(glmnet(X_train, y_train, alpha =1))
plot(cv_model_lasso)

pred_price_using_lasso <- predict(cv_model_lasso, newx = X_test, s = "lambda.min")


## ^^^ problems above some features are cuasing problems? 
  # bathrooms !!



library("randomForest")
## Random Forest could be a possibility

names(train) <- make.names(names(train))
names(test) <- make.names(names(test))

names(train)

#Fit Model
rf_model_complete <- randomForest(price ~ bathrooms + bedrooms + interest_level + outdoor.space + dishwasher + elevator + pool + furnished + 
                           in.unit.wash.dryer + in.building.wash.dryer + fitness.center + parking.spot + pet.policy +
                           doorman + near_restaurants_1_5_miles + moderate_restaurants_3_miles, data = train, ntree = 500, mtry = 3, importance = TRUE)


pred_price_using_rf_complete <- predict(rf_model_complete, newdata = test)
pred_price_using_rf_complete[0:20]

importance(rf_model_complete)
varImpPlot(rf_model_complete)



## Model rf_model_completed found : ["furnished", "pet.policy"] unneeded
  # After rerun in rf_model I found : ["pool", "parking.spot"] unneeded
    # Found that having pool and pet.policy made model r^2 score better 
rf_model <- randomForest(price ~ bathrooms + bedrooms + interest_level + outdoor.space + dishwasher + elevator + 
                                    in.unit.wash.dryer + in.building.wash.dryer + fitness.center + pool +  pet.policy +
                                    doorman + near_restaurants_1_5_miles + moderate_restaurants_3_miles, data = train, ntree = 500, mtry = 3, importance = TRUE)


pred_price_using_rf <- predict(rf_model, newdata = test)
pred_price_using_rf[0:20]

importance(rf_model)
varImpPlot(rf_model)


checkPrice <- data.frame(Ridge = pred_price_using_ridge, Lasso = pred_price_using_lasso, random_forest_full = pred_price_using_rf_complete, random_forest_updated = pred_price_using_rf, ActualPrice = test$price)


 true_price <- test$price

r2_ridge <- 1 - sum((true_price - pred_price_using_ridge)^2) / sum((true_price - mean(true_price))^2)
r2_lasso <- 1 - sum((true_price - pred_price_using_lasso)^2) / sum((true_price - mean(true_price))^2)
r2_rf_complete <-  1 - sum((true_price - pred_price_using_rf_complete)^2) / sum((true_price - mean(true_price))^2)
re_rf<-  1 - sum((true_price - pred_price_using_rf)^2) / sum((true_price - mean(true_price))^2)


#pairs(train[, c("bedrooms", "bathrooms", "price")])


#price = e^(...)

#1.) create a switch counter  [12v]

# 2.) Build a circuit in the simulator that counts the number of switches in the ON position
  # and turn on the appropriate number of LED from left to right

#3.) Build something COOL!




