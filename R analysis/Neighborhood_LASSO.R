##Lasso analysis

library(dplyr)
library(glmnet)

master_final <- read.csv("C:\\Users\\pbita\\OneDrive - Providence College\\CSC485 Project 2\\Neighborhood Data\\master_final.csv")

lasso_df <- master_final |>
  select(
    avg_asking_rent,
    
    median_income,
    market_sale_price_unit,
    inventory_units,
    vacancy_rate,
    
    walk_score,
    transit_score,
    bike_score,
    
    early_education_enrollment,
    reading_test_scores_3rd_to_8th_grade,
    math_test_scores_3rd_to_8th_grade,
    graduation_rate,
    
    subway_station_count,
    avg_routes_per_station,
    percent_ada_accessible,
    
    restaurant_count,
    cuisine_diversity,
    avg_restaurant_score,
    
    green_space_count,
    total_green_acres,
    percent_waterfront
  )


lasso_df <- lasso_df |>
  mutate(across(
    -avg_asking_rent,
    ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)
  ))


x <- model.matrix(avg_asking_rent ~ ., data = lasso_df)[, -1]
y <- lasso_df$avg_asking_rent

set.seed(123)

lasso_model <- cv.glmnet(x, y, alpha = 1)

plot(lasso_model)

coef(lasso_model, s = "lambda.min")



#Correlation   NOT DONE YET
library(corrplot)

cor_matrix <- cor(lasso_df)
corrplot(cor_matrix, method = "color", type = "upper")


#Linear Regression
lm1 <- lm(avg_asking_rent ~ median_income, data = lasso_df)
summary(lm1)

lm2 <- lm(avg_asking_rent ~ market_sale_price_unit, data = lasso_df)
summary(lm2)


#Multiple Regression
final_model <- lm(
  avg_asking_rent ~ median_income +
    market_sale_price_unit +
    bike_score +
    early_education_enrollment +
    total_green_acres,
  data = lasso_df
)

summary(final_model)
