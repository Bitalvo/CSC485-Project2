##Lasso analysis

library(dplyr)
library(glmnet)

master_final <- read.csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\data sources\\master_final.csv")

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



#Correlation
library(corrplot)

#force everything to be numeric - I don't remember if it is so for saftey
lasso_df <- lasso_df |>
  mutate(across(everything(), as.numeric))

# get rid of the borough level features that just repeat 
lasso_df_var <- lasso_df |>
  select(where(~ sd(., na.rm = TRUE) > 0))

cor_matrix <- cor(lasso_df_var, use = "complete.obs")

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.cex = 0.7
)

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

View(master_final)


#Alternate Model 
# The goal in this section is to create a combine score for a category. For example...
# we will calculate an overall education score insted of using all the individual values.
# We will do the same for the following categories: Education, ease of movement, restaurants, green space.

#First step is to make sure the features have the same scale or units so we can combine data

scaled_df <- master_final |>
  mutate(across(
    where(is.numeric),
    ~ as.numeric(scale(.))
  ))

#Education
scaled_df <- scaled_df |>
  mutate(
    education_score = rowMeans(
      across(
        c(
          early_education_enrollment,
          reading_test_scores_3rd_to_8th_grade,
          math_test_scores_3rd_to_8th_grade,
          graduation_rate
        )
      ),
      na.rm = TRUE
    )
  )

#Walkability
scaled_df <- scaled_df |>
  mutate(
    movement_score = rowMeans(
      across(
        c(
          walk_score,
          transit_score,
          bike_score,
          subway_station_count,
          avg_routes_per_station
        )
      ),
      na.rm = TRUE
    )
  )

#Resturants
scaled_df <- scaled_df |>
  mutate(
    restaurant_score = rowMeans(
      across(
        c(
          restaurant_count,
          cuisine_diversity,
          avg_restaurant_score,
          percent_a_grade,
          critical_violation_share
        )
      ),
      na.rm = TRUE
    )
  )

#Green-score
scaled_df <- scaled_df |>
  mutate(
    green_score = rowMeans(
      across(
        c(
          green_space_count,
          total_green_acres,
          percent_waterfront
        )
      ),
      na.rm = TRUE
    )
  )

summary(scaled_df$education_score)

View(scaled_df)

#Education still has a ton of NA values... so were goign to do a model without education
feature_df <- scaled_df |>
  select(
    avg_asking_rent,
    median_income,
   # market_sale_price_unit,
    vacancy_rate,
    
    movement_score,
    restaurant_score,
    green_score
  )

final_model2 <- lm(
  avg_asking_rent ~ .,
  data = feature_df
)

summary(final_model2)
