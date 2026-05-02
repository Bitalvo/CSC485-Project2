library(dplyr)
apartments <- read.csv("full_rentalDataset_2016_FILTERED_updated.csv")

car::vif(apartments_lm)
# No multicolinearity problems
plot(apartments_lm)
# Residuals vs fitted looks good, no multicolinearity problems I can see
# Fat tailed distribution still, need to do something about that (log-log the log)
# Looking into observations 1831, 49284, 47009, 38559, 41570
head(apartments)

apartments_filtered <- apartments |> filter(price <= 35000 & price > 80) 

nrow(apartments_filtered[apartments_filtered$bathrooms == 0,])
# 156 apartments with no bathrooms, considering because this will have very high leverage on our data. 

range(apartments_filtered[apartments_filtered$bedrooms == 0,]$price)
# 695 25000, 25000 seems a bit expensive for a studio, will remove studios with price > 12000, very unlikely a studio is that expensive
apartments_filtered <- apartments_filtered |> filter(!(bedrooms == 0 & price > 8500))

apartments_filtered <- apartments_filtered |> mutate(ppbed = (price / bedrooms)) |> filter(ppbed > 500) |> select(-ppbed)

apartments_filtered <- apartments_filtered |> mutate(bathbeddiff = (bathrooms - bedrooms)) |> filter(bathbeddiff < 2) |> select(-bathbeddiff)


#               with feature  without feature
# doorman       21019         28333
# outdoor space 13226         36126
# dishwasher    20804         28548
# elevator      26272         23080
# pool          3029          46323
# parking spot  1430          47922
# furnished     690           48662
# in unit wash  9531          39821
# building wash 19957         19957
# no wash/dry   25186         24166 
# gym           36074         13278
# pets allowed  25678         25678
# 

# basic, ONLY APARTMENT FEATURE lm
# logged y because dataset has right skew
apartments_lm <- lm(log(price) ~ bedrooms + bathrooms + pool + parking.spot + outdoor.space + dishwasher + elevator + in.unit.wash.dryer + in.building.wash.dryer + fitness.center + doorman + pet.policy + near_restaurants_1_5_miles + moderate_restaurants_3_miles, data=apartments_filtered)
summary(apartments_lm)

plot(apartments_lm)
# Data cleaning round 1)
# 41533, 39670, 15469  
apartments_filtered[41533,] # Looks like a bit of a problem: 10 bathrooms, 2 bedrooms
apartments_filtered[39670,] # Looks fine
apartments_filtered[15469,] # Looks fine too 

apartments_lm2 <- lm(log(price) ~ bedrooms + bathrooms + elevator + in.unit.wash.dryer + in.building.wash.dryer + fitness.center + doorman + near_restaurants_1_5_miles, data=apartments_filtered)
summary(apartments_lm2)


# Round 2)
# 21247, 31297, 10647
apartments_filtered[21247, ]
apartments_filtered[31297, ]
apartments_filtered[10647, ]

# re-adding zipcodes back into the apartments_filtered dataset
# do this by long and lat
library(sf)
library(tigris)
apartments_filtered <- apartments_filtered |> rename(zip = zipcode)
apartments_filtered <- apartments_filtered |> mutate(zip = as.character(zip))

# That looks mostly good, thats the apartment features
# Now we will look at the neighborhood features
neighborhood_features <- read.csv("master_final_with_scores.csv")

apartments_full <- apartments_filtered |>
  left_join(crosswalk_clean, by = "zip") |>
  left_join(neighborhood_features, by = c("neighborhood_master" = "neighborhood"))


apartments_full <- apartments_full |> mutate(neighborhood = neighborhood_master)

head(full_data)

library(dplyr)
library(stringr)
median_income <- read.csv("NYC median incomes.csv")
# head(median_income, 1)
median_income <- median_income |> select(NAME, S1903_C03_001E)
median_income <- median_income |>
  mutate(
    zip = str_replace(NAME, "ZCTA5 ", ""),                   
    median_income_census = as.numeric(S1903_C03_001E)
  ) |>
  select(zip, median_income_census)

head(median_income, 5)

median_income |> select(median_income_census)

write.csv(median_income, "NYC_median_income.csv", row.names=FALSE)

head(apartments_full)
apartments_full <- apartments_full |>
  left_join(median_income, by = "zip") |>
  select(-median_income) |>             
  rename(median_income = median_income_census) 

apartments_full |> filter(is.na(median_income)) 

apartments_full <- apartments_full |>
  group_by(neighborhood_master) |>
  mutate(
    median_income = ifelse(
      is.na(median_income),
      median(median_income, na.rm = TRUE),
      median_income
    )
  ) |>
  ungroup()

sum(is.na(apartments_full$median_income))

apartments_full <- apartments_full |> filter(!(bedrooms == 0 & bathrooms == 0 & price >= 8000))

apartments_full |> filter(bedrooms == 0 & bathrooms == 0) |> arrange(desc(price))

library(geosphere)

apartments_full <- apartments_full |>
  mutate(
    dist_to_cbd_km = distHaversine(
      cbind(longitude, latitude),
      c(-74.0134, 40.7128)
    ) / 1000
  )
head(apartments_full[,'dist_to_cbd_km'])


n_distinct(apartments_full$neighborhood_master)

# bathroom, bedroom, elevator, in.unit.wash.dryer, in.building.wash.dryer, fitness.center, outdoor.space, doorman, near_restaurants_1_5_miles, moderate_restaurants_3_miles, vacancy_rate, median_income


full_model <- lm(log(price) ~ bathrooms + bedrooms + elevator + in.unit.wash.dryer + in.building.wash.dryer + fitness.center + outdoor.space + doorman + near_restaurants_1_5_miles + moderate_restaurants_3_miles + vacancy_rate + median_income, data=apartments_full)
summary(full_model)
plot(full_model)
car::vif(full_model)

# Very extreme hetroskedasticity. Need to look at the CRPlots and transform variables!
car::crPlots(full_model)

apartments_full[15448,]
apartments_full[22572,]

apartments_full[44423,]

# full_model 2, adding quadratic term for bathrooms, and a log term for median_income
# recoding the categorical variables to be interpreted categorically, rather than as continuous variables
apartments_full <- apartments_full |>
  mutate(
    elevator = factor(elevator),
    doorman = factor(doorman),
    fitness.center = factor(fitness.center),
    in.unit.wash.dryer = factor(in.unit.wash.dryer),
    in.building.wash.dryer = factor(in.building.wash.dryer),
    outdoor.space = factor(outdoor.space),
    pool = factor(pool),
    parking.spot = factor(parking.spot),
    dishwasher = factor(dishwasher),
    furnished = factor(furnished)
  )

full_model2 <- lm(log(price) ~ bathrooms + I(bathrooms^2) + bedrooms + elevator + in.unit.wash.dryer + in.building.wash.dryer + fitness.center + outdoor.space + doorman + near_restaurants_1_5_miles + moderate_restaurants_3_miles + vacancy_rate + log(median_income) + dist_to_cbd_km, data=apartments_full)
summary(full_model2)

full_model3 <- lm(log(price) ~ bathrooms * dist_to_cbd_km + 
               I(bathrooms^2) * dist_to_cbd_km +
               bedrooms * dist_to_cbd_km + 
               elevator * dist_to_cbd_km + 
               in.unit.wash.dryer * dist_to_cbd_km + 
               in.building.wash.dryer + 
               fitness.center * dist_to_cbd_km + 
               outdoor.space * dist_to_cbd_km + 
               doorman * dist_to_cbd_km + 
               near_restaurants_1_5_miles + moderate_restaurants_3_miles + 
               vacancy_rate + log(median_income), data = apartments_full)
anova(full_model2, full_model3)
summary(full_model3)

plot(full_model3)

car::vif(full_model3, type='predictor')

cor(apartments_full$dist_to_cbd_km, log(apartments_full$median_income), use = "complete.obs")

model4 <- update(full_model3, . ~ . - log(median_income))
car::vif(model4, type='predictor')

apartments_full <- apartments_full %>%
  mutate(bathrooms_c = bathrooms - mean(bathrooms))

apartments_full <- apartments_full %>%
  mutate(dist_to_cbd_c = dist_to_cbd_km - mean(dist_to_cbd_km))

model3 <- lm(log(price) ~ bathrooms_c  * dist_to_cbd_c  + 
               I(bathrooms_c ^2)  +
               bedrooms * dist_to_cbd_c  + 
               elevator * dist_to_cbd_c  + 
               in.unit.wash.dryer * dist_to_cbd_c  + 
               in.building.wash.dryer + 
               fitness.center * dist_to_cbd_c  + 
               outdoor.space * dist_to_cbd_c  + 
               doorman * dist_to_cbd_c  + 
               near_restaurants_1_5_miles + moderate_restaurants_3_miles + 
               vacancy_rate + median_income, data = apartments_full) 
summary(model3)
car::vif(model3, type='predictor')

model4 <- lm(log(price) ~ bathrooms_c  * dist_to_cbd_c  + 
                         I(bathrooms_c ^2)  +
                         bedrooms * dist_to_cbd_c  + 
                         elevator * dist_to_cbd_c  + 
                         in.unit.wash.dryer * dist_to_cbd_c  + 
                         in.building.wash.dryer + 
                         fitness.center * dist_to_cbd_c  + 
                         outdoor.space * dist_to_cbd_c  + 
                         doorman * dist_to_cbd_c  + 
                         moderate_restaurants_3_miles + 
                         vacancy_rate + median_income, data = apartments_full) 
summary(model4)
car::vif(model4, type='predictor')
plot(model4)


# still looks pretty bad so 
library(lmtest)
library(sandwich)
library(MASS)

coeftest(full_model2, vcov = vcovHC(full_model2, type = "HC1"))
# outdoor.space no longer significant

robust_model <- rlm(log(price) ~ bathrooms_c  * dist_to_cbd_c  + 
                      I(bathrooms_c ^2)  +
                      bedrooms * dist_to_cbd_c  + 
                      elevator * dist_to_cbd_c  + 
                      in.unit.wash.dryer * dist_to_cbd_c  + 
                      in.building.wash.dryer + 
                      fitness.center * dist_to_cbd_c  + 
                      outdoor.space * dist_to_cbd_c  + 
                      doorman * dist_to_cbd_c  + 
                      moderate_restaurants_3_miles + 
                      vacancy_rate + median_income, data = apartments_full)
summary(robust_model)
car::crPlots(robust_model)

apartments_full <- apartments_full |> filter(!is.na(neighborhood_master))

plot(robust_model)