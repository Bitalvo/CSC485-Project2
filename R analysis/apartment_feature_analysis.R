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

apartments_full <- apartments_filtered |>
  left_join(crosswalk_clean, by = "zip") |>
  left_join(neighborhood_features, by = c("neighborhood_master" = "neighborhood"))


# That looks mostly good, thats the apartment features
# Now we will look at the neighborhood features
neighborhood_features <- read.csv("master_final.csv")

head(neighborhood_features)

# features: 
# neighborhood
# average asking rent
# market sale price unit
# inventory units
# assest value billions
# vacancy rate
# median income

# early education enrollment
# reading test scores 3rd to 8th grade
# math test scores 3rd to 8th grade
# graduation rate

# walk score
# transit score
# bike score
# subway station count
# average routes per station
# percent ada accessible

# restaurant count
# cuisine diversity
# avg restaurant score
# percent a grade
# critical violation share

# green space count
# total green acres
# average green acres
# percent waterfront




