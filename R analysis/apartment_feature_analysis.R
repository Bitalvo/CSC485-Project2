library(dplyr)
apartments <- read.csv("full_rentalDataset_2016_FILTERED_updated.csv")

apartments_filtered <- apartments |> filter(price < 200000 & price > 80) 
apartments_filtered |> 
  arrange(price) |> 
  select(price, street_address) |>
  head(5)

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

# basic, ONLY APARTMENT FEATURE lm
# logged y because dataset has right skew
apartments_lm <- lm(log(price) ~ bedrooms + bathrooms + pool + furnished + parking.spot + outdoor.space + dishwasher + elevator + in.unit.wash.dryer + in.building.wash.dryer + fitness.center + pet.policy + doorman, data=apartments_filtered)
summary(apartments_lm)

car::vif(apartments_lm)
# No multicolinearity problems
plot(apartments_lm)
# Residuals vs fitted looks good, no multicolinearity problems I can see
# Fat tailed distribution still, need to do something about that (log-log the log)
# Looking into observations 1831, 49284, 47009, 38559, 41570
apartments |> slice(1831, 49284, 47009, 38559, 41570)



