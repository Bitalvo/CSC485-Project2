library(dplyr)
apartments <- read.csv("full_rentalDataset_2016_FILTERED.csv")

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

apartments_lm1 <- lm(price ~ bedrooms + bathrooms + bedrooms * bathrooms + pool + furnished + parking.spot + outdoor.space + dishwasher + elevator + in.unit.wash.dryer + in.building.wash.dryer + fitness.center + pet.policy + doorman, data=apartments)


