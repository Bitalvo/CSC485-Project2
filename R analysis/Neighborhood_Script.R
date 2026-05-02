# Script to loan in, clean and join the neighborhood datasets
#Update

# avg_rent <- read.csv("C:\\Users\\pbita\\Downloads\\Neighborhood Data\\Neighborhood_Avg_Rent.csv")

library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)

# Load
rent <- read_csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_Avg_Rent.csv") |> clean_names()
school <- read_csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_School_Rank.csv", skip = 5) |> clean_names()
walk <- read_excel("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_Walk_Score.xlsx") |> clean_names()

income <- read_csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_Median_Incomes.csv",
  skip = 11,
  col_names = c("location", "household_type", "timeframe", "data_format", "data", "fips")
) |> clean_names()

clean_neighborhood <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", "") |>  # remove punctuation
    str_squish()
}

#Clean Rent Data
rent_clean <- rent |>
  rename(neighborhood = submarket) |>
  mutate(
    neighborhood = clean_neighborhood(neighborhood),
    
    avg_asking_rent = market_asking_rent_unit |>
      str_remove_all("\\$") |>
      str_remove_all(",") |>
      as.numeric(),
    
    market_sale_price_unit = market_sale_price_unit |>
      str_remove_all("\\$") |>
      str_replace_all("K", "000") |>
      str_remove_all(",") |>
      as.numeric(),
    
    inventory_units = invintory_units,
    
    asset_value_billions = asset_value |>
      str_remove_all("\\$") |>
      str_replace_all("B", "") |>
      as.numeric(),
    
    vacancy_rate = vacancy_rate |>
      str_remove_all("%") |>
      as.numeric()
  ) |>
  filter(!is.na(avg_asking_rent)) |>
  filter(!str_detect(neighborhood, "jersey|county|hoboken|hackensack|yonkers|white plains|bergen|passaic|rockland|putnam")) |>
  select(
    neighborhood,
    avg_asking_rent,
    market_sale_price_unit,
    inventory_units,
    asset_value_billions,
    vacancy_rate
  )

#Test
summary(rent_clean)
head(rent_clean)

#JOIN
master_step2 <- rent_clean |>
  left_join(income_clean, by = "neighborhood")

master_step3 <- master_step2 |>
  left_join(education_clean, by = "neighborhood")

master_final <- master_step3 |>
  left_join(walk_clean, by = "neighborhood")

summary(master_final)

#Subway data
subway <- read_csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_Subway_Stations.csv") |>
  clean_names()

subway_clean <- subway |>
  mutate(
    borough_clean = case_when(
      borough == "M" ~ "manhattan",
      borough == "Bk" ~ "brooklyn",
      borough == "Q" ~ "queens",
      borough == "Bx" ~ "bronx",
      borough == "SI" ~ "staten island"
    ),
    
    # count number of routes per station
    route_count = str_count(daytime_routes, "[A-Za-z0-9]")
  ) |>
  
  group_by(borough_clean) |>
  summarise(
    subway_station_count = n_distinct(station_id),
    avg_routes_per_station = mean(route_count, na.rm = TRUE),
    percent_ada_accessible = mean(ada, na.rm = TRUE),
    .groups = "drop"
  )


#Map neighborhoods
rent_borough_lookup <- tribble(
  ~neighborhood, ~borough_clean,
  
  # Manhattan
  "financial district", "manhattan",
  "chelsea", "manhattan",
  "upper west side", "manhattan",
  "lower west side", "manhattan",
  "murray hillkips bay", "manhattan",
  "midtown south", "manhattan",
  "east village", "manhattan",
  "midtown west", "manhattan",
  "midtown east", "manhattan",
  "upper east side", "manhattan",
  "lower east side", "manhattan",
  "little italychinatown", "manhattan",
  "morningside heights", "manhattan",
  "east harlem", "manhattan",
  "harlem", "manhattan",
  "upper manhattan", "manhattan",
  "roosevelt island", "manhattan",
  
  # Brooklyn
  "williamsburg", "brooklyn",
  "downtown brooklyn", "brooklyn",
  "bushwick", "brooklyn",
  "flatbush", "brooklyn",
  "prospect park", "brooklyn",
  "east new york", "brooklyn",
  "south shore brooklyn", "brooklyn",
  "southwest brooklyn", "brooklyn",
  "southeast brooklyn", "brooklyn",
  
  # Queens
  "long island city", "queens",
  "northwestern queens", "queens",
  "central queens", "queens",
  "northeast queens", "queens",
  "southeast queens", "queens",
  "south shore queens", "queens",
  
  # Bronx
  "west bronx", "bronx",
  "east bronx", "bronx",
  "northwest bronx", "bronx",
  "south bronx", "bronx",
  "southeast bronx", "bronx",
  
  # Staten Island
  "staten island", "staten island"
)

master_final <- master_final |>
  left_join(rent_borough_lookup, by = "neighborhood") |>
  left_join(subway_clean, by = "borough_clean") |>
  select(-borough_clean)

master_final <- master_final |>
  select(
    -subway_station_count.y,
    -avg_routes_per_station.y,
    -percent_ada_accessible.y
  ) |>
  rename(
    subway_station_count = subway_station_count.x,
    avg_routes_per_station = avg_routes_per_station.x,
    percent_ada_accessible = percent_ada_accessible.x
  )


#Resturants 
restaurants <- read_csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_Resturants.csv") |>
  clean_names()

restaurants_clean <- restaurants |>
  mutate(
    inspection_date = as.Date(inspection_date, format = "%m/%d/%Y"),
    boro_clean = clean_neighborhood(boro),
    boro_clean = case_when(
      boro_clean == "manhattan" ~ "manhattan",
      boro_clean == "brooklyn" ~ "brooklyn",
      boro_clean == "queens" ~ "queens",
      boro_clean == "bronx" ~ "bronx",
      boro_clean == "staten island" ~ "staten island",
      TRUE ~ NA_character_
    )
  ) |>
  filter(
    !is.na(boro_clean),
    !is.na(camis),
    inspection_date > as.Date("1900-01-01")
  ) |>
  arrange(camis, desc(inspection_date)) |>
  group_by(camis) |>
  slice(1) |>
  ungroup()

restaurant_features <- restaurants_clean |>
  rename(borough_clean = boro_clean) |>
  group_by(borough_clean) |>
  summarise(
    restaurant_count = n_distinct(camis),
    cuisine_diversity = n_distinct(cuisine_description, na.rm = TRUE),
    avg_restaurant_score = mean(score, na.rm = TRUE),
    percent_a_grade = mean(grade == "A", na.rm = TRUE),
    critical_violation_share = mean(critical_flag == "Critical", na.rm = TRUE),
    .groups = "drop"
  )

master_final <- master_final |>
  left_join(rent_borough_lookup, by = "neighborhood") |>
  left_join(restaurant_features, by = "borough_clean") |>
  select(-borough_clean)

#Green Space
green <- read_csv("C:\\Users\\pbita\\OneDrive - Providence College\\--DataSci Capstone\\CSC485-Project2\\Neighborhood Data\\Neighborhood_Green_Space.csv") |>
  clean_names()

green_clean <- green |>
  mutate(
    borough_clean = case_when(
      borough == "M" ~ "manhattan",
      borough == "Q" ~ "queens",
      borough == "R" ~ "staten island",
      borough == "B" ~ "brooklyn",
      borough == "X" ~ "bronx",
      TRUE ~ NA_character_
    ),
    waterfront = as.numeric(waterfront)
  ) |>
  filter(!is.na(borough_clean)) |>
  group_by(borough_clean) |>
  summarise(
    green_space_count = n(),
    total_green_acres = sum(acres, na.rm = TRUE),
    avg_green_acres = mean(acres, na.rm = TRUE),
    percent_waterfront = mean(waterfront, na.rm = TRUE),
    .groups = "drop"
  )

master_final <- master_final |>
  left_join(rent_borough_lookup, by = "neighborhood") |>
  left_join(green_clean, by = "borough_clean") |>
  select(-borough_clean)

#Summary
summary(master_final)

sum(is.na(master_final$green_space_count))
sum(is.na(master_final$total_green_acres))
sum(is.na(master_final$avg_green_acres))
sum(is.na(master_final$percent_waterfront))


#############################################################################

master_final <- rent_clean |>
  left_join(income_clean, by = "neighborhood") |>
  left_join(education_clean, by = "neighborhood") |>
  left_join(walk_clean, by = "neighborhood") |>
  left_join(rent_borough_lookup, by = "neighborhood") |>
  left_join(subway_clean, by = "borough_clean") |>
  left_join(restaurant_features, by = "borough_clean") |>
  left_join(green_clean, by = "borough_clean") |>
  select(-borough_clean)

summary(master_final)

View(master_final)


write.csv(master_final, 
          "C:\\Users\\pbita\\Downloads\\master_final.csv", 
          row.names = FALSE)
