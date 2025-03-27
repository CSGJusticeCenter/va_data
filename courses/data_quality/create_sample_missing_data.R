# ---------------------------------- #
# Data Quality - Lesson 7: Missing Data
# ---------------------------------- #
#
# This dataset recreates rates of missing data that 
#   you may encounter as part of your work. 
# The frequency of missing values in each variable is
#   based on actual data analysis, but the values 
#   themselves are randomly assigned to each row.
#
# total N: 10,000
# variables: 6
# ---------------------------------- #

# ---------------------------------- #
# Load packages
library(tidyverse)

# set seed for random values
set.seed(756331248)

# ---------------------------------- #
# create dataframe with gender & risk value distributions

df_gender_risk <- setNames(data.frame(matrix(ncol = 1, nrow = 10000)), 
                    c("gender")) |> 
  # assign values to rows to replicate frequencies 
  mutate(gender = case_when(row_number() %in% 1:1427 ~ "Female",
                            row_number() %in% 1428:9568 ~ "Male",
                            TRUE ~ NA_character_)) |> 
  # flag rows with missing gender
  mutate(gender_na = ifelse(is.na(gender), 1, 0)) |> 
  # sort records so missing gender rows are FIRST 
  arrange(-gender_na) |> 
  # assign risk level values to rows to replicate frequencies by gender
  # (first range is rows with missing gender; second is rows with gender)
  mutate(risk_level = case_when(
    row_number() %in% c(1:241, 433:870) ~ "Low",
    row_number() %in% c(242:341, 871:5010) ~ "Medium",
    row_number() %in% c(342:425, 5011:9524) ~ "High",
    TRUE ~ NA_character_)) |> 
  # set risk_level as factor for sorting
  mutate(risk_level = factor(risk_level, 
                             levels = c("Low", "Medium", "High"))) |> 
  # randomly assign ID number to each row
  mutate(id = sample(row_number()))

# ---------------------------------- #
# create dataframe with race value distribution
# create separate df bc we want a different random order for values

df_race <- setNames(data.frame(matrix(ncol = 1, nrow = 10000)), 
                    c("race")) |> 
  # assign values to rows to replicate frequencies
  mutate(race = case_when(
    row_number() %in% 1:402 ~ "American Indian or Alaskan Native",
    row_number() %in% 403:445 ~ "Asian or Pacific Islander",
    row_number() %in% 446:768 ~ "Black",
    row_number() %in% 769:943 ~ "Unknown",
    row_number() %in% 944:9564 ~ "White",
    TRUE ~ NA_character_)) |> 
  # randomly assign ID number to each row
  mutate(id = sample(row_number()))

# ---------------------------------- #
# create dataframe with hearing_type value distribution
# create separate df bc we want a different random order for values

df_hearings <- setNames(data.frame(matrix(ncol = 1, nrow = 10000)), 
                        c("hearing_type")) |> 
  # assign values to rows to replicate hearing_type frequencies
  mutate(hearing_type = case_when(
    row_number() %in% 1:216 ~ "Disciplinary", 
    row_number() %in% 217:243 ~ "Medical",
    row_number() %in% 244:1036 ~ "Other",
    row_number() %in% 1037:6104 ~ "Regular",
    row_number() %in% 6105:8630 ~ "Revocation",
    TRUE ~ NA_character_)) |> 
  # create flag for rows missing hearing type
  mutate(hearing_na = ifelse(is.na(hearing_type), 1, 0)) |>   
  # sort records so missing hearing_type rows are LAST 
  arrange(hearing_na) |> 
  # assign hearing_year values to rows to replicate frequencies by hearing_type
  mutate(hearing_year = case_when(
    ## rows WITH hearing_type
    row_number() %in% c(1:922) ~ 2016,
    row_number() %in% c(923:1973) ~ 2017,
    row_number() %in% c(1974:3419) ~ 2019,
    row_number() %in% c(3420:4669) ~ 2020,
    row_number() %in% c(4670:5942) ~ 2021,
    row_number() %in% c(5943:7314) ~ 2022,
    row_number() %in% c(7315:8630) ~ 2023,
    ## all rows WITHOUT hearing_type == 2018  
    hearing_na == 1 ~ 2018,
    TRUE ~ NA_integer_)) |> 
  # randomly assign ID number to each row
  mutate(id = sample(row_number())) 

# ---------------------------------- #
# merge dfs to create dataset
missing_data <- df_gender_risk |> 
  left_join(df_race, by = "id") |> 
  left_join(df_hearings, by = "id") |>
  select(id, gender, race, hearing_year, hearing_type, risk_level) |> 
  arrange(id)

# check values 
lapply(missing_data |> 
         select(gender, race, hearing_year, hearing_type, risk_level),
       function(x) table(x, useNA = "ifany"))

# save file as csv
write.csv(missing_data, file = ("missing_data.csv"), row.names = FALSE)
