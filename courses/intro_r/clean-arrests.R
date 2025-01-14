# script to clean up arrests file for intro to r course
 
library(tidyverse)

arrests_raw <- read_csv(
  "courses/intro_r/arrests-raw.csv",
  col_types = cols(.default = "c")
  )

set.seed(782)

arrests_clean <- arrests_raw |> 
  janitor::clean_names() |>
  mutate(
    arrest_date = str_remove(arrest_date, " 0:00"),
    booking_date = str_remove(booking_date, " 0:00"),
    arrest_time = str_pad(time, 4, pad = "0"),
    arrest_time = paste0(str_sub(arrest_time, 1, 2), ":", str_sub(arrest_time, 3, 4)),
    booking_time = str_pad(booking_time, 4, pad = "0"),
    booking_time = paste0(str_sub(booking_time, 1, 2), ":", str_sub(booking_time, 3, 4))
    ) |> 
  filter(arrest_type_code %in% c("I", "M", "F")) |> 
  drop_na() |> 
  select(-sex_code) |> 
  sample_n(1000) |> 
  mutate(arrest_id = row_number()) |> 
  select(
    arrest_id,
    arrest_date,
    arrest_time,
    booking_date,
    booking_time,
    arrest_type = arrest_type_code,
    charge_group = charge_group_description
  )

arrests_clean |> 
  write_csv("courses/intro_r/arrests.csv")
