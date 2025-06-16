library(tidyverse)
library(csgjcr)
library(janitor)

ncrp_path <- csg_sp_path("jr_data_library", "data", "raw", "bjs", "ncrp", "icpsr_39234")

my_state <- "(06) California"
min_year <- 2010
max_year <- 2019

recode_sex_off <- function(df) {
  df |> 
    mutate(
      sex = str_sub(sex, 5),
      off_group = str_sub(offgeneral, 5)
    )
}

remove_other_na <- function(df) {
  df |> 
    filter(!is.na(off_group), off_group != "Other/unspecified")
}

load(file.path(ncrp_path, "ds0002", "39234-0002-Data.rda"))
load(file.path(ncrp_path, "ds0003", "39234-0003-Data.rda"))
load(file.path(ncrp_path, "ds0004", "39234-0004-Data.rda"))

admissions <- da39234.0002 |> 
  clean_names() |>
  filter(
    state == my_state,
    between(admityr, min_year, max_year),
    ) |> 
  recode_sex_off() |> 
  remove_other_na() |> 
  count(year = admityr, sex, off_group)

releases <- da39234.0003 |> 
  clean_names() |>
  filter(
    state == my_state,
    between(relyr, min_year, max_year),
    ) |> 
  recode_sex_off() |> 
  remove_other_na() |> 
  count(year = relyr, sex, off_group)

population <- da39234.0004 |> 
  clean_names() |>
  filter(
    state == my_state,
    between(rptyear, min_year, max_year),
    ) |> 
  recode_sex_off() |>
  remove_other_na() |> 
  count(year = rptyear, sex, off_group)

write_csv(admissions, "courses/forecast/admissions.csv")
write_csv(releases, "courses/forecast/releases.csv")
write_csv(population, "courses/forecast/population.csv")
