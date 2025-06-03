# ---------------------------------------- #
# Measuring Corrections Populations
# ---------------------------------------- #
#
# These datasets are larger versions of the ones used as examples
#   in the lessons, to be used for each lesson exercise.
#
# The example datasets in the lessons are based on 1991-2021 NCRP data, 
#   found at: https://doi.org/10.3886/ICPSR39234.v1
#
# ---------------------------------------- #

# ---------------------------------- #
# SETUP ----
# Load packages
library(tidyverse)

# set repo path
csv_path <- "https://raw.githubusercontent.com/CSGJusticeCenter/va_data/refs/heads/main/courses/pop_measures/"

# ---------------------------------- #  
# facility data ----

# import 10k dataset from repo
facility_data <- read_csv(paste0(csv_path, "pop_measures_sample_facility_data.csv"), 
                          show_col_types = FALSE) |> 
  distinct() |> 
  # drop datetime columns
  select(person_id, sex, race, admit_date, release_date, admit_type, 
         release_type, offense_category) |> 
  arrange(person_id, admit_date)
  
# check dataset 
head(facility_data)
lapply(facility_data |> 
         mutate(adm_year = year(admit_date),
                rel_year = year(release_date)) |> 
         select(ends_with("_year")),
       function(x) table(x, useNA = "ifany"))

# save file as csv to this repo
write.csv(facility_data, file = ("MCP_facility_data.csv"), row.names = FALSE)


# ---------------------------------- #  
# supervision data ----

# import 10k dataset from repo
supervision_data <- read_csv(paste0(csv_path, "pop_measures_sample_supervision_data.csv"), 
                             show_col_types = FALSE) |> 
  distinct() |> 
  # reorder columns
  select(person_id, sex, race, start_date, end_date, supervision_type, 
         end_reason, offense_category) |> 
  arrange(person_id, start_date)

# check dataset 
head(supervision_data)
lapply(supervision_data |> 
         mutate(start_year = year(start_date),
                end_year = year(end_date)) |> 
         select(ends_with("_year")),
       function(x) table(x, useNA = "ifany"))

# save file as csv to this repo
write.csv(supervision_data, file = ("MCP_supervision_data.csv"), 
          row.names = FALSE)
