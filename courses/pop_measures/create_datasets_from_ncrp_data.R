# ---------------------------------------- #
# Measuring Corrections Populations
# ---------------------------------------- #
#
# Create example datasets for lessons in the
#   Measuring Corrections Populations course,
#   using NCRP public data for 1991-2021
# Source: https://doi.org/10.3886/ICPSR39234.v1
#
# These datasets recreate common scenarios related to 
#   population measures that you may encounter as part 
#   of your work. 
#
# "Facility" data
#   - total N: 10,000
#   - variables: 10
#
# "Supervision" data
#   - total N: 10,000
#   - variables: 10
# ---------------------------------- #


# ---------------------------------- #
# SETUP ----
# Load packages
library(tidyverse)

# set seed for random values
set.seed(646787321)

# The "rpath" and "csv_path" values are hidden here. 
# To run this code on your own machine, set the values below to 
#   the corresponding paths on your computer

## "rpath" will be the path to the folder where you have downloaded 
##    and unzipped the NCRP data files
# rpath <- "paste full folder path here with, a "/" at the end"

## "csv_path" will be the path to your local clone of this repository
# csv_path <- "paste full folder path here with, a "/" at the end"

# ---------------------------------- #  
# IMPORT & PREP ----
# Import raw NCRP prison term data
#   (includes admissions, releases, population)

# NOTE: Due to file size limits, the raw NCRP data is not saved in this repo.
#	      This section of code is included to show how the 250k sample file was made.

### import raw data ----
ncrp_raw <- read_tsv(
  paste0(rpath, "NCRP 1991-2021/ICPSR_39234/DS0001/39234-0001-Data.tsv"),
  col_names = TRUE, show_col_types = FALSE,
  name_repair = tolower) 

# reduce size of file - rows & columns
ncrp_250k <- ncrp_raw |>  
  ## drop records with no admission year
  filter(admityr != 9999) |> 
  ## drop records for releases prior to 2017 
  filter(releaseyr >= 2017) |> 
  ## randomly select 250,000 records to make smaller file
  slice_sample(n = 250000) |> 
  ## drop columns we won't be using; keep columns we *might* use
  select(-c(mand_prisrel_year, proj_prisrel_year, parelig_year, timesrvd))

# check dataset
head(ncrp_250k)

# save file with 250k records prior to changing data
write.csv(ncrp_250k, 
          file = (paste0(csv_path, "ncrp_raw_1991-2021_ds0001_250k_sample.csv")),
          row.names = FALSE)

# check years in data
table(ncrp_250k |> select(admityr), useNA = "ifany")
table(ncrp_250k |> select(releaseyr), useNA = "ifany")  

# create new ID var and shift dates to be more recent
ncrp_shift <- ncrp_250k |> 
  ## set "missing" releases to NA
  mutate(releaseyr = ifelse(releaseyr == 9999, NA_integer_, releaseyr),
         reltype = ifelse(is.na(releaseyr), NA_integer_, reltype)) |> 
  ## INCREASE years in data by 2 so dates are more recent
  mutate(admit_year = admityr + 2,
         release_year = ifelse(!is.na(releaseyr), 
                               releaseyr + 2, NA_integer_)) |> 
  ## create fake ID numbers - "1" + last 7 chars of existing ID
  mutate(person_id = paste0("1", str_sub(abt_inmate_id, -7))) |> 
  ## only keep columns needed for datasets
  select(person_id, sex, race, admit_year, release_year, admtype, 
         reltype, offgeneral)
head(ncrp_shift)

# check years in data after updating
table(ncrp_shift |> select(admit_year), useNA = "ifany")
table(ncrp_shift |> select(release_year), useNA = "ifany")  

### standardize values ----
# standardize SEX and RACE for each id number
ncrp_standard <- ncrp_shift |>  
  ## select 50,000 random records to start with
  slice_sample(n = 50000) |> 
  ## randomly assign value for sorting rows per person by SEX
  mutate(rand_sex = sample(row_number())) |> 
  ## sort by random number & person_id
  arrange(person_id, rand_sex) |>
  ## get the first SEX value per person (randomly sorted)
  group_by(person_id) |> 
  mutate(sex = first(sex)) |> 
  ungroup() |> 
  ## randomly assign value for sorting rows per person by RACE
  mutate(rand_race = sample(row_number())) |> 
  ## sort by random number & person_id
  arrange(person_id, rand_race) |>
  ## get the first RACE value per person (randomly sorted)
  group_by(person_id) |> 
  mutate(race = first(race)) |> 
  ungroup() |> 
  ## drop random sort vars
  select(-c(rand_sex, rand_race)) |> 
  ## drop duplicate rows after updating sex & race
  distinct()

# make sure values are distributed the way we want
#   - use "table" to get frequencies, then "prop.table" for %s
lapply(ncrp_standard |> select(sex, race),
       function(x) prop.table(table(x, useNA = "ifany"),
                              margin = NULL))

# check count of records by person_id
ncrp_standard |> 
  group_by(person_id) |>
  summarize(person_N = max(row_number())) |>
  ungroup() |> 
  select(person_N) |> 
  table(useNA = "ifany")

### drop duplicates ----
# the "distinct()" function keeps the FIRST observation if there are
#   duplicate rows. ".keep_all = TRUE" retains all columns
ncrp_distinct <- ncrp_standard |> 
  ## sort rows 
  arrange(person_id, sex, race, admtype, offgeneral, reltype, 
          admit_year, release_year) |> 
  ## drop duplicate obs in all remaining variables
  distinct(person_id, sex, race, admtype, offgeneral, reltype, 
           admit_year, release_year) |> 
  ## duplicates where everything is the same except race
  distinct(person_id, sex, admtype, offgeneral, reltype, admit_year,
           release_year, .keep_all = TRUE) |> 
  ## duplicates where everything is the same except sex
  distinct(person_id, race, admtype, offgeneral, reltype, admit_year,
           release_year, .keep_all = TRUE) |> 
  ## duplicates where everything is the same except offense
  distinct(person_id, sex, race, admtype, reltype, admit_year,
           release_year, .keep_all = TRUE) |> 
  ## duplicates where everything is the same except release year & type
  distinct(person_id, sex, race, admtype, offgeneral, admit_year,
           .keep_all = TRUE) 

# check count of records by person_id
ncrp_distinct |> 
  group_by(person_id) |>
  summarize(person_N = max(row_number())) |>
  ungroup() |> 
  select(person_N) |> 
  table(useNA = "ifany")

### random admit dates ----

# create random admission MONTH
# NOTE: we don't want a lot of records with a random month of 10, 11, or 12 if 
#  the admit year = the release year because they will have a very short LOS
ncrp_admdate_mon <- ncrp_distinct |> 
  ## get difference between admit and release year
  mutate(los_apprx_yrs = release_year - admit_year) |> 
  ## generate random month value (integer between 1 and 12)
  mutate(adm_mon_rand = sample(1:12, n(), replace = TRUE)) |> 
  ## generate another random month for these records (use full 12 months 
  ##  because we don't want to replace ALL values, just want significantly less)
  mutate(adm_mon_updt = ifelse((los_apprx_yrs == 0 & adm_mon_rand > 9),
                               sample(1:12, n(), replace = TRUE),
                               NA_integer_)) |> 
  ## re-assign these months
  mutate(adm_mon_rand = ifelse(!is.na(adm_mon_updt), adm_mon_updt, 
                               adm_mon_rand))
    
# check random admission months & updated months
ncrp_admdate_mon |> select(adm_mon_rand, adm_mon_updt) |> 
  table(useNA = "ifany")

# check rate of random months
prop.table(ncrp_admdate_mon |> select(adm_mon_rand) |> 
             table(useNA = "ifany"))
  
# randomly admission DAY
ncrp_admdate_day <- ncrp_admdate_mon |> 
  ## drop column to update random month
  select(-adm_mon_updt) |> 
  ## set max days in each month (not worrying about leap years)
  mutate(mon_maxdays = 
           case_when(adm_mon_rand == 2 ~ 28,
                     adm_mon_rand %in% c(4, 6, 9, 11) ~ 30,
                     adm_mon_rand %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31)) |> 
  ## generate random day for each month based on max days
  mutate(adm_day_rand = ceiling(runif(n = n(), 0, mon_maxdays)))

# check max days for each month           
ncrp_admdate_day |> select(adm_mon_rand, mon_maxdays) |> 
  table(useNA = "ifany")
# check random day & month values
ncrp_admdate_day |> select(adm_day_rand, adm_mon_rand) |> 
  table(useNA = "ifany")

# check dates for December admissions with los_apprx_yrs = 0
#   we want a very small # of records with 1-2 day LOS -- <1%
ncrp_admdate_day |> 
  mutate(dec_flag = case_when(
    (adm_mon_rand == 12 & los_apprx_yrs == 0 & adm_day_rand > 29) ~ 1,
    TRUE ~ 0)) |> 
  select(dec_flag) |> 
  table(useNA = "ifany") |> 
  prop.table()

## combine random month & day into admission date
ncrp_admdate <- ncrp_admdate_day |> 
  mutate(admit_date = as.Date(
    paste(admit_year, adm_mon_rand, adm_day_rand, sep = "-"))) |> 
  ## drop variables for creating random date
  select(-c(mon_maxdays, adm_day_rand))

## check min/max dates by year
ncrp_admdate |> 
  group_by(admit_year) |> 
  summarize(min_dt = min(admit_date),
            max_dt = max(admit_date)) |> 
  arrange(desc(admit_year))

# check columns
str(ncrp_admdate)
head(ncrp_admdate)

# clean up environment
rm(ncrp_admdate_day, ncrp_admdate_mon, ncrp_standard, ncrp_distinct)

### random release dates ---- 
### based on randomly assigned LOS

# check LOS values (NA = no release year)
lapply(ncrp_admdate |> 
         select(los_apprx_yrs, release_year),
       function(x) table(x, useNA = "ifany"))

# create separate dataframe for records without release_year
# these will be appended back after generating random release dates
ncrp_not_released <- ncrp_admdate |> 
  filter(is.na(release_year)) |> 
  ## create blank release date column
  mutate(release_date = NA_Date_) |> 
  ## drop LOS columns
  select(-los_apprx_yrs, -adm_mon_rand)
str(ncrp_not_released)

# randomly generate LOS days for creating release date
ncrp_reldate_los <- ncrp_admdate |> 
  filter(!is.na(release_year)) |> 
  ## calculate last day of release year & days from admit_date
  mutate(end_of_year = paste(release_year, "12", "31", sep = "-"),
         end_of_year = as.Date(end_of_year),
         days_to_eoy = (end_of_year - admit_date)) |> 
  ## set range for number of days, based on appx LOS in years
  ##  (e.g., if los_apprx_yrs = 2, min = 730 and max = 1094)
  ### min = # of days in los_apprx_yrs (0 days here is OK - it just 
  ###   means that calculated LOS will be 1 day)
  mutate(los_apprx_days_min = los_apprx_yrs * 365) |> 
  ### max = 1 less than # of days in los_apprx_yrs+1
  ### if los_apprx_yrs is 0, max = number of days to end of year
  mutate(los_apprx_days_max = ifelse(los_apprx_yrs > 0, 
                                     ((los_apprx_yrs + 1) * 365) - 1,
                                     days_to_eoy)) |> 
  ## randomly assign a number of days between min/max apprx days 
  mutate(los_days_rand = ceiling(runif(n = n(), los_apprx_days_min, 
                                       los_apprx_days_max))) |> 
  ## replace "0" LOS values with "1"
  mutate(los_days_rand = ifelse(los_days_rand == 0, 1, los_days_rand))

# check min/max days for each value
ncrp_reldate_los |> 
  group_by(los_apprx_yrs) |> 
  summarize(min = min(los_apprx_days_min),
            max = max(los_apprx_days_max))
# 2020 was a leap year, so max days for los_apprx_yrs=0 is **365**
ncrp_reldate_los |> 
  filter(los_apprx_yrs == 0) |> 
  group_by(admit_year) |> 
  summarize(min = min(los_apprx_days_min),
            max = max(los_apprx_days_max))

# check max random LOS for 12/31 admission dates - all should = 1
ncrp_reldate_los |> 
  filter(admit_date == as.Date(paste(release_year, "12", "31", sep = "-"))) |> 
  group_by(admit_year) |> 
  summarize(max_los = max(los_days_rand))

# create random release date 
ncrp_reldate_init <- ncrp_reldate_los |> 
  ## drop LOS calculation vars
  select(-c(adm_mon_rand, end_of_year, days_to_eoy, los_apprx_days_min, 
            los_apprx_days_max)) |> 
  ## calculate release date based on admit_date and random LOS
  mutate(release_date = admit_date + los_days_rand) |> 
  ## create check variable with year of calculated release date
  mutate(reldate_year = year(release_date)) |> 
  ## flag rows where calculated release year does not match NCRP release_year
  mutate(rel_flag = ifelse(reldate_year != release_year, 1, 0))

# check for records where release_year is different from release_date year
ncrp_reldate_init |> count(rel_flag)
ncrp_reldate_init |> 
  filter(rel_flag == 1) |> 
  select(admit_year, release_year, los_apprx_yrs, admit_date, 
         release_date, los_days_rand) |> 
  distinct()

# update release years that don't match the year of the calculated release
#   date. these are due to LOS spanning across years. we don't need to stay 
#   true to NCRP values.
ncrp_reldate <- ncrp_reldate_init |> 
  mutate(release_year = year(release_date)) |> 
  ## drop preliminary columns after cleaning up release_year
  select(-c(rel_flag, los_apprx_yrs, los_days_rand, reldate_year))

# make sure release year matches dates 
ncrp_reldate |> 
  mutate(check = ifelse(year(release_date) != release_year, 1, 0)) |> 
  count(check)
ncrp_reldate |> select(release_year) |> table(useNA = "ifany")

# append non-releases back to dataset
ncrp_comb_dates <- rbind(ncrp_reldate, ncrp_not_released) |> 
  arrange(person_id, admit_date) 
str(ncrp_comb_dates)
head(ncrp_comb_dates)

# clean up environment
rm(ncrp_reldate_init, ncrp_reldate_los, ncrp_not_released)

### decode values ----

# convert numeric values to strings (from data documentation)
ncrp_decode <- ncrp_comb_dates |> 
  ## rename numeric versions of vars that will have the same name
  rename(sex_num = "sex",
         race_num = "race") |> 
  mutate(sex = case_when(sex_num == 1 ~ "Male",
                         sex_num == 2 ~ "Female"),
         race = case_when(race_num == 1 ~ "White, non-Hispanic",
                          race_num == 2 ~ "Black, non-Hispanic",
                          race_num == 3 ~ "Hispanic, any race",
                          race_num == 4 ~ "Other, non-Hispanic",
                          race_num == 9 ~ "Missing",
                          is.na(race_num) ~ "Missing"),
         admit_type = case_when(admtype == 1 ~ "New court commitment",
                                admtype == 2 ~ "Parole return/revocation",
                                admtype == 3 ~ "Other",
                                admtype == 9 ~ "Missing"),
         release_type = case_when(reltype == 1 ~ "Conditional release",
                                  reltype == 2 ~ "Unconditional release",
                                  reltype == 3 ~ "Other",
                                  reltype == 9 ~ "Missing",
                                  is.na(release_date) ~ NA_character_),
         offense_category = case_when(offgeneral == 1 ~ "Violent",
                                      offgeneral == 2 ~ "Property",
                                      offgeneral == 3 ~ "Drugs",
                                      offgeneral == 4 ~ "Other",
                                      offgeneral == 5 ~ "Other",
                                      offgeneral == 9 ~ "Missing",
                                      is.na(offgeneral) ~ "Missing")) 

# check converted values
ncrp_decode |> select(sex, sex_num) |> table(useNA = "ifany")
ncrp_decode |> select(race, race_num) |> table(useNA = "ifany")
ncrp_decode |> select(admtype, admit_type) |> table(useNA = "ifany")
ncrp_decode |> select(reltype, release_type) |> table(useNA = "ifany")
ncrp_decode |> select(offgeneral, offense_category) |> table(useNA = "ifany")

# only keep variables for datsets (drop numeric versions)
ncrp_final <- ncrp_decode |> 
  select(person_id, sex, race, admit_year, release_year, admit_date, 
         admit_type, release_date, release_type, offense_category) |>
  ## sort rows
  arrange(person_id, admit_date)

# check data
head(ncrp_final)

# clean up environment
rm(ncrp_comb_dates, ncrp_decode, ncrp_admdate, ncrp_reldate, ncrp_250k)

# ---------------------------------- #  
# FACILITY DATA ----

# build in scenarios for examples
facility_data <- ncrp_final |> 
  ## drop release dates in 2024
  filter(release_year < 2024 | is.na(release_year)) |> 
  ## randomly select 10,000 records
  slice_sample(n = 10000) |> 
  ## make sure rows are sorted by person_id & admit_date
  arrange(person_id, admit_date)
  
table(facility_data |> select(release_year), useNA = "ifany")

head(facility_data, n = 10)

# update records to force scenarios for example 
## force some records to have 2023 admission & release dates
facility_data$admit_date[2] <- as.Date("2023-02-13")
facility_data$admit_type[2] <- "New court commitment"
facility_data$release_date[2] <- as.Date("2023-10-14")
facility_data$release_type[2] <- "Conditional release"
facility_data$admit_date[4] <- as.Date("2023-05-01")
facility_data$admit_type[4] <- "Parole return/revocation"
facility_data$release_date[4] <- as.Date("2023-10-23")
facility_data$release_type[4] <- "Unconditional release"
# * force some people to have n>1
facility_data$person_id[5:6] <- "10000119"
facility_data$sex[5:6] <- "Male"
facility_data$race[5:6] <- "Black, non-Hispanic"
facility_data$admit_date[5] <- as.Date("2023-01-16")
facility_data$admit_type[5] <- "Parole return/revocation"
facility_data$offense_category[5] <- "Violent"
facility_data$release_date[5] <- as.Date("2023-03-24")
facility_data$release_type[5] <- "Unconditional release"
facility_data$admit_date[6] <- as.Date("2023-06-21")
facility_data$admit_type[6] <- "New court commitment"
facility_data$offense_category[6] <- "Public order"
facility_data$release_date[6] <- NA_Date_
facility_data$release_type[6] <- NA_character_
facility_data$person_id[8:9] <- "10000159"
facility_data$sex[8:9] <- "Male"
facility_data$race[8:9] <- "White, non-Hispanic"
facility_data$admit_date[8] <- as.Date("2019-10-11")
facility_data$admit_type[8] <- "New court commitment"
facility_data$release_date[8] <- as.Date("2023-06-19")
facility_data$release_type[8] <- "Conditional release"
facility_data$admit_date[9] <- as.Date("2023-09-04")
facility_data$admit_type[9] <- "Parole return/revocation"
facility_data$release_date[9] <- as.Date("2023-12-06")
facility_data$release_type[9] <- "Conditional release"

# Prep final dataset
# DO NOT RE-SORT, or the records we just updated will be moved around!
facility_final <- facility_data |> 
  ## update admit & release years
  mutate(admit_year = year(admit_date),
         release_year = year(release_date)) |> 
  ## drop any duplicate records
  distinct()

str(facility_final)
head(facility_final, n = 10)
 
## export as csv
write.csv(facility_final, file = paste0(csv_path, "MCP_facility_data.csv"),
          row.names = FALSE)

# ---------------------------------- #  
# SUPERVISION DATA ----

# drop facility vars & create vars for supervision
supv_data <- ncrp_final |> 
  ## drop release dates in 2024
  filter(release_year < 2024 | is.na(release_year)) |> 
  ## randomly select 10,000 records
  slice_sample(n = 10000) |> 
  ## rename variables
  rename(start_date = admit_date,
         start_year = admit_year,
         end_date = release_date,
         end_year = release_year) |> 
  ## shift fake ID numbers
  mutate(pers_id_num = as.numeric(person_id),
         pers_id_num = pers_id_num + 123456,
         person_id = as.character(pers_id_num)) |> 
  ## drop facility-specific vars
  select(-c(admit_type, release_type, pers_id_num))

head(supv_data, n = 10)

# randomly generate supervision type (parole, probation)
supv_rand_type <- supv_data |> 
  ## randomly assign probation or parole
  mutate(rand_type = sample(1:2, n(), replace = TRUE)) 

# check rates
supv_rand_type |> select(rand_type) |> table(useNA = "ifany") |> prop.table()

# decrease number of parole records by 1/3 to make data more realistic
supv_rand_par <- supv_rand_type |> 
  ## create another random value between 1-3 for parole records
  mutate(rand_parole = ifelse(rand_type == 1,
                              sample(1:3, n(), replace = TRUE),
                              NA_integer_)) |> 
  ## change 1/3 of parole values to probation value
  mutate(rand_type = ifelse(rand_type == 1 & rand_parole == 1,
                            2, rand_type)) |> 
  ## categorize values
  mutate(supervision_type = case_when(rand_type == 1 ~ "Parole",
                                      rand_type == 2 ~ "Probation"))

# check rates
supv_rand_par |> select(rand_type, rand_parole) |> table(useNA = "ifany") 
supv_rand_par |> select(supervision_type) |> 
  table(useNA = "ifany") |> prop.table()

# randomly assign outcome (completed, revoked, other)
supv_rand_outcome <- supv_rand_par |> 
  ## drop random supv type vars
  select(-c(rand_type, rand_parole)) |> 
  ## create column with random value if end_date is not NA
  mutate(rand_outcome = ifelse(!is.na(end_date),
                               sample(1:3, n(), replace = TRUE),
                               NA_integer_)) 

# check values
supv_rand_outcome |> select(rand_outcome) |> 
  table(useNA = "no") |> prop.table()

# we don't want equal frequencies for outcomes 
# re-assign "other" values to reduce by 2/3
supv_rand_other <- supv_rand_outcome |> 
  ## create another random value between 1-3 for outcome = 3
  mutate(rand_other = ifelse(rand_outcome == 3,
                             sample(1:3, n(), replace = TRUE),
                             NA_integer_)) |> 
  ## change 1/3 of other outcomes to random value
  mutate(rand_outcome = ifelse(rand_outcome == 3 & !is.na(rand_other),
                            rand_other, rand_outcome)) 

# check rates
supv_rand_other |> select(rand_outcome, rand_other) |> table(useNA = "ifany") 
supv_rand_other |> select(rand_outcome) |> 
  table(useNA = "no") |> prop.table()

# re-assign "revoked" values to reduce by 1/3
supv_rand_revs <- supv_rand_other |> 
  ## create another random value between 1-3 for outcome = 2
  mutate(rand_revs = ifelse(rand_outcome == 2,
                             sample(1:3, n(), replace = TRUE),
                             NA_integer_)) |> 
  ## change 1/3 of other outcomes to random value
  mutate(rand_outcome = ifelse(rand_outcome == 2 & rand_revs == 1,
                               rand_revs, rand_outcome)) |> 
  ## categorize values
  mutate(end_reason = case_when(rand_outcome == 1 ~ "Completed",
                                rand_outcome == 2 ~ "Revoked",
                                rand_outcome == 3 ~ "Other",
                                is.na(end_year) ~ NA_character_)) |> 
  ## make sure rows are sorted by person_id & start_date
  arrange(person_id, start_date)

# check rates
supv_rand_revs |> select(rand_outcome, rand_revs) |> table(useNA = "ifany") 
table(supv_rand_revs |> select(end_reason), useNA = "no") |> prop.table()
table(supv_rand_revs |> select(end_year, end_reason), useNA = "no") |> 
  prop.table(margin = 1)

head(supv_rand_revs |> select(-starts_with("rand")), n = 10)

##  build in scenarios for examples
# force a record to have n>1
supv_rand_revs$person_id[3:4] <- "10915996"
supv_rand_revs$race[3:4] <- "Black, non-Hispanic"
supv_rand_revs$sex[3:4] <- "Male"
supv_rand_revs$supervision_type[3:4] <- "Probation"
supv_rand_revs$start_date[3] <- as.Date("2022-06-02") 
supv_rand_revs$start_date[4] <- as.Date("2017-06-28") 
supv_rand_revs$end_date[3:4] <- NA_Date_  
supv_rand_revs$end_reason[3:4] <- NA_character_ 
supv_rand_revs$offense_category[3] <- "Violent"
supv_rand_revs$offense_category[4] <- "Public order"

# Prep final dataset
# DO NOT RE-SORT, or the records we just updated will be moved around!
supv_final <- supv_rand_revs |> 
  ## update start & end years
  mutate(start_year = year(start_date),
         end_year = year(end_date))  |> 
  ## order columns
  select(person_id, sex, race, start_year, end_year, start_date, 
         end_date, supervision_type, end_reason, offense_category) |> 
  ## drop any duplicate records
  distinct()

head(supv_final, n = 10)

# compare columns in both datasets
colnames(supv_final)
colnames(facility_final)

# save supervision file as csv
write.csv(supv_final, file = (paste0(csv_path, "MCP_supervision_data.csv")),
          row.names = FALSE)
