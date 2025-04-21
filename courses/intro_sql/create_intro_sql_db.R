library(tidyverse)
library(randomNames)
library(RSQLite)
library(lubridate)

set.seed(313)

gender <- sample(c(rep(0, times = 800), rep(1, times = 200)))

ethnicity <- sample(c(
  rep(1, times = 50), 
  rep(2, times = 20),
  rep(3, times = 300),
  rep(4, times = 130),
  rep(5, times = 500)
  ))

rgbeta <- function(n, mean, var, min = 0, max = 1)
{
  dmin <- mean - min
  dmax <- max - mean

  if (dmin <= 0 || dmax <= 0)
  {
    stop(paste("mean must be between min =", min, "and max =", max)) 
  }

  if (var >= dmin * dmax)
  {
    stop(paste("var must be less than (mean - min) * (max - mean) =", dmin * dmax))
  }

  # mean and variance of the standard beta distributed variable
  mx <- (mean - min) / (max - min)
  vx <- var / (max - min)^2

  # find the corresponding alpha-beta parameterization
  a <- ((1 - mx) / vx - 1 / mx) * mx^2
  b <- a * (1 / mx - 1)

  # generate standard beta observations and transform
  x <- rbeta(n, a - 0.5, b + 0.5)
  y <- (max - min) * x + min

  return(y)
}

age_days <- round(rgbeta(1000, 32, 50, min = 18, max = 1000) * 365, 0)

off_weights <- tribble(
  ~off, ~weight,
  "VIOL", 0.5,
  "PROP", 0.15,
  "PUB_ORDER", 0.1,
  "DRUG", 0.25
)

sample_lang <- function(prob) {
  sample(
    c("English", "Spanish", "French", "Other"),
    size = 1000, replace = TRUE, prob = prob
    )
}

residents <- randomNames(
  gender = gender,
  ethnicity = ethnicity,
  return.complete.data = TRUE
  ) |> 
  as_tibble() |> 
  mutate(
    resident_id = paste0("doc_", str_pad(row_number(), 4, pad = "0")),
    sex = if_else(gender == 0, "M", "F"),
    race = case_when(
      ethnicity == 1 ~ "AI",
      ethnicity == 2 ~ "A",
      ethnicity == 3 ~ "B",
      ethnicity == 4 ~ "H",
      ethnicity == 5 ~ "W"
      ),
    dob = as.Date("2025-03-13") - days(age_days),
    age = year(as.period(interval(dob, as.Date("2024-01-01")))),
    birth_country = sample(
      c("United States", "Mexico", "Canada", NA),
      size = 1000, replace = TRUE, prob = c(0.7, 0.2, 0.05, 0.05)
      ),
    primary_language = case_when(
      birth_country == "United States" ~ sample_lang(prob = c(0.8, 0.1, 0.05, 0.05)),
      birth_country == "Mexico" ~ sample_lang(prob = c(0.05, 0.89, 0.01, 0.05)),
      birth_country == "Canada"  ~ sample_lang(prob = c(0.6, 0.05, 0.3, 0.05)),
      TRUE ~ sample_lang(prob = c(0.45, 0.2, 0.05, 0.3))
      )
    ) |> 
  rowwise() |> 
  mutate(age_at_first_arrest = sample(seq(17, age, by = 1), 1)) |> 
  ungroup() |> 
  select(
    resident_id,
    first_name,
    last_name,
    sex,
    race,
    dob,
    age_at_first_arrest,
    birth_country,
    primary_language
    ) |> 
  mutate(dob = as.character(dob))

facilities <- tribble(
  ~facil_id, ~facil_name, ~facil_beds, ~secure_level,
   "NSCF", "Northern State Correctional Facility", 500, "Medium",
   "ESCF", "Eastern Correctional Facility", 650, "High",
   "SWSCF", "Southern Women's State Correctional Facility", 250, "Medium",
   "WSCF", "Western State Correctional Facility", 750, "Low"
)

rel_date_norm <- rnorm(1000, mean = 30, sd = 10)
days_to_rel_record <- round(pmin(pmax(rel_date_norm, 10), 60))

sent_date_norm <- rnorm(1000, mean = 12, sd = 5)
days_to_admit <- round(pmin(pmax(sent_date_norm, 1), 30))

intakes <- residents |> 
  mutate(
    intake_id = row_number(),
    admission_date = sample(
      seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"),
      1000, replace = TRUE
      ),
    off_group = sample(
      off_weights$off, size = 1000, 
      replace = TRUE, prob = off_weights$weight
      ),
    sentence_years = case_when(
      off_group == "VIOL" ~ sample(5:25, 1000, replace = TRUE),
      off_group == "PROP" ~ sample(1:8, 1000, replace = TRUE),
      off_group == "PUB_ORDER" ~ sample(1:5, 1000, replace = TRUE),
      off_group == "DRUG" ~ sample(2:10, 1000, replace = TRUE)
      ),
    sentence_months = sample(0:11, 1000, replace = TRUE),
    facil_id = if_else(
      sex == "F", "SWSCF",
      sample(c("NSCF", "ESCF", "WSCF"), 1000, replace = TRUE)
      ),
    county_id = sample(1:7, 1000, replace = TRUE),
    rel_ready_date = admission_date + days_to_rel_record,
    sentence_date = admission_date - days_to_admit,
    exp_release_date = sentence_date + sentence_years * 365 + sentence_months * 30,
    across(ends_with("date"), as.character),
    sentence_date = if_else(
      sample(c(TRUE, FALSE), 1000, replace = TRUE, prob = c(0.86, 0.14)),
      sentence_date, NA
      ),
    rel_ready_date = if_else(!is.na(sentence_date), rel_ready_date, NA),
    rel_ready_date = if_else(
      sample(c(TRUE, FALSE), 1000, replace = TRUE, prob = c(0.88, 0.12)),
      rel_ready_date, NA 
    ),
    exp_release_date = if_else(!is.na(rel_ready_date), exp_release_date, NA),
    ) |> 
  select(
    intake_id,
    resident_id,
    admission_date,
    sentence_date,
    rel_ready_date,
    exp_release_date,
    sentence_years,
    sentence_months,
    county_id,
    facil_id,
    off_group
  )

counties <- tribble(
  ~county_id, ~county_name, ~county_fips, ~region,
  1, "Washington", "89101", "North",
  2, "Hamilton", "89102", "North",
  3, "Jefferson", "89103", "South",
  4, "Adams", "89104", "West",
  5, "Madison", "89105", "East",
  6, "Orleans", "89106", "South",
  7, "Wright", "89107", "South"
)

con <- dbConnect(RSQLite::SQLite(), "courses/intro_sql/state_doc.db")

dbWriteTable(con, "residents", residents)
dbWriteTable(con, "intakes", intakes)
dbWriteTable(con, "counties", counties)
dbWriteTable(con, "facilities", facilities)

dbListTables(con)

dbReadTable(con, "intakes")

dbDisconnect(con)

