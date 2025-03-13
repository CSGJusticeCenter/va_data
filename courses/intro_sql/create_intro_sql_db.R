library(tidyverse)
library(randomNames)
library(RSQLite)

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

age_months <- round(rgbeta(1000, 32, 50, min = 18, max = 1000) * 365, 0)

off_weights <- tribble(
  ~off, ~weight,
  "VIOL", 0.5,
  "PROP", 0.15,
  "PUB_ORDER", 0.1,
  "DRUG", 0.25
)

person <- randomNames(
  gender = gender,
  ethnicity = ethnicity,
  return.complete.data = TRUE
  ) |> 
  as_tibble() |> 
  mutate(
    person_id = paste0("doc_", str_pad(row_number(), 4, pad = "0")),
    sex = if_else(gender == 0, "M", "F"),
    race = case_when(
      ethnicity == 1 ~ "AI",
      ethnicity == 2 ~ "A",
      ethnicity == 3 ~ "B",
      ethnicity == 4 ~ "H",
      ethnicity == 5 ~ "W"
      ),
    dob = as.Date("2025-03-13") - days(age_months)
    ) |> 
  select(person_id, first_name, last_name, sex, race, dob) |> 
  mutate(dob = as.character(dob))

facility <- tribble(
  ~facil_code, ~facil_name, ~facil_beds, ~secure_level,
   "NSCF", "Northern State Correctional Facility", 500, "Medium",
   "ESCF", "Eastern Correctional Facility", 650, "High",
   "SWSCF", "Southern Women's State Correctional Facility", 250, "Medium",
   "WSCF", "Western State Correctional Facility", 750, "Low"
)

admit <- person |> 
  sample_n(800) |> 
  mutate(
    facil_code = if_else(
      sex == "F", "SWSCF",
      sample(c("NSCF", "ESCF", "WSCF"), 800, replace = TRUE)
      ),
    adm_date = sample(seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day"), 800, replace = TRUE),
    off_group = sample(off_weights$off, size = 800, replace = TRUE, prob = off_weights$weight)
    ) |> 
  select(person_id, adm_date, facil_code, off_group)


con <- dbConnect(RSQLite::SQLite(), "courses/intro_sql/state_doc.db")

dbWriteTable(con, "person", person)
dbWriteTable(con, "admit", admit)
dbWriteTable(con, "facility", facility)

dbDisconnect(con)
