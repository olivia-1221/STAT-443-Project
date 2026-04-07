# =============================================================================
# 01_data_wrangling.R
# STAT 443 Project - Data Wrangling
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Load raw data, merge NHPI with interest rate, compute lags,
#              and save wrangled data for downstream analyses.
# =============================================================================

knitr::opts_chunk$set(echo = TRUE)
library(readr)
library(dplyr)
library(lubridate)

## 1. Read and prepare the full NHPI dataset
# Covers 1981-01 to 2025-12
nhpi_full <- read.csv(
  "18100205-eng/18100205.csv",
  na.strings = c("..", "NA", "")
) %>%
  mutate(
    REF_DATE = as.Date(paste0(REF_DATE, "-01")),
    REF_DATE_ym = format(REF_DATE, "%Y-%m")
  )

head(nhpi_full)

## 2. Read and prepare the interest-rate dataset
# Covers 1981-01 to 2023-12
ir <- read_csv(
  "IRSTCB01CAM156N.csv",
  show_col_types = FALSE
) %>%
  rename(
    REF_DATE = observation_date,
    IR = IRSTCB01CAM156N
  ) %>%
  mutate(
    REF_DATE = as.Date(REF_DATE),
    REF_DATE_ym = format(REF_DATE, "%Y-%m"),
    IR = as.numeric(IR)
  ) %>%
  filter(!is.na(IR))

head(ir)

## 3. Merge NHPI with interest rates for ARMAX/ARIMAX analysis
# Keep overlap only: 1981-01 to 2023-12
nhpi_ir <- nhpi_full %>%
  left_join(
    ir %>% select(REF_DATE_ym, IR),
    by = "REF_DATE_ym"
  ) %>%
  filter(REF_DATE <= as.Date("2023-12-01")) %>%
  group_by(GEO, New.housing.price.indexes) %>%
  arrange(REF_DATE) %>%
  mutate(
    IR_lag1 = lag(IR, 1),
    IR_lag2 = lag(IR, 2)
  ) %>%
  ungroup()

head(nhpi_ir)

## 4. Sanity checks
range(nhpi_full$REF_DATE)   # should be 1981-01-01 to 2025-12-01
range(ir$REF_DATE)          # should be 1981-01-01 to 2023-12-01
range(nhpi_ir$REF_DATE)     # should be 1981-01-01 to 2023-12-01
mean(is.na(nhpi_ir$IR))     # should be 0 within overlap

nhpi_ir %>%
  filter(GEO == "Canada", New.housing.price.indexes == "Total (house and land)") %>%
  select(REF_DATE, VALUE, IR, IR_lag1, IR_lag2) %>%
  head(5)

## 5. Save merged dataset for later ARMAX/ARIMAX modeling
write.csv(
  nhpi_ir,
  "nhpi_with_interest_rate_1981_2023.csv",
  row.names = FALSE
)

## 6. Extract city-level ARMAX datasets (1981-2023)
cities <- c(
  "Toronto, Ontario",
  "Vancouver, British Columbia",
  "Montréal, Quebec",
  "Québec, Quebec"
)

city_armax <- nhpi_ir %>%
  filter(
    GEO %in% cities,
    New.housing.price.indexes == "Total (house and land)"
  ) %>%
  select(REF_DATE, GEO, VALUE, IR_lag1, IR_lag2) %>%
  arrange(GEO, REF_DATE)

head(city_armax)

toronto <- city_armax %>% filter(GEO == "Toronto, Ontario")
vancouver <- city_armax %>% filter(GEO == "Vancouver, British Columbia")
montreal <- city_armax %>% filter(GEO == "Montréal, Quebec")
quebec <- city_armax %>% filter(GEO == "Québec, Quebec")

sum(is.na(toronto))
sum(is.na(vancouver))
sum(is.na(montreal))
sum(is.na(quebec))

write.csv(toronto, "toronto.csv", row.names = FALSE)
write.csv(vancouver, "vancouver.csv", row.names = FALSE)
write.csv(montreal, "montreal.csv", row.names = FALSE)
write.csv(quebec, "quebec.csv", row.names = FALSE)

## 7. Extract full-horizon univariate city datasets (1981-2025)
city_full <- nhpi_full %>%
  filter(
    GEO %in% cities,
    New.housing.price.indexes == "Total (house and land)"
  ) %>%
  select(REF_DATE, GEO, VALUE) %>%
  arrange(GEO, REF_DATE)

head(city_full)

toronto_full <- city_full %>% filter(GEO == "Toronto, Ontario")
vancouver_full <- city_full %>% filter(GEO == "Vancouver, British Columbia")
montreal_full <- city_full %>% filter(GEO == "Montréal, Quebec")
quebec_full <- city_full %>% filter(GEO == "Québec, Quebec")

sum(is.na(toronto_full))
sum(is.na(vancouver_full))
sum(is.na(montreal_full))
sum(is.na(quebec_full))

write.csv(toronto_full, "toronto_full.csv", row.names = FALSE)
write.csv(vancouver_full, "vancouver_full.csv", row.names = FALSE)
write.csv(montreal_full, "montreal_full.csv", row.names = FALSE)
write.csv(quebec_full, "quebec_full.csv", row.names = FALSE)

## 8. Final objects created by this script

# `nhpi_full`: full NHPI dataset, 1981-2025
# `ir`: monthly interest-rate dataset, 1981-2023
# `nhpi_ir`: merged NHPI + interest-rate dataset, 1981-2023
# `city_armax`: city-level ARMAX-ready dataset
# `city_full`: city-level full-horizon univariate dataset
# Saved CSV files for Toronto, Vancouver, Montréal, and Québec
