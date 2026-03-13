library(readr)
library(dplyr)
library(lubridate)

# Cover 1981-01 to 2025-12
nhpi_full <- read.csv(
  "18100205-eng/18100205.csv",
  na.strings = c("..", "NA", "")
) %>%
  mutate(
    REF_DATE = as.Date(paste0(REF_DATE, "-01")),
    REF_DATE_ym = format(REF_DATE, "%Y-%m")
  )
head(nhpi_full)

# Cover 1981-01 to 2023-12
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

# Combine dataset for ARMAX (cover the overlap only)
nhpi_ir <- nhpi_full %>%
  left_join(
    ir %>% select(REF_DATE_ym, IR),
    by = "REF_DATE_ym"
  ) %>%
  filter(REF_DATE <= as.Date("2023-12-01"))

head(nhpi_ir)
# Lagged interest rate
nhpi_ir <- nhpi_ir %>%
  group_by(GEO, New.housing.price.indexes) %>%
  arrange(REF_DATE) %>%
  mutate(
    IR_lag1 = lag(IR, 1),
    IR_lag2 = lag(IR, 2)
  ) %>%
  ungroup()

head(nhpi_ir)

# Date coverage
range(nhpi_full$REF_DATE)  # should include 1981-01-01 to 2025-12-01 (or close)
range(ir$REF_DATE)         # should include 1981-01-01 to 2023-12-01
range(nhpi_ir$REF_DATE)    # should include 1981-01-01 to 2023-12-01

# Any missing IR after merge? (should be basically none within overlap)
mean(is.na(nhpi_ir$IR))

# Check lags (first month per series will be NA)
nhpi_ir %>%
  filter(GEO == "Canada", New.housing.price.indexes == "Total (house and land)") %>%
  select(REF_DATE, VALUE, IR, IR_lag1, IR_lag2) %>%
  head(5)
