# =============================================================================
# 01_data_wrangling.R
# STAT 443 Project - Data Wrangling
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Load raw data, merge NHPI with interest rate, compute lags,
#              and save wrangled data for downstream analyses.
# =============================================================================

library(readr)
library(dplyr)
library(lubridate)

# -----------------------------------------------------------------------------
# 1. Load raw data
# -----------------------------------------------------------------------------

# NHPI: covering 1981-01 to 2025-12
nhpi_full <- read.csv("18100205.csv", na.strings = c("..", "NA", "")) %>%
  mutate(REF_DATE = as.Date(paste0(REF_DATE, "-01")),
         REF_DATE_ym = format(REF_DATE, "%Y-%m"))

# Interest rate: covering 1981-01 to 2023-12
ir <- read_csv("IRSTCB01CAM156N.csv", show_col_types = FALSE) %>%
  rename(REF_DATE = observation_date,
         IR = IRSTCB01CAM156N) %>%
  mutate(REF_DATE = as.Date(REF_DATE),
         REF_DATE_ym = format(REF_DATE, "%Y-%m"),
         IR = as.numeric(IR)) %>%
  filter(!is.na(IR))

# -----------------------------------------------------------------------------
# 2. Merge NHPI with interest rate (overlap: 1981-01 to 2023-12)
# -----------------------------------------------------------------------------

nhpi_ir <- nhpi_full %>%
  left_join(ir %>% select(REF_DATE_ym, IR), by = "REF_DATE_ym") %>%
  filter(REF_DATE <= as.Date("2023-12-01"))

# Compute lagged interest rates
nhpi_ir <- nhpi_ir %>%
  group_by(GEO, New.housing.price.indexes) %>%
  arrange(REF_DATE) %>%
  mutate(IR_lag1 = lag(IR, 1),
         IR_lag2 = lag(IR, 2)) %>%
  ungroup()

# -----------------------------------------------------------------------------
# 3. Checks
# -----------------------------------------------------------------------------

range(nhpi_full$REF_DATE)  # 1981-01-01 to 2025-12-01
range(ir$REF_DATE)         # 1981-01-01 to 2023-12-01
range(nhpi_ir$REF_DATE)    # 1981-01-01 to 2023-12-01
mean(is.na(nhpi_ir$IR))    # should be ~0

# -----------------------------------------------------------------------------
# 4. Save
# -----------------------------------------------------------------------------

save(nhpi_full, ir, nhpi_ir, file = "wrangled_data.RData")
message("Saved: wrangled_data.RData")
