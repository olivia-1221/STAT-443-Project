# =============================================================================
# 02_exponential_smoothing.R
# STAT 443 Project - Exponential Smoothing
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Apply Simple, Linear (Holt), and Additive Seasonal ESM to
#              Montreal, Toronto, and Vancouver. Also runs baseline forecasts.
# =============================================================================

library(forecast)

load("wrangled_data.RData")

# -----------------------------------------------------------------------------
# Helper functions
# -----------------------------------------------------------------------------

persist_fc <- function(train, holdout, iprint = FALSE) {
  n <- length(train); n_holdout <- length(holdout)
  mse <- 0; fcvec <- numeric(n_holdout)
  fc <- train[n]; fcvec[1] <- fc; mse <- mse + (holdout[1] - fc)^2
  for (i in 2:n_holdout) {
    fc <- holdout[i-1]; fcvec[i] <- fc; mse <- mse + (holdout[i] - fc)^2
  }
  if (iprint) print(cbind(holdout, fcvec))
  return(list(fc = fcvec, rmse = sqrt(mse / n_holdout)))
}

iid_fc <- function(train, holdout, iprint = FALSE) {
  n_holdout <- length(holdout); average <- mean(train)
  mse <- 0; fcvec <- numeric(n_holdout)
  for (i in 1:n_holdout) { fcvec[i] <- average; mse <- mse + (holdout[i] - average)^2 }
  if (iprint) print(cbind(holdout, fcvec))
  return(list(fc = fcvec, rmse = sqrt(mse / n_holdout)))
}

esm_fc <- function(train, holdout, alpha, level, iprint = FALSE) {
  n_holdout <- length(holdout); sse <- 0; fcvec <- numeric(n_holdout)
  fc <- level; level_new <- level; fcvec[1] <- fc; sse <- sse + (holdout[1] - fc)^2
  for (i in 2:n_holdout) {
    level_new <- alpha * holdout[i-1] + (1 - alpha) * level_new
    fc <- level_new; fcvec[i] <- fc; sse <- sse + (holdout[i] - fc)^2
  }
  return(list(fc = fcvec, rmse = sqrt(sse / n_holdout)))
}

lholt_fc <- function(train, holdout, alpha, beta, level, slope, iprint = FALSE) {
  n_holdout <- length(holdout); sse <- 0; fcvec <- numeric(n_holdout)
  fc <- level + slope; fcvec[1] <- fc; sse <- sse + (holdout[1] - fc)^2
  level_prev <- level; slope_prev <- slope
  for (i in 2:n_holdout) {
    level_new <- alpha * holdout[i-1] + (1 - alpha) * fc
    slope_new <- beta * (level_new - level_prev) + (1 - beta) * slope_prev
    fc <- level_new + slope_new; fcvec[i] <- fc; sse <- sse + (holdout[i] - fc)^2
    level_prev <- level_new; slope_prev <- slope_new
  }
  return(list(fc = fcvec, rmse = sqrt(sse / n_holdout)))
}

aseason_fc <- function(train, holdout, alpha, beta, gamma, level, slope, season, iprint = FALSE) {
  n_holdout <- length(holdout); m <- length(season); sse <- 0; fcvec <- numeric(n_holdout)
  level_prev <- level; slope_prev <- slope
  spos <- function(i) ((i-1) %% m) + 1
  fc <- level_prev + slope_prev + season[spos(1)]; fcvec[1] <- fc; sse <- sse + (holdout[1] - fc)^2
  for (i in 2:n_holdout) {
    k_prev <- spos(i-1); s_prev <- season[k_prev]
    level_new <- alpha * (holdout[i-1] - s_prev) + (1 - alpha) * (level_prev + slope_prev)
    slope_new <- beta * (level_new - level_prev) + (1 - beta) * slope_prev
    s_new <- gamma * (holdout[i-1] - level_new) + (1 - gamma) * s_prev
    season[k_prev] <- s_new
    fc <- level_new + slope_new + season[spos(i)]; fcvec[i] <- fc; sse <- sse + (holdout[i] - fc)^2
    level_prev <- level_new; slope_prev <- slope_new
  }
  return(list(fc = fcvec, rmse = sqrt(sse / n_holdout)))
}

# -----------------------------------------------------------------------------
# Montreal
# -----------------------------------------------------------------------------

montreal_nhpi_data    <- read.csv("Full_Cities/montreal_full.csv")
montreal_nhpi_ir_data <- read.csv("Extracted_Cities/montreal.csv")
montreal_nhpi   <- montreal_nhpi_data$VALUE
montreal_irlag1 <- montreal_nhpi_ir_data$IR_lag1
montreal_ts     <- ts(montreal_nhpi, start = c(1981,1), end = c(2023,12), frequency = 12)
montreal_ir_ts  <- ts(montreal_irlag1, start = c(1981,1), end = c(2023,12), frequency = 12)
montreal_transformed_nhpi <- diff(log(montreal_ts))
montreal_train_ts         <- window(montreal_transformed_nhpi, end = c(2021,12))
montreal_holdout_ts       <- window(montreal_transformed_nhpi, start = c(2022,1))
montreal_train_ir_xreg    <- window(montreal_ir_ts, start = c(1981,2), end = c(2021,12))
montreal_holdout_ir_xreg  <- window(montreal_ir_ts, start = c(2022,1))
ntrain <- length(montreal_train_ts)
ntotal <- length(montreal_ts) - 1
montreal_transformed_nhpi_train   <- montreal_transformed_nhpi[1:ntrain]
montreal_transformed_nhpi_holdout <- montreal_transformed_nhpi[(ntrain+1):ntotal]

montreal_persist_fc <- persist_fc(montreal_train_ts, montreal_holdout_ts)
montreal_iid_fc     <- iid_fc(montreal_train_ts, montreal_holdout_ts)

fit_expsmo <- HoltWinters(montreal_train_ts, beta = FALSE, gamma = FALSE)
montreal_esm_fc <- esm_fc(montreal_transformed_nhpi_train, montreal_transformed_nhpi_holdout,
                           alpha = fit_expsmo$alpha, level = fit_expsmo$coefficients["a"])

fit_hw <- HoltWinters(montreal_train_ts, gamma = FALSE)
montreal_holt_fc <- lholt_fc(montreal_transformed_nhpi_train, montreal_transformed_nhpi_holdout,
                              alpha = fit_hw$alpha, beta = fit_hw$beta,
                              level = fit_hw$coefficients["a"], slope = fit_hw$coefficients["b"])

wafit <- HoltWinters(montreal_train_ts, seasonal = "additive")
montreal_aseason_fc <- aseason_fc(montreal_transformed_nhpi_train, montreal_transformed_nhpi_holdout,
                                   alpha = wafit$alpha, beta = wafit$beta, gamma = wafit$gamma,
                                   level = wafit$coef[1], slope = wafit$coef[2],
                                   season = wafit$coefficient[3:14])

rmse_esm_montreal <- cbind(montreal_esm_fc$rmse, montreal_holt_fc$rmse, montreal_aseason_fc$rmse)
colnames(rmse_esm_montreal) <- c("esm", "lholt", "aseason")
print(rmse_esm_montreal)
# Best: Linear Holt (lholt)

# -----------------------------------------------------------------------------
# Toronto
# -----------------------------------------------------------------------------

toronto_nhpi_data    <- read.csv("Full_Cities/toronto_full.csv")
toronto_nhpi_ir_data <- read.csv("Extracted_Cities/toronto.csv")
toronto_nhpi   <- toronto_nhpi_data$VALUE
toronto_irlag1 <- toronto_nhpi_ir_data$IR_lag1
toronto_ts     <- ts(toronto_nhpi, start = c(1981,1), end = c(2023,12), frequency = 12)
toronto_ir_ts  <- ts(toronto_irlag1, start = c(1981,1), end = c(2023,12), frequency = 12)
toronto_transformed_nhpi <- diff(log(toronto_ts))
toronto_train_ts         <- window(toronto_transformed_nhpi, end = c(2021,12))
toronto_holdout_ts       <- window(toronto_transformed_nhpi, start = c(2022,1))
toronto_train_ir_xreg    <- window(toronto_ir_ts, start = c(1981,2), end = c(2021,12))
toronto_holdout_ir_xreg  <- window(toronto_ir_ts, start = c(2022,1))
ntrain <- length(toronto_train_ts)
ntotal <- length(toronto_ts) - 1
toronto_transformed_nhpi_train   <- toronto_transformed_nhpi[1:ntrain]
toronto_transformed_nhpi_holdout <- toronto_transformed_nhpi[(ntrain+1):ntotal]

toronto_persist_fc <- persist_fc(toronto_train_ts, toronto_holdout_ts)
toronto_iid_fc     <- iid_fc(toronto_train_ts, toronto_holdout_ts)

fit_expsmo <- HoltWinters(toronto_train_ts, beta = FALSE, gamma = FALSE)
toronto_esm_fc <- esm_fc(toronto_transformed_nhpi_train, toronto_transformed_nhpi_holdout,
                          alpha = fit_expsmo$alpha, level = fit_expsmo$coefficients["a"])

fit_hw <- HoltWinters(toronto_train_ts, gamma = FALSE)
toronto_holt_fc <- lholt_fc(toronto_transformed_nhpi_train, toronto_transformed_nhpi_holdout,
                             alpha = fit_hw$alpha, beta = fit_hw$beta,
                             level = fit_hw$coefficients["a"], slope = fit_hw$coefficients["b"])

wafit <- HoltWinters(toronto_train_ts, seasonal = "additive")
toronto_aseason_fc <- aseason_fc(toronto_transformed_nhpi_train, toronto_transformed_nhpi_holdout,
                                  alpha = wafit$alpha, beta = wafit$beta, gamma = wafit$gamma,
                                  level = wafit$coef[1], slope = wafit$coef[2],
                                  season = wafit$coefficient[3:14])

rmse_esm_toronto <- cbind(toronto_esm_fc$rmse, toronto_holt_fc$rmse, toronto_aseason_fc$rmse)
colnames(rmse_esm_toronto) <- c("esm", "lholt", "aseason")
print(rmse_esm_toronto)
# Best: Simple ESM

# -----------------------------------------------------------------------------
# Vancouver
# -----------------------------------------------------------------------------

vancouver_nhpi_data    <- read.csv("Full_Cities/vancouver_full.csv")
vancouver_nhpi_ir_data <- read.csv("Extracted_Cities/vancouver.csv")
vancouver_nhpi   <- vancouver_nhpi_data$VALUE
vancouver_irlag1 <- vancouver_nhpi_ir_data$IR_lag1
vancouver_ts     <- ts(vancouver_nhpi, start = c(1981,1), end = c(2023,12), frequency = 12)
vancouver_ir_ts  <- ts(vancouver_irlag1, start = c(1981,1), end = c(2023,12), frequency = 12)
vancouver_transformed_nhpi <- diff(log(vancouver_ts))
vancouver_train_ts         <- window(vancouver_transformed_nhpi, end = c(2021,12))
vancouver_holdout_ts       <- window(vancouver_transformed_nhpi, start = c(2022,1))
vancouver_train_ir_xreg    <- window(vancouver_ir_ts, start = c(1981,2), end = c(2021,12))
vancouver_holdout_ir_xreg  <- window(vancouver_ir_ts, start = c(2022,1))
ntrain <- length(vancouver_train_ts)
ntotal <- length(vancouver_transformed_nhpi)
vancouver_transformed_nhpi_train   <- vancouver_transformed_nhpi[1:ntrain]
vancouver_transformed_nhpi_holdout <- vancouver_transformed_nhpi[(ntrain+1):ntotal]

vancouver_persist_fc <- persist_fc(vancouver_train_ts, vancouver_holdout_ts)
vancouver_iid_fc     <- iid_fc(vancouver_train_ts, vancouver_holdout_ts)

fit_expsmo <- HoltWinters(vancouver_train_ts, beta = FALSE, gamma = FALSE)
vancouver_esm_fc <- esm_fc(vancouver_transformed_nhpi_train, vancouver_transformed_nhpi_holdout,
                            alpha = fit_expsmo$alpha, level = fit_expsmo$coefficients["a"])

fit_hw <- HoltWinters(vancouver_train_ts, gamma = FALSE)
vancouver_holt_fc <- lholt_fc(vancouver_transformed_nhpi_train, vancouver_transformed_nhpi_holdout,
                               alpha = fit_hw$alpha, beta = fit_hw$beta,
                               level = fit_hw$coefficients["a"], slope = fit_hw$coefficients["b"])

# Note: switched to ets() because HoltWinters failed to converge for Vancouver
vancouver_aseason_fit      <- ets(vancouver_train_ts, model = "AAA")
vancouver_aseason_forecast <- forecast(vancouver_aseason_fit, h = length(vancouver_holdout_ts))
vancouver_aseason_rmse     <- sqrt(mean((vancouver_holdout_ts - vancouver_aseason_forecast$mean)^2))

rmse_esm_vancouver <- cbind(vancouver_esm_fc$rmse, vancouver_holt_fc$rmse, vancouver_aseason_rmse)
colnames(rmse_esm_vancouver) <- c("esm", "lholt", "aseason")
print(rmse_esm_vancouver)
# Best: Simple ESM

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(persist_fc, iid_fc, esm_fc, lholt_fc, aseason_fc,
     montreal_nhpi, montreal_irlag1, montreal_ts,
     montreal_transformed_nhpi, montreal_train_ts, montreal_holdout_ts,
     montreal_transformed_nhpi_train, montreal_transformed_nhpi_holdout,
     montreal_train_ir_xreg, montreal_holdout_ir_xreg,
     montreal_persist_fc, montreal_iid_fc,
     montreal_esm_fc, montreal_holt_fc, montreal_aseason_fc,
     toronto_nhpi, toronto_irlag1, toronto_ts,
     toronto_transformed_nhpi, toronto_train_ts, toronto_holdout_ts,
     toronto_transformed_nhpi_train, toronto_transformed_nhpi_holdout,
     toronto_train_ir_xreg, toronto_holdout_ir_xreg,
     toronto_persist_fc, toronto_iid_fc,
     toronto_esm_fc, toronto_holt_fc, toronto_aseason_fc,
     vancouver_nhpi, vancouver_irlag1, vancouver_ts,
     vancouver_transformed_nhpi, vancouver_train_ts, vancouver_holdout_ts,
     vancouver_transformed_nhpi_train, vancouver_transformed_nhpi_holdout,
     vancouver_train_ir_xreg, vancouver_holdout_ir_xreg,
     vancouver_persist_fc, vancouver_iid_fc,
     vancouver_esm_fc, vancouver_holt_fc, vancouver_aseason_rmse,
     file = "esm_results.RData")
message("Saved: esm_results.RData")
