# =============================================================================
# 04_arimax.R
# STAT 443 Project - ARIMAX Modelling
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Fit ARIMAX models using lagged interest rate as external
#              predictor for Montreal, Toronto, and Vancouver.
#              Uses same ARIMA orders as selected in 03_arima.R.
# =============================================================================

library(forecast)

load("esm_results.RData")
load("arima_results.RData")

# -----------------------------------------------------------------------------
# Helper function: moving 1-step-ahead ARIMAX forecast
# -----------------------------------------------------------------------------

armax_fc <- function(tsdata, ntrain, order, seasonal, xreg, method, traincoef, include.mean, iprint = FALSE) {
  obj <- arima(tsdata, order = order, seasonal = seasonal,
               init = traincoef, fixed = traincoef, method = method,
               include.mean = include.mean, xreg = xreg,
               optim.control = list(maxit = 0))
  fc     <- tsdata - obj$residuals
  ntotal <- length(tsdata)
  holdout_fc <- fc[(ntrain+1):ntotal]
  holdout    <- tsdata[(ntrain+1):ntotal]
  if (iprint) print(cbind(holdout, holdout_fc))
  rmse <- sqrt(mean((holdout - holdout_fc)^2))
  return(list(rmse = rmse, fc = holdout_fc, residuals = obj$residuals))
}

# -----------------------------------------------------------------------------
# Montreal  -  ARIMAX(3,0,5)(2,0,1)[12] with IR_lag1
# -----------------------------------------------------------------------------

montreal_ir_ts         <- ts(montreal_irlag1, start = c(1981,1), end = c(2023,12), frequency = 12)
montreal_train_ir_xreg <- window(montreal_ir_ts, start = c(1981,2), end = c(2021,12))

montreal_fit_arimax <- forecast::auto.arima(montreal_train_ts, xreg = montreal_train_ir_xreg)
summary(montreal_fit_arimax)
acf(residuals(montreal_fit_arimax), main = "ACF: Montreal ARIMAX residuals")

train_arimax_montreal <- arima(montreal_train_ts,
                                order = montreal_order, seasonal = montreal_seasonal,
                                xreg = montreal_train_ir_xreg, method = "ML", include.mean = TRUE)
montreal_arimax_results <- armax_fc(tsdata = montreal_transformed_nhpi,
                                     ntrain = length(montreal_train_ts),
                                     order = montreal_order, seasonal = montreal_seasonal,
                                     xreg = montreal_irlag1[-1], method = "ML",
                                     traincoef = coef(train_arimax_montreal), include.mean = TRUE)
cat("Montreal ARIMAX RMSE:", montreal_arimax_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Toronto  -  ARIMAX(1,0,2)(1,0,0)[12] with IR_lag1
# -----------------------------------------------------------------------------

toronto_ir_ts         <- ts(toronto_irlag1, start = c(1981,1), end = c(2023,12), frequency = 12)
toronto_train_ir_xreg <- window(toronto_ir_ts, start = c(1981,2), end = c(2021,12))

toronto_fit_arimax <- forecast::auto.arima(toronto_train_ts, xreg = toronto_train_ir_xreg)
summary(toronto_fit_arimax)
acf(residuals(toronto_fit_arimax), main = "ACF: Toronto ARIMAX residuals")

train_arimax_toronto <- arima(toronto_train_ts,
                               order = toronto_order, seasonal = toronto_seasonal,
                               xreg = toronto_train_ir_xreg, method = "ML", include.mean = TRUE)
toronto_arimax_results <- armax_fc(tsdata = toronto_transformed_nhpi,
                                    ntrain = length(toronto_train_ts),
                                    order = toronto_order, seasonal = toronto_seasonal,
                                    xreg = toronto_irlag1[-1], method = "ML",
                                    traincoef = coef(train_arimax_toronto), include.mean = TRUE)
cat("Toronto ARIMAX RMSE:", toronto_arimax_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Vancouver  -  ARIMAX(2,0,1)(1,0,0)[12] with IR_lag1
# -----------------------------------------------------------------------------

vancouver_ir_ts         <- ts(vancouver_irlag1, start = c(1981,1), end = c(2023,12), frequency = 12)
vancouver_train_ir_xreg <- window(vancouver_ir_ts, start = c(1981,2), end = c(2021,12))

vancouver_fit_arimax <- forecast::auto.arima(vancouver_train_ts, xreg = vancouver_train_ir_xreg)
summary(vancouver_fit_arimax)
acf(residuals(vancouver_fit_arimax), main = "ACF: Vancouver ARIMAX residuals")

train_arimax_vancouver <- arima(vancouver_train_ts,
                                 order = vancouver_order, seasonal = vancouver_seasonal,
                                 xreg = vancouver_train_ir_xreg, method = "ML", include.mean = TRUE)
vancouver_arimax_results <- armax_fc(tsdata = vancouver_transformed_nhpi,
                                      ntrain = length(vancouver_train_ts),
                                      order = vancouver_order, seasonal = vancouver_seasonal,
                                      xreg = vancouver_irlag1[-1], method = "ML",
                                      traincoef = coef(train_arimax_vancouver), include.mean = TRUE)
cat("Vancouver ARIMAX RMSE:", vancouver_arimax_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------

save(armax_fc,
     montreal_arimax_results, montreal_fit_arimax,
     toronto_arimax_results,  toronto_fit_arimax,
     vancouver_arimax_results, vancouver_fit_arimax,
     file = "arimax_results.RData")
message("Saved: arimax_results.RData")
