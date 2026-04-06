# =============================================================================
# 04_arimax.R
# STAT 443 Project - ARIMAX Modelling
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Fit ARIMAX models with lagged interest rate for Montreal,
#              Toronto, and Vancouver. Uses same orders as 03_arima.R.
#   Best models:
#     Montreal  -> ARIMAX(2,0,5)(2,0,2)[12]
#     Toronto   -> ARIMAX(1,0,2)(1,0,0)[12]
#     Vancouver -> ARIMAX(1,0,2)(1,0,1)[12]
# =============================================================================

library(forecast)

load("esm_results.RData")
load("arima_results.RData")

# -----------------------------------------------------------------------------
# Helper function
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
# Montreal  -  ARIMAX(2,0,5)(2,0,2)[12]
# -----------------------------------------------------------------------------

montreal_fit_arimax <- forecast::auto.arima(montreal_train_ts, xreg = montreal_train_ir_xreg)
summary(montreal_fit_arimax)
acf(residuals(montreal_fit_arimax), main = "ACF: Montreal ARIMAX residuals")

train_arimax_montreal <- arima(montreal_train_ts,
                                order = montreal_order, seasonal = montreal_seasonal,
                                xreg = montreal_train_ir_xreg, method = "ML", include.mean = TRUE)
montreal_arimax_results <- armax_fc(montreal_transformed_nhpi, length(montreal_train_ts),
                                     montreal_order, montreal_seasonal,
                                     xreg = montreal_irlag1[-1], "ML",
                                     coef(train_arimax_montreal), TRUE)
cat("Montreal ARIMAX RMSE:", montreal_arimax_results$rmse, "\n")

montreal_best <- data.frame(
  City         = "Montreal",
  ARIMA_Model  = "ARIMA(2,0,5)(2,0,2)[12]",
  ARIMA_RMSE   = round(montreal_arima_results$rmse, 4),
  ARIMAX_Model = "ARIMAX(2,0,5)(2,0,2)[12]",
  ARIMAX_RMSE  = round(montreal_arimax_results$rmse, 4)
)

# -----------------------------------------------------------------------------
# Toronto  -  ARIMAX(1,0,2)(1,0,0)[12]
# -----------------------------------------------------------------------------

toronto_fit_arimax <- forecast::auto.arima(toronto_train_ts, xreg = toronto_train_ir_xreg)
summary(toronto_fit_arimax)
acf(residuals(toronto_fit_arimax), main = "ACF: Toronto ARIMAX residuals")

train_arimax_toronto <- arima(toronto_train_ts,
                               order = toronto_order, seasonal = toronto_seasonal,
                               xreg = toronto_train_ir_xreg, method = "ML", include.mean = TRUE)
toronto_arimax_results <- armax_fc(toronto_transformed_nhpi, length(toronto_train_ts),
                                    toronto_order, toronto_seasonal,
                                    xreg = toronto_irlag1[-1], "ML",
                                    coef(train_arimax_toronto), TRUE)
cat("Toronto ARIMAX RMSE:", toronto_arimax_results$rmse, "\n")

toronto_best <- data.frame(
  City         = "Toronto",
  ARIMA_Model  = "ARIMA(1,0,2)(1,0,0)[12]",
  ARIMA_RMSE   = round(toronto_arima_results$rmse, 4),
  ARIMAX_Model = "ARIMAX(1,0,2)(1,0,0)[12]",
  ARIMAX_RMSE  = round(toronto_arimax_results$rmse, 4)
)

# -----------------------------------------------------------------------------
# Vancouver  -  ARIMAX(1,0,2)(1,0,1)[12]
# -----------------------------------------------------------------------------

vancouver_fit_arimax <- forecast::auto.arima(vancouver_train_ts, xreg = vancouver_train_ir_xreg)
summary(vancouver_fit_arimax)
acf(residuals(vancouver_fit_arimax), main = "ACF: Vancouver ARIMAX residuals")

train_arimax_vancouver <- arima(vancouver_train_ts,
                                 order = vancouver_order, seasonal = vancouver_seasonal,
                                 xreg = vancouver_train_ir_xreg, method = "ML", include.mean = TRUE)
vancouver_arimax_results <- armax_fc(vancouver_transformed_nhpi, length(vancouver_train_ts),
                                      vancouver_order, vancouver_seasonal,
                                      xreg = vancouver_irlag1[-1], "ML",
                                      coef(train_arimax_vancouver), TRUE)
cat("Vancouver ARIMAX RMSE:", vancouver_arimax_results$rmse, "\n")

vancouver_best <- data.frame(
  City         = "Vancouver",
  ARIMA_Model  = "ARIMA(1,0,2)(1,0,1)[12]",
  ARIMA_RMSE   = round(vancouver_arima_results$rmse, 4),
  ARIMAX_Model = "ARIMAX(1,0,2)(1,0,1)[12]",
  ARIMAX_RMSE  = round(vancouver_arimax_results$rmse, 4)
)

# -----------------------------------------------------------------------------
# Best model summary table (for report)
# -----------------------------------------------------------------------------

best_model_table <- rbind(montreal_best, toronto_best, vancouver_best)
print(best_model_table)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(armax_fc,
     montreal_arimax_results, montreal_fit_arimax, montreal_best,
     toronto_arimax_results,  toronto_fit_arimax,  toronto_best,
     vancouver_arimax_results, vancouver_fit_arimax, vancouver_best,
     best_model_table,
     file = "arimax_results.RData")
message("Saved: arimax_results.RData")
