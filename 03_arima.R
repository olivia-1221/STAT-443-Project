# =============================================================================
# 03_arima.R
# STAT 443 Project - ARIMA Modelling
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Fit and select SARIMA models for Montreal, Toronto, and
#              Vancouver using AIC and holdout RMSE. Produces moving
#              1-step-ahead forecasts for the holdout set (2022-2023).
# =============================================================================

library(forecast)

load("esm_results.RData")

# -----------------------------------------------------------------------------
# Helper function: moving 1-step-ahead ARIMA forecast
# -----------------------------------------------------------------------------

arma_fc <- function(tsdata, ntrain, order, seasonal, method, traincoef, include.mean, iprint = FALSE) {
  obj <- arima(tsdata, order = order, seasonal = seasonal,
               init = traincoef, fixed = traincoef, method = method,
               include.mean = include.mean, optim.control = list(maxit = 0))
  fc     <- tsdata - obj$residuals
  ntotal <- length(tsdata)
  holdout_fc <- fc[(ntrain+1):ntotal]
  holdout    <- tsdata[(ntrain+1):ntotal]
  if (iprint) print(cbind(holdout, holdout_fc))
  rmse <- sqrt(mean((holdout - holdout_fc)^2))
  return(list(rmse = rmse, fc = holdout_fc, residuals = obj$residuals))
}

# -----------------------------------------------------------------------------
# Montreal  -  Best model: ARIMA(3,0,5)(2,0,1)[12]
# -----------------------------------------------------------------------------

montreal_transformed_nphi_auto <- forecast::auto.arima(montreal_train_ts)
summary(montreal_transformed_nphi_auto)
acf(residuals(montreal_transformed_nphi_auto), main = "ACF: Montreal auto.arima residuals")

# Candidate models
manual_fit <- arima(montreal_train_ts, order=c(2,0,5), seasonal=list(order=c(2,0,1),period=12), method="ML")
test1      <- arima(montreal_train_ts, order=c(2,0,4), seasonal=list(order=c(2,0,1),period=12), method="ML")
test2      <- arima(montreal_train_ts, order=c(3,0,4), seasonal=list(order=c(2,0,1),period=12), method="ML")
test3      <- arima(montreal_train_ts, order=c(3,0,5), seasonal=list(order=c(2,0,1),period=12), method="ML")
test4      <- arima(montreal_train_ts, order=c(2,0,5), seasonal=list(order=c(2,0,2),period=12), method="ML")
test5      <- arima(montreal_train_ts, order=c(2,0,4), seasonal=list(order=c(2,0,2),period=12), method="ML")

aic_montreal <- cbind(manual_fit$aic, test1$aic, test2$aic, test3$aic, test4$aic, test5$aic)
colnames(aic_montreal) <- c("(2,0,5)(2,0,1)", "(2,0,4)(2,0,1)", "(3,0,4)(2,0,1)",
                             "(3,0,5)(2,0,1)", "(2,0,5)(2,0,2)", "(2,0,4)(2,0,2)")
print(aic_montreal)
# Best: test3 -> ARIMA(3,0,5)(2,0,1)[12]

montreal_order    <- c(3,0,5)
montreal_seasonal <- list(order = c(2,0,1), period = 12)
train_arima_montreal <- arima(montreal_train_ts, order = montreal_order,
                               seasonal = montreal_seasonal, method = "ML", include.mean = TRUE)
montreal_arima_results <- arma_fc(tsdata = montreal_transformed_nhpi,
                                   ntrain = length(montreal_train_ts),
                                   order = montreal_order, seasonal = montreal_seasonal,
                                   method = "ML", traincoef = coef(train_arima_montreal),
                                   include.mean = TRUE)
cat("Montreal ARIMA RMSE:", montreal_arima_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Toronto  -  Best model: ARIMA(1,0,2)(1,0,0)[12]
# -----------------------------------------------------------------------------

toronto_transformed_nphi_auto <- forecast::auto.arima(toronto_train_ts)
summary(toronto_transformed_nphi_auto)
acf(residuals(toronto_transformed_nphi_auto), main = "ACF: Toronto auto.arima residuals")

manual_fit <- arima(toronto_train_ts, order=c(1,0,2), seasonal=list(order=c(1,0,0),period=12), method="ML")
test1      <- arima(toronto_train_ts, order=c(1,0,1), seasonal=list(order=c(1,0,0),period=12), method="ML")
test2      <- arima(toronto_train_ts, order=c(2,0,1), seasonal=list(order=c(1,0,0),period=12), method="ML")
test3      <- arima(toronto_train_ts, order=c(2,0,2), seasonal=list(order=c(1,0,0),period=12), method="ML")
test4      <- arima(toronto_train_ts, order=c(1,0,2), seasonal=list(order=c(1,0,1),period=12), method="ML")
test5      <- arima(toronto_train_ts, order=c(1,0,1), seasonal=list(order=c(1,0,1),period=12), method="ML")

aic_toronto <- cbind(manual_fit$aic, test1$aic, test2$aic, test3$aic, test4$aic, test5$aic)
colnames(aic_toronto) <- c("(1,0,2)(1,0,0)", "(1,0,1)(1,0,0)", "(2,0,1)(1,0,0)",
                            "(2,0,2)(1,0,0)", "(1,0,2)(1,0,1)", "(1,0,1)(1,0,1)")
print(aic_toronto)
# Best: manual_fit -> ARIMA(1,0,2)(1,0,0)[12]

toronto_order    <- c(1,0,2)
toronto_seasonal <- list(order = c(1,0,0), period = 12)
train_arima_toronto <- arima(toronto_train_ts, order = toronto_order,
                              seasonal = toronto_seasonal, method = "ML", include.mean = TRUE)
toronto_arima_results <- arma_fc(tsdata = toronto_transformed_nhpi,
                                  ntrain = length(toronto_train_ts),
                                  order = toronto_order, seasonal = toronto_seasonal,
                                  method = "ML", traincoef = coef(train_arima_toronto),
                                  include.mean = TRUE)
cat("Toronto ARIMA RMSE:", toronto_arima_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Vancouver  -  Best model: ARIMA(2,0,1)(1,0,0)[12]
# -----------------------------------------------------------------------------

vancouver_auto_arima <- forecast::auto.arima(vancouver_train_ts)
summary(vancouver_auto_arima)
acf(residuals(vancouver_auto_arima), main = "ACF: Vancouver auto.arima residuals")

fit1 <- arima(vancouver_train_ts, order=c(1,0,1), seasonal=list(order=c(1,0,0),period=12), method="ML")
fit2 <- arima(vancouver_train_ts, order=c(1,0,2), seasonal=list(order=c(1,0,0),period=12), method="ML")
fit3 <- arima(vancouver_train_ts, order=c(2,0,1), seasonal=list(order=c(1,0,0),period=12), method="ML")
fit4 <- arima(vancouver_train_ts, order=c(2,0,2), seasonal=list(order=c(1,0,0),period=12), method="ML")
fit5 <- arima(vancouver_train_ts, order=c(1,0,2), seasonal=list(order=c(1,0,1),period=12), method="ML")

aic_vancouver <- cbind(fit1$aic, fit2$aic, fit3$aic, fit4$aic, fit5$aic)
colnames(aic_vancouver) <- c("(1,0,1)(1,0,0)", "(1,0,2)(1,0,0)", "(2,0,1)(1,0,0)",
                              "(2,0,2)(1,0,0)", "(1,0,2)(1,0,1)")
print(aic_vancouver)
# Best: fit3 -> ARIMA(2,0,1)(1,0,0)[12]

vancouver_order    <- c(2,0,1)
vancouver_seasonal <- list(order = c(1,0,0), period = 12)
train_arima_vancouver <- arima(vancouver_train_ts, order = vancouver_order,
                                seasonal = vancouver_seasonal, method = "ML", include.mean = TRUE)
vancouver_arima_results <- arma_fc(tsdata = vancouver_transformed_nhpi,
                                    ntrain = length(vancouver_train_ts),
                                    order = vancouver_order, seasonal = vancouver_seasonal,
                                    method = "ML", traincoef = coef(train_arima_vancouver),
                                    include.mean = TRUE)
cat("Vancouver ARIMA RMSE:", vancouver_arima_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Save results
# -----------------------------------------------------------------------------

save(arma_fc,
     montreal_order, montreal_seasonal, montreal_arima_results,
     montreal_transformed_nphi_auto, aic_montreal,
     toronto_order, toronto_seasonal, toronto_arima_results,
     toronto_transformed_nphi_auto, aic_toronto,
     vancouver_order, vancouver_seasonal, vancouver_arima_results,
     vancouver_auto_arima, aic_vancouver,
     file = "arima_results.RData")
message("Saved: arima_results.RData")
