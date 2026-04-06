# =============================================================================
# 03_arima.R
# STAT 443 Project - ARIMA Modelling
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Fit and select SARIMA models for Montreal, Toronto, and
#              Vancouver using AIC and holdout RMSE.
#   Best models (by holdout RMSE):
#     Montreal  -> ARIMA(2,0,5)(2,0,2)[12]
#     Toronto   -> ARIMA(1,0,2)(1,0,0)[12]
#     Vancouver -> ARIMA(1,0,2)(1,0,1)[12]
# =============================================================================

library(forecast)

load("esm_results.RData")

# -----------------------------------------------------------------------------
# Helper function
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
# Montreal
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

# Compare by AIC
aic_montreal <- cbind(manual_fit$aic, test1$aic, test2$aic, test3$aic, test4$aic, test5$aic)
colnames(aic_montreal) <- c("(2,0,5)(2,0,1)","(2,0,4)(2,0,1)","(3,0,4)(2,0,1)",
                             "(3,0,5)(2,0,1)","(2,0,5)(2,0,2)","(2,0,4)(2,0,2)")
print(aic_montreal)

# Compare by holdout RMSE
rmse_manual_fit <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts), c(2,0,5), list(order=c(2,0,1),period=12), "ML", coef(manual_fit), TRUE)$rmse
rmse_test1      <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts), c(2,0,4), list(order=c(2,0,1),period=12), "ML", coef(test1), TRUE)$rmse
rmse_test2      <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts), c(3,0,4), list(order=c(2,0,1),period=12), "ML", coef(test2), TRUE)$rmse
rmse_test3      <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts), c(3,0,5), list(order=c(2,0,1),period=12), "ML", coef(test3), TRUE)$rmse
rmse_test4      <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts), c(2,0,5), list(order=c(2,0,2),period=12), "ML", coef(test4), TRUE)$rmse
rmse_test5      <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts), c(2,0,4), list(order=c(2,0,2),period=12), "ML", coef(test5), TRUE)$rmse

montreal_arima_rmse_compare <- data.frame(
  City  = "Montreal",
  Candidate_ARIMA_Model = c("ARIMA(2,0,5)(2,0,1)[12]","ARIMA(2,0,4)(2,0,1)[12]",
                             "ARIMA(3,0,4)(2,0,1)[12]","ARIMA(3,0,5)(2,0,1)[12]",
                             "ARIMA(2,0,5)(2,0,2)[12]","ARIMA(2,0,4)(2,0,2)[12]"),
  Holdout_RMSE = round(c(rmse_manual_fit, rmse_test1, rmse_test2, rmse_test3, rmse_test4, rmse_test5), 4)
)
print(montreal_arima_rmse_compare)
# Best by holdout RMSE: ARIMA(2,0,5)(2,0,2)[12]

montreal_order    <- c(2,0,5)
montreal_seasonal <- list(order = c(2,0,2), period = 12)
train_arima_montreal <- arima(montreal_train_ts, order = montreal_order,
                               seasonal = montreal_seasonal, method = "ML", include.mean = TRUE)
montreal_arima_results <- arma_fc(montreal_transformed_nhpi, length(montreal_train_ts),
                                   montreal_order, montreal_seasonal, "ML",
                                   coef(train_arima_montreal), TRUE)
cat("Montreal ARIMA RMSE:", montreal_arima_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Toronto
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
colnames(aic_toronto) <- c("(1,0,2)(1,0,0)","(1,0,1)(1,0,0)","(2,0,1)(1,0,0)",
                            "(2,0,2)(1,0,0)","(1,0,2)(1,0,1)","(1,0,1)(1,0,1)")
print(aic_toronto)

rmse_manual_fit <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts), c(1,0,2), list(order=c(1,0,0),period=12), "ML", coef(manual_fit), TRUE)$rmse
rmse_test1      <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts), c(1,0,1), list(order=c(1,0,0),period=12), "ML", coef(test1), TRUE)$rmse
rmse_test2      <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts), c(2,0,1), list(order=c(1,0,0),period=12), "ML", coef(test2), TRUE)$rmse
rmse_test3      <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts), c(2,0,2), list(order=c(1,0,0),period=12), "ML", coef(test3), TRUE)$rmse
rmse_test4      <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts), c(1,0,2), list(order=c(1,0,1),period=12), "ML", coef(test4), TRUE)$rmse
rmse_test5      <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts), c(1,0,1), list(order=c(1,0,1),period=12), "ML", coef(test5), TRUE)$rmse

toronto_arima_rmse_compare <- data.frame(
  City  = "Toronto",
  Candidate_ARIMA_Model = c("ARIMA(1,0,2)(1,0,0)[12]","ARIMA(1,0,1)(1,0,0)[12]",
                             "ARIMA(2,0,1)(1,0,0)[12]","ARIMA(2,0,2)(1,0,0)[12]",
                             "ARIMA(1,0,2)(1,0,1)[12]","ARIMA(1,0,1)(1,0,1)[12]"),
  Holdout_RMSE = round(c(rmse_manual_fit, rmse_test1, rmse_test2, rmse_test3, rmse_test4, rmse_test5), 4)
)
print(toronto_arima_rmse_compare)
# Best by holdout RMSE: ARIMA(1,0,2)(1,0,0)[12]

toronto_order    <- c(1,0,2)
toronto_seasonal <- list(order = c(1,0,0), period = 12)
train_arima_toronto <- arima(toronto_train_ts, order = toronto_order,
                              seasonal = toronto_seasonal, method = "ML", include.mean = TRUE)
toronto_arima_results <- arma_fc(toronto_transformed_nhpi, length(toronto_train_ts),
                                  toronto_order, toronto_seasonal, "ML",
                                  coef(train_arima_toronto), TRUE)
cat("Toronto ARIMA RMSE:", toronto_arima_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Vancouver
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
colnames(aic_vancouver) <- c("(1,0,1)(1,0,0)","(1,0,2)(1,0,0)","(2,0,1)(1,0,0)",
                              "(2,0,2)(1,0,0)","(1,0,2)(1,0,1)")
print(aic_vancouver)

rmse_fit1 <- arma_fc(vancouver_transformed_nhpi, length(vancouver_train_ts), c(1,0,1), list(order=c(1,0,0),period=12), "ML", coef(fit1), TRUE)$rmse
rmse_fit2 <- arma_fc(vancouver_transformed_nhpi, length(vancouver_train_ts), c(1,0,2), list(order=c(1,0,0),period=12), "ML", coef(fit2), TRUE)$rmse
rmse_fit3 <- arma_fc(vancouver_transformed_nhpi, length(vancouver_train_ts), c(2,0,1), list(order=c(1,0,0),period=12), "ML", coef(fit3), TRUE)$rmse
rmse_fit4 <- arma_fc(vancouver_transformed_nhpi, length(vancouver_train_ts), c(2,0,2), list(order=c(1,0,0),period=12), "ML", coef(fit4), TRUE)$rmse
rmse_fit5 <- arma_fc(vancouver_transformed_nhpi, length(vancouver_train_ts), c(1,0,2), list(order=c(1,0,1),period=12), "ML", coef(fit5), TRUE)$rmse

vancouver_arima_rmse_compare <- data.frame(
  City  = "Vancouver",
  Candidate_ARIMA_Model = c("ARIMA(1,0,1)(1,0,0)[12]","ARIMA(1,0,2)(1,0,0)[12]",
                             "ARIMA(2,0,1)(1,0,0)[12]","ARIMA(2,0,2)(1,0,0)[12]",
                             "ARIMA(1,0,2)(1,0,1)[12]"),
  Holdout_RMSE = round(c(rmse_fit1, rmse_fit2, rmse_fit3, rmse_fit4, rmse_fit5), 4)
)
print(vancouver_arima_rmse_compare)
# Best by holdout RMSE: ARIMA(1,0,2)(1,0,1)[12]

vancouver_order    <- c(1,0,2)
vancouver_seasonal <- list(order = c(1,0,1), period = 12)
train_arima_vancouver <- arima(vancouver_train_ts, order = vancouver_order,
                                seasonal = vancouver_seasonal, method = "ML", include.mean = TRUE)
vancouver_arima_results <- arma_fc(vancouver_transformed_nhpi, length(vancouver_train_ts),
                                    vancouver_order, vancouver_seasonal, "ML",
                                    coef(train_arima_vancouver), TRUE)
cat("Vancouver ARIMA RMSE:", vancouver_arima_results$rmse, "\n")

# -----------------------------------------------------------------------------
# Combined candidate model table (for Appendix)
# -----------------------------------------------------------------------------

all_arima_candidates <- rbind(montreal_arima_rmse_compare,
                               toronto_arima_rmse_compare,
                               vancouver_arima_rmse_compare)
print(all_arima_candidates)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

save(arma_fc,
     montreal_order, montreal_seasonal, montreal_arima_results,
     montreal_transformed_nphi_auto, montreal_arima_rmse_compare,
     toronto_order, toronto_seasonal, toronto_arima_results,
     toronto_transformed_nphi_auto, toronto_arima_rmse_compare,
     vancouver_order, vancouver_seasonal, vancouver_arima_results,
     vancouver_auto_arima, vancouver_arima_rmse_compare,
     all_arima_candidates,
     file = "arima_results.RData")
message("Saved: arima_results.RData")
