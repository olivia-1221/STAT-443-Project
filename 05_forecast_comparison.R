# =============================================================================
# 05_forecast_comparison.R
# STAT 443 Project - Final Forecast Comparison
# Authors: Tay, Chen, Jiang, Nguyen, Li
# Description: Combine holdout RMSE from all methods into a final comparison
#              table. Identify best overall method.
# =============================================================================

library(knitr)

load("esm_results.RData")
load("arima_results.RData")
load("arimax_results.RData")

# -----------------------------------------------------------------------------
# Final RMSE comparison table
# Best ESM per city:
#   Montreal  -> Linear Holt  (montreal_holt_fc)
#   Toronto   -> Simple ESM   (toronto_esm_fc)
#   Vancouver -> Simple ESM   (vancouver_esm_fc)
# -----------------------------------------------------------------------------

results_table <- data.frame(
  City    = c("Montreal", "Toronto", "Vancouver"),
  Persist = c(montreal_persist_fc$rmse,
              toronto_persist_fc$rmse,
              vancouver_persist_fc$rmse),
  IID     = c(montreal_iid_fc$rmse,
              toronto_iid_fc$rmse,
              vancouver_iid_fc$rmse),
  ESM     = c(montreal_holt_fc$rmse,
              toronto_esm_fc$rmse,
              vancouver_esm_fc$rmse),
  ARIMA   = c(montreal_arima_results$rmse,
              toronto_arima_results$rmse,
              vancouver_arima_results$rmse),
  ARIMAX  = c(montreal_arimax_results$rmse,
              toronto_arimax_results$rmse,
              vancouver_arimax_results$rmse)
)

avg_row <- data.frame(
  City    = "Average",
  Persist = mean(results_table$Persist),
  IID     = mean(results_table$IID),
  ESM     = mean(results_table$ESM),
  ARIMA   = mean(results_table$ARIMA),
  ARIMAX  = mean(results_table$ARIMAX)
)

final_table <- rbind(results_table, avg_row)
print(kable(final_table, digits = 4,
            caption = "One-Step-Ahead Rolling Forecast RMSE by City and Method"))

# Best overall method
avg_rmse    <- colMeans(results_table[, -1])
best_method <- names(which.min(avg_rmse))
cat("\nBest overall method:", best_method,
    "| Average RMSE =", round(min(avg_rmse), 4), "\n")

# -----------------------------------------------------------------------------
# Best ARIMA and ARIMAX models per city (for report)
# -----------------------------------------------------------------------------

print(best_model_table)

# -----------------------------------------------------------------------------
# All candidate ARIMA models (for Appendix)
# -----------------------------------------------------------------------------

print(all_arima_candidates)

# -----------------------------------------------------------------------------
# Save
# -----------------------------------------------------------------------------

write.csv(final_table,        "final_rmse_table.csv",      row.names = FALSE)
write.csv(best_model_table,   "best_model_table.csv",      row.names = FALSE)
write.csv(all_arima_candidates, "all_arima_candidates.csv", row.names = FALSE)
message("Saved: final_rmse_table.csv, best_model_table.csv, all_arima_candidates.csv")
