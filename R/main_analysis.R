# ==============================================================================
# ANALYSIS AND FORECASTING PIPELINE - MGP NORD
# ==============================================================================

# Load core libraries and custom functions
source("R/knn_library.R")
source("R/sarima_benchmark.R")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)
library(zoo)
library(matrixStats)


# ==============================================================================
# 1. DATA IMPORT AND CLEANING
# ==============================================================================

# Note: The Excel files must be placed in the 'data/' directory with colon-free names
data23 = read_excel("data/north_2023.xlsx", col_names = TRUE)
data24 = read_excel("data/north_2024.xlsx", col_names = TRUE)
data25 = read_excel("data/north_2025_h1.xlsx", col_names = TRUE)

hour23x = round(
  as.numeric(gsub(",", ".", data23$`€/MWh`)),
  2
)

hour24x = round(
  as.numeric(gsub(",", ".", data24$`€/MWh`)),
  2
)

hour25x = round(
  as.numeric(gsub(",", ".", data25$`€/MWh`)),
  2
)


# Handle Daylight Saving Time / Standard Time changes
# to preserve 24-hour profiles

# 2023
hour23 = append(
  hour23x,
  126.37,
  after = 2018
)

hour23 = hour23[-c(7227, 7228)]

hour23 = append(
  hour23,
  111.5,
  after = 7226
)


# 2024
hour24 = append(
  hour24x,
  95.12,
  after = 2162
)

hour24 = hour24[-c(7203, 7204)]

hour24 = append(
  hour24,
  96.495,
  after = 7202
)


# 2025
hour25 = append(
  hour25x,
  (116.50 + 112.52) / 2,
  after = 2114
)


# Build daily matrices for kNN
xmat23 = matrix(
  hour23,
  nrow = 365,
  ncol = 24,
  byrow = TRUE
)

xmat24 = matrix(
  hour24,
  nrow = 366,
  ncol = 24,
  byrow = TRUE
)

xmat25 = matrix(
  hour25,
  nrow = 181,
  ncol = 24,
  byrow = TRUE
)

xmat_full = rbind(
  xmat23,
  xmat24,
  xmat25
)


# Build hourly zoo series for SARIMA benchmark
full_hourly = c(
  hour23,
  hour24,
  hour25
)

time_index = seq(
  from = as.POSIXct(
    "2023-01-01 00:00",
    tz = "UTC"
  ),
  by = "hour",
  length.out = length(full_hourly)
)

full_series = zoo(
  full_hourly,
  order.by = time_index
)


# ==============================================================================
# 2. PARAMETER OPTIMIZATION & FORECASTS (EXAMPLE: JANUARY 2025)
# ==============================================================================

# Data subset up to December 2024 to optimize January 2025
mat_jan = xmat_full[1:731, ]


optimization_january = optimize_parameters(
  mat_jan,
  dim.prof = 24,
  n.prof = nrow(mat_jan),
  k.values = c(3, 5, 7, 10, 13, 15, 17, 20),
  n.test = 90
)

print("Best combination for January (kNN):")
print(optimization_january$best_combination)


# Extract the optimized parameters
best_k = optimization_january$best_combination$K
best_dist = optimization_january$best_combination$Distance
best_method = optimization_january$best_combination$Method


# Run rolling forecasts for January 2025 (kNN)
january_knn = iterative_forecast(
  xmat_full = xmat_full,
  dim.prof = 24,
  k = best_k,
  dist = best_dist,
  method = best_method,
  start_day = 731,
  horizon = 31
)


# Run rolling forecasts for January 2025 (SARIMA benchmark)
january_sarima = rolling_sarima_with_real_update(
  full_series = full_series,
  start_day = "2025-01-01",
  n_days_forecast = 31,
  training_days = 120
)


# ==============================================================================
# 3. RESULTS VISUALIZATION AND COMPARISON
# ==============================================================================

# Evaluate SARIMA results
sarima_eval = evaluate_forecast(
  forecast_zoo = january_sarima,
  start_day = "2025-01-01",
  n_days_forecast = 31,
  hour_full = full_hourly,
  model_name = "SARIMA + dummy"
)


# Plot Daily SMAPE Comparison (kNN vs SARIMA)
df_acc_jan = data.frame(
  day = 1:31,
  SMAPE_kNN = january_knn$accuracy$SMAPE,
  SMAPE_SARIMA = sarima_eval$daily_smape
)

df_acc_melt = melt(
  df_acc_jan,
  id.vars = "day",
  variable.name = "Model",
  value.name = "SMAPE"
)

ggplot(
  df_acc_melt,
  aes(
    x = day,
    y = SMAPE,
    color = Model
  )
) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_color_manual(
    values = c(
      "SMAPE_kNN" = "#4DAF4A",
      "SMAPE_SARIMA" = "#E41A1C"
    )
  ) +
  labs(
    title = "Daily SMAPE Comparison - January 2025",
    x = "Day",
    y = "SMAPE (%)"
  ) +
  theme_minimal()


# Extract actual January data for profile comparison
jan_actual = xmat_full[732:762, ]


# Plot average hourly profile comparison (Actual vs kNN Forecast)
df_profiles = data.frame(
  Hour = 1:24,
  Actual = colMeans(jan_actual),
  Forecast = colMeans(january_knn$predictions)
)

df_profiles_long = pivot_longer(
  df_profiles,
  cols = -Hour,
  names_to = "Type",
  values_to = "Price"
)

ggplot(
  df_profiles_long,
  aes(
    x = Hour,
    y = Price,
    color = Type,
    linetype = Type
  )
) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(
    values = c(
      "Actual" = "black",
      "Forecast" = "#4DAF4A"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Actual" = "solid",
      "Forecast" = "dashed"
    )
  ) +
  labs(
    title = "Average Hourly Profile - January 2025 (kNN)",
    x = "Hour of the Day",
    y = "Price (€/MWh)"
  ) +
  scale_x_continuous(
    breaks = 1:24
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )
