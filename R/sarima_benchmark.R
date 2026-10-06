# ==============================================================================
# SARIMA LIBRARY FOR BENCHMARKING (MGP)
# ==============================================================================

library(forecast)
library(lubridate)
library(tseries)
library(zoo)


# ==============================================================================
# ROLLING SARIMA FORECAST FUNCTION WITH DUMMIES
# ==============================================================================

rolling_sarima_with_real_update = function(full_series,
                                           start_day = "2025-01-01",
                                           n_days_forecast = 30,
                                           training_days = 120) {
  
  h = 24
  
  # Force UTC
  index(full_series) = lubridate::with_tz(
    index(full_series),
    tzone = "UTC"
  )
  
  
  # --- PRE-BUILD THE DUMMIES FOR THE WHOLE SERIES ---
  
  # week_start = 1 ensures Monday = 1 independently of OS language settings
  day_of_week = lubridate::wday(
    time(full_series),
    week_start = 1
  )
  
  # Order the levels and set Monday as the reference
  day_of_week = factor(
    day_of_week,
    levels = 1:7,
    ordered = FALSE
  )
  
  contrasts(day_of_week) = contr.treatment(7)
  
  # Create the dummy matrix (Monday excluded)
  # and select Friday, Saturday and Sunday
  dummies = model.matrix(~ day_of_week)[, -1]
  
  dummies = dummies[
    ,
    c("day_of_week5", "day_of_week6", "day_of_week7")
  ]
  
  
  # Predictions vector
  all_predictions = numeric(
    n_days_forecast * h
  )
  
  
  for (i in 0:(n_days_forecast - 1)) {
    
    current_day = as.POSIXct(
      start_day,
      tz = "UTC"
    ) + lubridate::days(i)
    
    train_start = current_day - lubridate::days(training_days)
    train_end = current_day - lubridate::hours(1)
    
    
    # Training window
    y_train = window(
      full_series,
      start = train_start,
      end = train_end
    )
    
    if (length(y_train) < h * training_days) {
      stop("Not enough data for training.")
    }
    
    
    # Dummies matching the training window
    xreg_train = dummies[
      time(full_series) >= train_start &
        time(full_series) <= train_end,
      ,
      drop = FALSE
    ]
    
    
    # Fit SARIMA model (2,0,0)(0,1,2)[24]
    model = forecast::Arima(
      y_train,
      order = c(2, 0, 0),
      seasonal = list(
        order = c(0, 1, 2),
        period = 24
      ),
      xreg = xreg_train,
      include.mean = TRUE,
      method = "CSS"
    )
    
    
    # Forecast window
    forecast_start = current_day
    forecast_end = current_day + lubridate::hours(23)
    
    xreg_pred = dummies[
      time(full_series) >= forecast_start &
        time(full_series) <= forecast_end,
      ,
      drop = FALSE
    ]
    
    
    # 24h forecast
    forecast_result = forecast::forecast(
      model,
      h = h,
      xreg = xreg_pred
    )
    
    
    # Store
    all_predictions[
      (i * h + 1):((i + 1) * h)
    ] = forecast_result$mean
  }
  
  
  # Output as zoo
  prediction_index = seq(
    from = as.POSIXct(start_day, tz = "UTC"),
    by = "hour",
    length.out = n_days_forecast * h
  )
  
  return(
    zoo::zoo(
      all_predictions,
      order.by = prediction_index
    )
  )
}


# ==============================================================================
# EVALUATION AND PLOT FUNCTION
# ==============================================================================

evaluate_forecast = function(forecast_zoo,
                             start_day,
                             n_days_forecast,
                             hour_full,
                             col_actual = "black",
                             col_forecast = "red",
                             model_name = "SARIMA") {
  
  # Compute observed index
  obs_start = as.numeric(
    difftime(
      as.POSIXct(start_day, tz = "UTC"),
      as.POSIXct("2023-01-01 00:00", tz = "UTC"),
      units = "hours"
    )
  ) + 1
  
  obs_end = obs_start + (n_days_forecast * 24) - 1
  
  observed_values = hour_full[
    obs_start:obs_end
  ]
  
  observed_dates = seq(
    from = as.POSIXct(start_day, tz = "UTC"),
    by = "hour",
    length.out = length(observed_values)
  )
  
  y_hat = coredata(forecast_zoo)
  
  
  # Compute overall metrics
  mae = mean(
    abs(y_hat - observed_values)
  )
  
  rmse = sqrt(
    mean((y_hat - observed_values)^2)
  )
  
  epsilon = 1e-6
  
  smape = mean(
    2 * abs(y_hat - observed_values) /
      (abs(y_hat) + abs(observed_values) + epsilon)
  ) * 100
  
  
  # Compute daily metrics
  daily_mae = numeric(n_days_forecast)
  daily_rmse = numeric(n_days_forecast)
  daily_smape = numeric(n_days_forecast)
  
  absolute_error = matrix(
    NA,
    nrow = n_days_forecast,
    ncol = 24
  )
  
  
  for (i in 1:n_days_forecast) {
    
    idx = ((i - 1) * 24 + 1):(i * 24)
    
    y_true_day = observed_values[idx]
    y_pred_day = y_hat[idx]
    
    absolute_error[i, ] = abs(
      y_pred_day - y_true_day
    )
    
    daily_mae[i] = mean(
      absolute_error[i, ]
    )
    
    daily_rmse[i] = sqrt(
      mean((y_pred_day - y_true_day)^2)
    )
    
    daily_smape[i] = mean(
      2 * abs(y_pred_day - y_true_day) /
        (abs(y_pred_day) + abs(y_true_day) + epsilon)
    ) * 100
  }
  
  
  return(
    invisible(
      list(
        mae = mae,
        rmse = rmse,
        smape = smape,
        daily_mae = daily_mae,
        daily_rmse = daily_rmse,
        daily_smape = daily_smape,
        absolute_error = absolute_error
      )
    )
  )
}
