# ==============================================================================
# kNN LIBRARY FOR ELECTRICITY PRICE FORECASTING (Italian day-ahead market, MGP)
# ==============================================================================
# Companion code to the bachelor's thesis "Previsione dei prezzi del mercato
# elettrico italiano mediante il metodo dei vicini piu' vicini"
# (University of Padova, A.Y. 2024/25).
#
# The method forecasts a whole daily profile (m = 24 hourly prices) from the
# k most similar past daily profiles: the forecast is an aggregation (mean,
# median or distance-weighted mean) of the profiles that FOLLOWED those
# neighbours. Only complete daily profiles are compared with each other.
#
# Data format
#   xmat: numeric matrix with one row per day and one column per hour
#         (d x 24), no missing values, rows in chronological order.
#
# Minimal example
#   source("knn_library.R")
#   out = knn(xmat, k = 10, dist = "manhattan", method = "wknn")
#   out$pred                       # forecast of the day after the last row
#
#   opt = optimize_parameters(xmat, k.values = c(5, 10, 20), n.test = 90)
#   opt$best_combination           # best (method, distance, k) by mean RMSE
#
# Note: the function `accuracy` has the same name as forecast::accuracy.
# If both are needed, call the other one explicitly (forecast::accuracy).
#
# No external packages are required (only base R).
# ==============================================================================


# ------------------------------------------------------------------------------
# Distances
# ------------------------------------------------------------------------------

# Euclidean distance
euc.dist = function(v1, v2) {
  return(sqrt(sum((v1 - v2)^2)))
}


# Manhattan distance
manhattan.dist = function(v1, v2) {
  return(sum(abs(v1 - v2)))
}


# ==============================================================================
# MAIN kNN FUNCTION (one-step-ahead forecast of a daily profile)
# ==============================================================================
# Arguments
# - xmat:     data matrix [one row = one complete daily profile]
# - dim.prof: length of each profile [default ncol(xmat)]
# - n.prof:   number of profiles [default nrow(xmat)]
# - k:        number of neighbours
# - xlast:    index of the profile preceding the one to forecast, i.e. the
#             "instance" [default: the last available profile = true forecast]
# - dist:     distance between profiles ("euclidean" or "manhattan")
# - method:   how the targets of the neighbours are combined
#             ("mean", "median", "wknn" = inverse-distance weighted mean)
# - accuracy: if TRUE, compute MAE, RMSE, SMAPE and the absolute errors
#             [only possible if xlast is not the last profile, i.e. when the
#             true profile following xlast is available]
#
# Only profiles 1, ..., xlast - 1 are used as candidate neighbours, so no
# information after xlast is ever used (the instance itself is excluded).
#
# Value: list with
# - knn:   k x dim.prof matrix of the neighbours
# - knn1:  k x dim.prof matrix of the targets (profiles following the neighbours)
# - kdist: distances of the k neighbours from the instance
# - pred:  forecast of the profile following xlast (vector of length dim.prof)
# - errors (only if accuracy = TRUE and available): MAE, RMSE, SMAPE,
#          ABSOLUTE_ERROR (hourly absolute errors)

knn = function(xmat,
               dim.prof = ncol(xmat),
               n.prof = nrow(xmat),
               k = 3,
               xlast = nrow(xmat),
               dist = c("euclidean", "manhattan"),
               method = c("mean", "median", "wknn"),
               accuracy = FALSE) {
  
  # ---- Checks ----------------------------------------------------------------
  
  dist = match.arg(dist, several.ok = FALSE)
  method = match.arg(method, several.ok = FALSE)
  xmat = as.matrix(xmat)
  
  if (anyNA(xmat)) {
    stop("xmat contains NA values: the series must be complete (dim.prof values per day)")
  }
  
  if (xlast > n.prof) {
    stop("xlast must be an existing profile")
  }
  
  if (xlast < 2) {
    stop("xlast cannot be the first profile: there are no previous profiles to compare with")
  }
  
  if (k < 1) {
    stop("k must be at least 1")
  }
  
  if ((xlast - 1) < k) {
    stop("Not enough previous profiles to select k neighbours")
  }
  
  
  # ---- Matrices ---------------------------------------------------------------
  
  # xmat1: profile following each row of xmat
  # the last row has no successor -> NA
  xmat1 = rbind(xmat[2:n.prof, , drop = FALSE], rep(NA, dim.prof))
  
  # xmat01: input profiles side by side with their successors
  xmat01 = cbind(xmat, xmat1)
  
  
  # Distances between the instance (row xlast) and the previous profiles.
  # Profiles from xlast onwards are left as NA, so they are never selected.
  dist.fun = if (dist == "euclidean") euc.dist else manhattan.dist
  
  vdist = rep(NA_real_, n.prof)
  
  for (i in seq_len(xlast - 1)) {
    vdist[i] = dist.fun(xmat[xlast, ], xmat[i, ])
  }
  
  
  # profile | successor | distance from the instance
  xmat01 = cbind(xmat01, vdist)
  
  
  # Sort by increasing distance; rows with NA distance are dropped
  xmat01.ord = xmat01[
    order(xmat01[, 2 * dim.prof + 1], na.last = NA),
    ,
    drop = FALSE
  ]
  
  
  # ---- k nearest neighbours, their targets and distances ----------------------
  
  idx = seq_len(k)
  
  knn_matrix = xmat01.ord[idx, 1:dim.prof, drop = FALSE]
  
  knn1 = xmat01.ord[
    idx,
    (dim.prof + 1):(2 * dim.prof),
    drop = FALSE
  ]
  
  kdist = xmat01.ord[idx, 2 * dim.prof + 1]
  
  
  # ---- Forecast ----------------------------------------------------------------
  
  pred = switch(
    method,
    mean = colMeans(knn1),
    median = apply(knn1, 2, median),
    wknn = {
      # weights inversely proportional to the distance, normalised to sum to 1
      # (pmax avoids division by zero if a neighbour is identical to the instance)
      w = 1 / pmax(kdist, 1e-8)
      w = w / sum(w)
      colSums(knn1 * w)
    }
  )
  
  pred = unname(pred)
  
  
  # ---- Accuracy (only if the true following profile is available) --------------
  
  if (accuracy == TRUE) {
    
    if (xlast == n.prof) {
      
      warning(
        "Cannot compute MAE, RMSE and SMAPE: the profile following xlast is not available"
      )
      
    } else {
      
      true_values = xmat1[xlast, ]
      
      mae = function(pred, true) {
        mean(abs(pred - true), na.rm = TRUE)
      }
      
      rmse = function(pred, true) {
        sqrt(mean((pred - true)^2, na.rm = TRUE))
      }
      
      smape = function(pred, true) {
        mean(
          2 * abs(true - pred) /
            (abs(true) + abs(pred) + 1e-6),
          na.rm = TRUE
        ) * 100
      }
      
      abserr = function(pred, true) {
        abs(true - pred)
      }
      
      errors = list(
        MAE = mae(pred, true_values),
        RMSE = rmse(pred, true_values),
        SMAPE = smape(pred, true_values),
        ABSOLUTE_ERROR = abserr(pred, true_values)
      )
      
      return(
        list(
          knn = knn_matrix,
          knn1 = knn1,
          kdist = kdist,
          pred = pred,
          errors = errors
        )
      )
    }
  }
  
  
  # accuracy = FALSE, or xlast is the last profile: base output only
  return(
    list(
      knn = knn_matrix,
      knn1 = knn1,
      kdist = kdist,
      pred = pred
    )
  )
}


# ==============================================================================
# ACCURACY OVER MANY FORECASTS (rolling origin, one-step ahead)
# ==============================================================================
# Forecasts the last n.test profiles with the data available before each one:
# the instances are xlast = n.prof - 1, n.prof - 2, ..., n.prof - n.test, and
# each forecast uses only profiles before xlast (no look-ahead).
#
# Value: list with
# - mean:           average MAE, RMSE, SMAPE over the n.test forecasts
# - sd:             standard deviation of the three measures
# - raw:            n.test x 3 matrix of the individual measures
# - absolute_error: n.test x dim.prof matrix of hourly absolute errors

accuracy = function(xmat,
                    dim.prof = ncol(xmat),
                    n.prof = nrow(xmat),
                    k,
                    dist,
                    method,
                    n.test = 3) {
  
  if (n.test < 1) {
    stop("n.test must be at least 1")
  }
  
  if ((n.prof - n.test - 1) < k) {
    stop("n.test too large for this k: not enough previous profiles for the earliest test")
  }
  
  results = matrix(NA, nrow = n.test, ncol = 3)
  colnames(results) = c("MAE", "RMSE", "SMAPE")
  
  absolute_error = matrix(
    NA,
    nrow = n.test,
    ncol = dim.prof
  )
  
  for (i in 1:n.test) {
    
    xlast = n.prof - i
    
    output = knn(
      xmat = xmat,
      dim.prof = dim.prof,
      n.prof = n.prof,
      k = k,
      xlast = xlast,
      dist = dist,
      method = method,
      accuracy = TRUE
    )
    
    results[i, ] = unlist(
      output$errors[c("MAE", "RMSE", "SMAPE")]
    )
    
    absolute_error[i, ] = output$errors$ABSOLUTE_ERROR
  }
  
  mu = colMeans(results)
  s = apply(results, 2, sd)
  
  return(
    list(
      mean = mu,
      sd = s,
      raw = results,
      absolute_error = absolute_error
    )
  )
}


# ==============================================================================
# PARAMETER OPTIMISATION
# ==============================================================================
# Tests every combination of aggregation method (mean, median, wknn), distance
# (euclidean, manhattan) and k in k.values on the last n.test profiles of xmat
# (via `accuracy`), then picks the combination with the lowest mean daily RMSE.
#
# - xmat, dim.prof, n.prof: as in `knn` (xmat must stop at the last day that is
#   allowed to be used for the choice, to avoid using future information)
# - k.values: candidate numbers of neighbours
# - n.test:   number of past profiles forecast for each combination
# - plot:     if TRUE, plot MAE, RMSE and SMAPE against k for the best
#             (method, distance) pair
#
# Value: list with
# - best_combination: row with the best (lowest mean RMSE) combination
# - summary_table:    all combinations, sorted by increasing mean RMSE
#                     (head(summary_table, 5) gives the 5 best combinations)

optimize_parameters = function(xmat,
                               dim.prof = ncol(xmat),
                               n.prof = nrow(xmat),
                               k.values = c(3, 5, 7, 10, 13, 15, 17, 20),
                               n.test = 90,
                               plot = FALSE) {
  
  methods = c("mean", "median", "wknn")
  distances = c("euclidean", "manhattan")
  results = data.frame()
  
  for (m in methods) {
    for (d in distances) {
      for (k in k.values) {
        
        acc = accuracy(
          xmat = xmat,
          dim.prof = dim.prof,
          n.prof = n.prof,
          k = k,
          dist = d,
          method = m,
          n.test = n.test
        )
        
        results = rbind(
          results,
          data.frame(
            Method = m,
            Distance = d,
            K = k,
            MAE = acc$mean[1],
            RMSE = acc$mean[2],
            SMAPE = acc$mean[3]
          )
        )
      }
    }
  }
  
  results = results[order(results$RMSE), ]
  rownames(results) = NULL
  
  best_row = results[1, ]
  
  if (plot) {
    
    subset_plot = results[
      results$Method == best_row$Method &
        results$Distance == best_row$Distance,
    ]
    
    subset_plot = subset_plot[order(subset_plot$K), ]
    
    old.par = par(mfrow = c(1, 3))
    on.exit(par(old.par))
    
    for (measure in c("MAE", "RMSE", "SMAPE")) {
      
      plot(
        subset_plot$K,
        subset_plot[[measure]],
        type = "l",
        xlab = "k",
        ylab = measure,
        main = paste(
          measure,
          "-",
          best_row$Method,
          best_row$Distance
        )
      )
    }
  }
  
  return(
    list(
      best_combination = best_row,
      summary_table = results
    )
  )
}


# ==============================================================================
# ITERATIVE FORECAST (rolling origin over consecutive days, with accuracy)
# ==============================================================================
# Forecasts `horizon` consecutive days one step ahead. Each forecast is based
# on all the observed data up to the previous day, so xmat_full must already
# contain the days being forecast (their true values are used both to extend
# the history at each step and to compute the errors).
#
# - xmat_full: matrix with the history AND the days to forecast
# - start_day: index of the LAST OBSERVED day before the first forecast day,
#              i.e. the first instance. The forecast days are
#              start_day + 1, ..., start_day + horizon.
#              Example: with 731 days in 2023-2024, start_day = 731 and
#              horizon = 31 forecast January 2025.
# - horizon:   number of days to forecast
#
# Value: list with
# - predictions:    horizon x dim.prof matrix of forecast profiles
# - accuracy:       data frame with the daily MAE, RMSE, SMAPE
# - absolute_error: horizon x dim.prof matrix of hourly absolute errors

iterative_forecast = function(xmat_full,
                              dim.prof = ncol(xmat_full),
                              k = 3,
                              dist = "euclidean",
                              method = "wknn",
                              start_day = 10,
                              horizon = 10) {
  
  # the true profile following the last instance must exist to compute errors
  if ((start_day - 1 + horizon) >= nrow(xmat_full)) {
    stop(
      "start_day + horizon - 1 must be lower than nrow(xmat_full): the days to forecast must be observed"
    )
  }
  
  preds = matrix(
    NA,
    nrow = horizon,
    ncol = dim.prof
  )
  
  absolute_error = matrix(
    NA,
    nrow = horizon,
    ncol = dim.prof
  )
  
  accuracy_table = data.frame(
    MAE = numeric(horizon),
    RMSE = numeric(horizon),
    SMAPE = numeric(horizon)
  )
  
  for (i in 1:horizon) {
    
    xlast = (start_day - 1) + i
    
    output = knn(
      xmat = xmat_full,
      dim.prof = dim.prof,
      n.prof = nrow(xmat_full),
      k = k,
      xlast = xlast,
      dist = dist,
      method = method,
      accuracy = TRUE
    )
    
    preds[i, ] = output$pred
    absolute_error[i, ] = output$errors$ABSOLUTE_ERROR
    
    accuracy_table$MAE[i] = output$errors$MAE
    accuracy_table$RMSE[i] = output$errors$RMSE
    accuracy_table$SMAPE[i] = output$errors$SMAPE
  }
  
  return(
    list(
      predictions = preds,
      accuracy = accuracy_table,
      absolute_error = absolute_error
    )
  )
}