library(dplyr)
library(forecast)
library(lubridate)

#' Generate input data frame with rolling forecasts for visualization
#' 
#' For each date in the data frame, take the data up to that date as training
#' data, train a model and forecast. Join the forecasts back against the
#' training and test data to make a data frame that can easily be plotted.
#' 
#' @param df A data frame with `date` and `value` columns
#' 
#' @return A data frame with rolling forecast results that can be plotted
#' 
make_rolling_forecasts <- function(df) {
  ls_forecasts <- vector(mode = "list", length = NROW(df))
  for (train_end_index in seq_along(df$date)) {
    train_end <- df$date[train_end_index]
    
    df_train <- df[df$date <= train_end, ]
    df_out <- dplyr::bind_rows(
      df_train,
      head(df[df$date > train_end, ], 12)
    )
    
    ls_forecasts[[train_end_index]] <- forecast::auto.arima(
        y = ts(df_train$value, frequency = 12)
      ) |>
      forecast::forecast(
        h = 12,
        level = c(6, 9, 11) / 12,
        bootstrap = TRUE
      ) |>
      reshape_forecasts(train_end = train_end) |>
      reverse_full_join(df_out, by = "date") |>
      dplyr::mutate(
        index = train_end_index,
        split_date = train_end - months(train_end_index) + lubridate::days(15),
        date_fixed = date - months(train_end_index)
      )
  }
  df_forecasts <- dplyr::bind_rows(ls_forecasts)
  
  return(df_forecasts)
}

#' Reshape the `forecast` results into a data frame amenable to `ggplot2`
reshape_forecasts <- function(forecasts, train_end) {
  data.frame(
    date = seq(
      train_end + months(1),
      by = "month",
      length.out = length(forecasts$mean)
    ),
    lb_11_12 = as.numeric(forecasts$lower[,3]),
    lb_09_12 = as.numeric(forecasts$lower[,2]),
    lb_06_12 = as.numeric(forecasts$lower[,1]),
    point = forecasts$mean,
    ub_06_12 = as.numeric(forecasts$upper[,1]),
    ub_09_12 = as.numeric(forecasts$upper[,2]),
    ub_11_12 = as.numeric(forecasts$upper[,3])
  )
}

reverse_full_join <- function(x, y, by) {
  dplyr::full_join(x = y, y = x, by = by)
}
