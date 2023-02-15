#' Read in week-ahead forecasts from a file
#'
#' @inheritParams utils::read.csv
#'
#' @importFrom utils read.csv
read_week_ahead <- function(file) {
  dat <- read.csv(file, colClasses = c(location = "character", forecast_date = "Date", target_end_date = "Date"), stringsAsFactors = FALSE)
  return(
    dat[grepl("^-?[01234] wk ahead", dat$target), ]
  )
}

#' @importFrom dplyr filter mutate rename select full_join
point_to_median <- function(df) {

  point_forecasts <- df |>
    filter(type == "point") |>
    mutate(type = "quantile", quantile = 0.5) |>
    rename(point_prediction = prediction)
  fc_df <- df |>
    filter(type == "quantile") |>
    full_join(point_forecasts) |>
    mutate(prediction =
             ifelse(is.na(prediction), point_prediction, prediction)) |>
    select(-point_prediction)

  return(fc_df)
}
