#' Read in week-ahead forecasts from a file
#'
#' @inheritParams utils::read.csv
#'
#' @importFrom utils read.csv
read_week_ahead <- function(file) {
  dat <- read.csv(file, colClasses = c(location = "character", forecast_date = "Date", target_end_date = "Date"), stringsAsFactors = FALSE)
  return(
    dat[grepl("^[1234] wk ahead", dat$target), ]
  )
}
