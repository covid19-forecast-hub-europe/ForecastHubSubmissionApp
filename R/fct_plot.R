#' Read in week-ahead forecasts from a file
#'
#' @inheritParams utils::read.csv
#'
#' @importFrom utils read.csv
read_week_ahead <- function(file) {
  dat <- read.csv(file, colClasses = c(location = "character", forecast_date = "Date", target_end_date = "Date"), stringsAsFactors = FALSE)
  return(
    dat[dat$target %in% c(
      paste(1:4, "wk ahead inc death"), paste(1:4, "wk ahead cum death"),
      paste(1:4, "wk ahead inc case"), paste(1:4, "wk ahead cum case")
    ), ]
  )
}

#' Get the subset of a forecast file needed for plotting
#'
#' @inheritParams plot_forecast
#' @param type type of forecast. Either `quantile` or `point`.
#'
subset_forecasts_for_plot <- function(forecasts, forecast_date = NULL, target_type, horizon, location, type = NULL) {
  check_target <- if (is.null(horizon)) {
    grepl(target_type, forecasts$target)
  } else {
    grepl(horizon, forecasts$target) & grepl(target_type, forecasts$target)
  }
  check_forecast_date <- if (is.null(forecast_date)) TRUE else forecasts$forecast_date == forecast_date

  forecasts <- forecasts[check_target &
    check_forecast_date &
    forecasts$location == location &
    (forecasts$quantile %in% c(0.025, 0.25, 0.5, 0.75, 0.975) |
      forecasts$type == "point"), ]
  if (!is.null(type)) forecasts <- forecasts[forecasts$type == type, ]
  return(forecasts)
}

#' Helper function to determine y-limit
#'
#' @inheritParams plot_forecast
#'
determine_ylim <- function(forecasts, forecast_date = NULL, target_type, horizon, location, truth, start_at_zero = TRUE) {
  truth <- subset(truth, date >= forecast_date - 28)
  lower <- if (start_at_zero) {
    0
  } else {
    0.95 * min(c(forecasts$value, truth[, target_type]))
  }
  truth <- truth[truth$location == location, ]
  ylim <- c(lower, 1.05 * max(c(forecasts$value, truth[, target_type]), na.rm = TRUE))
}

#' Create an empty plot to which forecasts can be added
#'
#' @inheritParams graphics::plot
#'
#' @importFrom graphics plot axis title box
empty_plot <- function(xlim, ylim, xlab, ylab) {
  plot(NULL,
    xlim = xlim, ylim = ylim,
    xlab = xlab, ylab = "", axes = FALSE
  )
  axis(2, las = 1)
  title(ylab = ylab, line = 4)
  all_dates <- seq(from = as.Date("2020-02-01"), to = Sys.Date() + 28, by = 1)
  saturdays <- all_dates[weekdays(all_dates) == "Saturday"]
  axis(1, at = saturdays, labels = as.Date(saturdays, origin = "1970-01-01"))
  box()
}

#' Add a single prediction interval
#'
#' @inheritParams plot_forecast
#' @param col color of the current band
#' @param coverage CI of the current band
#'
#' @importFrom graphics polygon
draw_prediction_band <- function(forecasts, forecast_date = NULL, target_type, horizon,
                                 location, coverage, col = "lightgrey") {
  if (!coverage %in% c(1:9 / 10, 0.95, 0.98)) stop("Coverage needs to be from 0.1, 0.2, ..., 0.9, 0.95, 0.98")

  # select points to draw polygon:
  lower <- forecasts[abs(forecasts$quantile - (1 - coverage) / 2) < 0.01, ]
  lower <- lower[order(lower$target_end_date), ]
  upper <- forecasts[abs(forecasts$quantile - (1 - (1 - coverage) / 2)) < 0.01, ]
  upper <- upper[order(upper$target_end_date, decreasing = TRUE), ]
  # draw:
  polygon(
    x = c(lower$target_end_date, upper$target_end_date),
    y = c(lower$value, upper$value), col = col, border = NA
  )
}

#' Draw many prediction intervals (resulting in a fanplot)
#'
#' @inheritParams plot_forecast
#' @param cols a vector of colors of the same length as levels_coverage
#'
draw_fanplot <- function(forecasts, target_type, forecast_date, horizon, location, levels_coverage = c(1:9 / 10, 0.95, 0.98),
                         cols = colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1]) {
  for (i in rev(seq_along(levels_coverage))) {
    draw_prediction_band(
      forecasts = forecasts,
      target_type = target_type,
      horizon = horizon,
      forecast_date = forecast_date,
      location = location,
      coverage = levels_coverage[i],
      col = cols[i]
    )
  }
}

#' Add points for point forecasts
#'
#' @inheritParams plot_forecast
#' @param col color of the point forecasts
#'
#' @importFrom graphics lines points
draw_points <- function(forecasts, target_type, horizon, forecast_date, location, col = "deepskyblue4") {
  forecasts <- subset_forecasts_for_plot(
    forecasts = forecasts, forecast_date = forecast_date,
    target_type = target_type, horizon = horizon, location = location,
    type = "point"
  )
  lines(forecasts$target_end_date, forecasts$value, col = col)
  points(forecasts$target_end_date, forecasts$value, pch = 21, col = col, bg = "white")
}

#' Add smaller points for truths
#'
#' @inheritParams plot_forecast
#'
draw_truths <- function(truth, location, target_type) {
  truth <- truth[weekdays(truth$date) == "Saturday" &
    truth$location == location, ]
  points(truth$date, truth[, target_type], pch = 20, type = "b")
}

#' Wrap it all up into one plotting function:

#' @param forecasts a data.frame containing forecasts from one model in the
#' standard long format needs to contain forecasts from different
#' `forecast_dates` to plot forecats by "horizon"
#' @param target_type `"inc death"` or `"cum death"`
#' @param horizon `"1 wk ahead"`, `"2 wk ahead"`, `"3 wk ahead"` or
#' `"4 wk ahead"`; if specified forecasts at this horizon are plotted for
#' different forecast dates. Has to be NULL if forecast_date is specified
#' @param forecast_date the date at which forecasts were issued; if specified,
#' 1 though 4 wk ahead forecasts are shown. Has to be NULL if horizon is
#' specified
#' @param location the location for which to plot forecasts
#' @param truth the truth data set
#' @param levels_coverage which intervals are to be shown? Defaults to all.
#' `c(0.5, 0.95)` is a reasonable parsimonious choice.
#' @param start,end beginning and end of the time period to plot
#' @param cols_intervals a vector of colors of the same length as levels_coverage
#' @param col_point color of the point forecasts
#' @param start_at_zero logical (defaults to `TRUE`): should the y-axis include
#' 0.
#'
#' @inheritParams graphics::plot
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics abline
plot_forecast <- function(forecasts,
                          target_type = "cum death",
                          horizon = NULL,
                          forecast_date = NULL,
                          location,
                          truth,
                          levels_coverage = c(1:9 / 10, 0.95, 0.98),
                          start = as.Date("2020-04-01"),
                          end = Sys.Date() + 28,
                          ylim = NULL,
                          cols_intervals = colorRampPalette(c("deepskyblue4", "lightgrey"))(length(levels_coverage) + 1)[-1],
                          col_point = "deepskyblue4",
                          xlab = "date",
                          ylab = target_type,
                          start_at_zero = TRUE) {
  if (is.null(horizon) & is.null(forecast_date)) stop("Exactly one out of horizon and forecast_date needs to be specified")

  forecasts <- forecasts[forecasts$target_end_date >= start & forecasts$target_end_date <= end, ]
  truth <- truth[truth$date >= start & truth$location == location, ]
  xlim <- c(start, end)

  forecasts <- subset_forecasts_for_plot(
    forecasts = forecasts, forecast_date = forecast_date,
    target_type = target_type, horizon = horizon, location = location,
    type = "quantile"
  )

  if (nrow(forecasts) == 0) {
    plot(NULL, xlim = 0:1, ylim = 0:1, xlab = "", ylab = "", axes = FALSE)
    text(0.5, 0.5, paste("No forecasts found."))
    return()
  }

  if (is.null(ylim)) {
    ylim <- determine_ylim(
      forecasts = forecasts, forecast_date = forecast_date,
      target_type = target_type, horizon = horizon,
      location = location, truth = truth, start_at_zero = start_at_zero
    )
  }

  empty_plot(xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab)
  if (!is.null(forecast_date)) abline(v = forecast_date, lty = "dashed")
  draw_fanplot(
    forecasts = forecasts, target_type = target_type,
    horizon = horizon, forecast_date = forecast_date,
    location = location, levels_coverage = levels_coverage,
    cols = cols_intervals
  )
  draw_points(
    forecasts = forecasts, target_type = target_type,
    horizon = horizon, forecast_date = forecast_date,
    location = location, col = col_point
  )
  draw_truths(truth = truth, location = location, target_type = target_type)
}
