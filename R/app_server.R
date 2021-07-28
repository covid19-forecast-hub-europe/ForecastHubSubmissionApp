#' @author Written by Johannes Bracher, johannes.bacher@@kit.edu
#' @import shiny
#' @importFrom graphics legend par text

# unix command to change language (for local testing)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# command that should work cross-platform
# Sys.setlocale(category = "LC_TIME","English")

local <- FALSE # set to FALSE when deploying, TRUE when testing locally

# get truth data:
if (local) {
  dat_truth <- read.csv("../../viz/truth_to_plot.csv",
                        colClasses = list("date" = "Date"), stringsAsFactors = FALSE
  )
} else {
  dat_truth <- read.csv("https://raw.githubusercontent.com/epiforecasts/covid19-forecast-hub-europe/main/viz/truth_to_plot.csv",
                        colClasses = list("date" = "Date"), stringsAsFactors = FALSE
  )
}
# adapt column names for matching with targets
colnames(dat_truth) <- gsub("inc_", "inc ", colnames(dat_truth))

# define colors
cols_legend <- c("#699DAF", "#D3D3D3")

app_server <- function(input, output, session) {

  model <- reactiveVal("")
  forecasts <- reactiveVal(NULL)
  locations <- reactiveVal(NULL)

  # if URL is provided as a URL param
  observeEvent(session$clientData, {
    query <- parseQueryString(session$clientData$url_search) # arguments provided in URL
    if (length(query) != 0) {
      model(basename(query$file))
      forecasts(try(read_week_ahead(query$file)))
    }
  })

  # if data file is uploaded via the file explorer:
  observeEvent(input$file, {
    path <- input$file$datapath
    model(basename(input$file$name))
    forecasts(try(read_week_ahead(path))) # wrapped in try() to avoid crash if no valid csv
  }, ignoreInit = TRUE)

  # if URL to csv provided in input field:
  observeEvent(input$path, {
    path <- input$path
    model(basename(input$path))
    forecasts(try(read_week_ahead(path))) # wrapped in try() to avoid crash if no valid csv
  }, ignoreInit = TRUE)

  observeEvent(forecasts(), {
    locations(unique(forecasts()$location))
    if (!is.null(forecasts()$location_name)) {
      names(locations()) <- unique(forecasts()$location_name)
    }
  })

  # output element to display file name:
  output$file_name <- renderText(model())

  # plot output:
  output$plot <- renderPlot({
    if (!is.null(forecasts())) {

      # get forecast date:
      forecast_date <- forecasts()$forecast_date[1]

      par(mfrow = c(length(locations()), 2), cex = 1)

      for (loc in locations()) {
        # plot for cases:
        if (any(grepl("case", forecasts()$target))) { # only if case forecasts available
          plot_forecast(forecasts(),
                        forecast_date = forecast_date,
                        location = loc,
                        truth = dat_truth, target_type = "inc case",
                        levels_coverage = c(0.5, 0.95),
                        start = as.Date(forecast_date) - 35,
                        end = as.Date(forecast_date) + 28
          )
          title(paste0("Incident cases - ", loc))
          legend("topleft", legend = c("50%PI", "95% PI"), col = cols_legend, pch = 15, bty = "n")
        } else { # otherwise empty plot
          plot(NULL, xlim = 0:1, ylim = 0:1, xlab = "", ylab = "", axes = FALSE)
          text(0.5, 0.5, paste("No case forecasts found."))
        }

        # plot for deaths:
        if (any(grepl("death", forecasts()$target))) { # only if case forecasts available
          plot_forecast(forecasts(),
                        forecast_date = forecast_date,
                        location = loc,
                        truth = dat_truth, target_type = "inc death",
                        levels_coverage = c(0.5, 0.95),
                        start = as.Date(forecast_date) - 37,
                        end = as.Date(forecast_date) + 28
          )
          title(paste0("Incident deaths - ", loc))
          legend("topleft", legend = c("50%PI", "95% PI"), col = cols_legend, pch = 15, bty = "n")
        } else { # otherwise empty plot
          plot(NULL, xlim = 0:1, ylim = 0:1, xlab = "", ylab = "", axes = FALSE)
          text(0.5, 0.5, paste("No  death forecasts found."))
        }
      }
    } else {
      # if no file is uploaded: empty plot with "Please select a valid csv file"
      plot(NULL, xlim = 0:1, ylim = 0:1, xlab = "", ylab = "", axes = FALSE)
      text(0.5, 0.5, "Please select a valid csv file.")
    }
  })

  output$plot_ui <- renderUI({
    plotOutput("plot", height = ifelse(is.null(locations()), 500, length(locations()) * 250))
  })
}
