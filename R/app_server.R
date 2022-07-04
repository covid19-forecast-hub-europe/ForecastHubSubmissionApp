#' @author Written by Johannes Bracher, johannes.bacher@@kit.edu
#' @import shiny
#' @importFrom graphics legend par text
#' @importFrom ggplot2 scale_y_continuous coord_cartesian expand_limits xlab .data aes scale_fill_viridis_d theme facet_wrap

# unix command to change language (for local testing)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# command that should work cross-platform
# Sys.setlocale(category = "LC_TIME","English")

truth <- covidHubUtils::load_truth(
  truth_source = "JHU",
  temporal_resolution = "weekly",
  hub = "ECDC"
)
truth$true_value <- truth$value
truth$model <- NULL
truth <- truth[, colnames(truth) != "value"]

# adapt column names for matching with targets
colnames(truth) <- gsub("inc_", "inc ", colnames(truth))

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
      locations(stats::setNames(locations(), unique(forecasts()$location_name)))
    }
  })

  # output element to display file name:
  output$file_name <- renderText(model())

  # plot output:
  output$plot <- renderPlot({
    if (!is.null(forecasts())) {

      # get forecast date:
      f_date <- forecasts()$forecast_date[1]

      fcasts <- forecasts() |>
        dplyr::mutate(
          prediction = value,
          target_variable = gsub("^\\d+ \\w+ \\w+ (\\w+ \\w+)$", "\\1", target),
          .keep = "unused"
        )

      fcasts <- tidyr::complete(
        fcasts,
        .data$location,
        .data$target_variable,
        .data$forecast_date,
        tidyr::nesting(type, quantile),
        fill = list(prediction = -1e6)
      )

      truth <- truth[truth$target_variable %in% fcasts$target_variable, ]
      truth <- truth[truth$location %in% fcasts$location, ]

      dat <- scoringutils::merge_pred_and_obs(
        fcasts,
        truth,
        "full"
      )

      p <- dat |>
        dplyr::filter(target_end_date > f_date - 35) |>
        scoringutils::plot_predictions(
          x = "target_end_date",
          range = c(0, 50, 95),
          by = c("location", "target_variable")
        ) +
          facet_wrap(
            location ~ target_variable,
            ncol = length(unique(dat$target_variable)),
            scales = "free_y"
          ) +
          scale_y_continuous(labels = scales::comma) +
          expand_limits(y = 0) +
          # Make sure negative values for cases/deaths are not displayed
          coord_cartesian(ylim = c(0, NA)) +
          xlab("Week") +
          theme(legend.position = "top")

      if (hasName(dat, "scenario_id")) {
        p +
          aes(fill = scenario_id) +
          scale_fill_viridis_d(alpha = 0.5)
      } else {
        p
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
