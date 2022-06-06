#' @author Written by Johannes Bracher, johannes.bacher@@kit.edu
#' @import shiny
#' @importFrom graphics legend par text
#' @import ggplot2

# unix command to change language (for local testing)
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF8")

# command that should work cross-platform
# Sys.setlocale(category = "LC_TIME","English")

truth <- covidHubUtils::load_truth(
  truth_source = "JHU",
  temporal_resolution = "weekly",
  hub = "ECDC"
)
truth$scenario_id <- "truth"

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
      origin_date <- forecasts()$origin_date[1]

      fcasts <- tidyr::complete(
        forecasts(),
        .data$location,
        .data$target_variable,
        .data$origin_date,
        fill = list(prediction = -1e6)
      )

      truth |>
        dplyr::filter(
          target_variable %in% fcasts$target_variable,
          location %in% fcasts$location,
          target_end_date > origin_date - 35
        ) |>
        dplyr::full_join(fcasts) |>
        ggplot(aes(x = target_end_date, y = value, group = paste0(scenario_id, sample), color = scenario_id)) +
        geom_line(alpha = 0.5) +
        facet_wrap(
          vars(location, target_variable),
          scales = "free_y",
          ncol = dplyr::n_distinct(fcasts$target_variable)
        ) +
        theme_minimal() +
        scale_color_brewer(palette = "Set1") +
        scale_y_continuous(labels = scales::comma) +
        expand_limits(y = 0) +
        # Make sure negative values for cases/deaths are not displayed
        coord_cartesian(ylim = c(0, NA)) +
        xlab("Week") +
        theme(legend.position = "top")

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
