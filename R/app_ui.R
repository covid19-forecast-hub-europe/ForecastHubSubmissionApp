#' @author Written by Johannes Bracher, johannes.bacher@@kit.edu
#' @import shiny

app_ui <- function(request) {
  fluidPage(

    # Application title
    titlePanel("Visualize your submission"),

    # Sidebar with control elements
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Choose file to upload", accept = ".csv"),
        textInput("path", "Or paste a URL to a csv file (the raw csv, not github preview)."),
        # uiOutput("inp_select_location") # currently not used
      ),

      # Main panel
      mainPanel(
        tags$style("#result_checks {font-size:15px;
                 font-family:'Courier New';
                 display:block; }"),
        h4("Preview of submission (may take a few seconds to load):"),
        textOutput(outputId = "file_name"), # paste name of visualized file
        # plotOutput("plot"), # plot
        uiOutput("plot_ui"),
        tags$div(
          tags$span(style = "color:white", ".")
        ),
        tags$div(
          tags$span(style = "color:white", ".")
        ),
        h6(HTML("Even if your files are displayed correctly here it is possible that they fail the format checks on the GitHub platform. The formal evaluation checks are not run on this site, it serves solely for visualization. Information on how to run
           validation checks locally can be found in the <a href='https://github.com/epiforecasts/covid19-forecast-hub-europe/wiki'>Wiki of our GitHub repository</a>.")),
        h4("A few things worth checking:"),
        h6(
          "The following are no requirements for submission and will not be checked after your pull request.",
          "This is just a non-exhaustive list of plausibility checks we have found useful in the past."
        ),
        tags$div(
          tags$ul(
            tags$li(
              tags$b("Vertical alignment:"),
              "pronounced and unexpected jumps between last observed data and predictions can indicate technical problems."
            ),
            tags$li(
              tags$b("Plausible pattern across time horizons:"),
              "if projections for successive time steps are not 'smooth' this may indicate problems (unless there is a plausible explanation)."
            ),
            tags$li(
              tags$b("Appropriate degree of dispersion:"),
              "Check whether you are confident that the 50% and 95% intervals will cover the observations in 50% and 95% of the cases, respectively."
            ),
            tags$li(
              tags$b("Dispersion increases with time horizon:"),
              "Uncertainty often (though not always) increases somewhat for larger horizons as events further in the future are typically harder to predict."
            ),
            tags$li(
              tags$b("Plausible skewness:"),
              "As count outcomes have a natural lower limit (zero), they often have a right-skewed distribution. This means that typically the median will",
              "be closer to the lower than the upper end of your forecast intervals."
            ),
            hr(),
            tags$footer(
              a("Source code. ", href = "https://github.com/epiforecasts/ForecastHubSubmissionApp")
            )
          )
        )
      )
    )
  )
}
