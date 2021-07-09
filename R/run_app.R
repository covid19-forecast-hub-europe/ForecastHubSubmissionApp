#' Run shiny app
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#'
#' @export
run_app <- function(...) {
  shinyApp(
    ui = app_ui,
    server = app_server
  )
}
