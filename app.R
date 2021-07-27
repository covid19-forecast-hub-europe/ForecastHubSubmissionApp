# Launch the ShinyApp (Do not remove this comment as it is used by RStudio to
# auto-detect this is a shiny app launcher.)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
ForecastHubSubmissionApp::run_app() # add parameters here (if any)
