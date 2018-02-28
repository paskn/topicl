# Run the shiny app
shine_tm <- function() {
  appDir <- system.file("topicl_app", package = "topicl")
  if (appDir == "") {
    stop("Could not find the shiny app. Try re-installing `topicl`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
