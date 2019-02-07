THEME.UI <- function() {
  appDir <- system.file("myapp", package = "THEME")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal",launch.browser=TRUE)
}
