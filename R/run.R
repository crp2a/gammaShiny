# SHINY

#' Run a Shiny App
#'
#' A wrapper for \code{\link[shiny]{shinyAppDir}}.
#' @param app A \code{\link{character}} string specifying the Shiny application
#'  to run. It must be one of "\code{doserate}" or "\code{calibration}"
#'  (see details). Any unambiguous substring can be given.
#' @param browser A \code{\link{logical}} scalar: should the app be run in
#'  the browser?
#' @param display A \code{\link{character}} string specifying the mode in which
#'  to display the application (see \code{\link[shiny]{runApp}}).
#' @details
#'  \tabular{ll}{
#'   **Application name** \tab  **Keyword** \cr
#'   Dose rate estimation \tab \code{doserate} \cr
#'   Calibration curve builder \tab \code{calibration} \cr
#'  }
#' @examples
#' \dontrun{
#' run_app("doserate")
#' run_app("calibration")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny
#' @author N. Frerebeau
#' @export
run_app <- function(app = c("doserate", "calibration"),
                    browser = TRUE, display = "auto") {
  app <- match.arg(app, several.ok = FALSE)
  if (app == "calibration") stop("Work in progress!", call. = FALSE)
  appDir <- system.file(app, package = "gammaShiny")
  if (appDir == "")
    stop(sprintf("Could not find %s app. ", sQuote(app)),
         "Try re-installing 'gammaShiny'.", call. = FALSE)
  shiny::shinyAppDir(
    appDir = appDir,
    options = list(launch.browser = browser, display.mode = display)
  )
}
