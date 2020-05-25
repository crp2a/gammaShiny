# SHINY

#' Run a Shiny App
#'
#' A wrapper for \code{\link[shiny]{shinyAppDir}}.
#' @param app A \code{\link{character}} string specifying the Shiny application
#'  to run. It must be one of "\code{doserate}" or "\code{calibration}"
#'  (see details). Any unambiguous substring can be given.
#' @param browser A \code{\link{logical}} scalar: should the app be run in
#'  the browser?
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
                    browser = TRUE) {
  app <- match.arg(app, several.ok = FALSE)
  shiny::shinyAppDir(
    appDir = system.file(app, package = "gammaShiny"),
    options = list(launch.browser = browser)
  )
}
