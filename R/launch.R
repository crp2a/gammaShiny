# SHINY

#' Launch the Shiny App
#'
#' A wrapper for \code{\link[shiny]{shinyAppDir}}.
#' @param app A \code{\link{character}} string specifying the Shiny application
#'  to launch. It must be one of \code{"doserate"} or \code{"calibration"}
#'  (see details).
#' @param browser A \code{\link{logical}} scalar: should the app be launched in
#'  the browser?
#' @details
#'  \tabular{ll}{
#'   **Application name** \tab  **Keyword** \cr
#'   Dose rate estimation \tab \code{doserate} \cr
#'   Calibration curve builder \tab \code{calibration}
#'  }
#' @examples
#' \dontrun{
#' launch_app("doserate")
#' launch_app("calibration")
#' }
#' @return A \pkg{shiny} application object.
#' @family shiny
#' @author N. Frerebeau
#' @export
launch_app <- function(app = c("doserate", "calibration"), browser = TRUE) {
  app <- match.arg(app, several.ok = FALSE)
  if (app == "calibration")
    stop("Work in progress.", call. = FALSE)
  shiny::shinyAppDir(
    appDir = system.file(app, package = "gammaShiny"),
    options = list(launch.browser = browser)
  )
}
