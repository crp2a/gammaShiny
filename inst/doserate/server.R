#' Dose Rate Shiny App Server Function
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  # Set reactive values
  user_data <- reactiveValues()
  user_settings <- reactiveValues()
  callModule(module_import_server, "import", user_data, user_settings)
  callModule(module_energy_server, "energy", user_data, user_settings)
  callModule(module_dose_server, "dose", user_data, user_settings)
  callModule(module_settings_server, "settings", user_settings)
  session$onSessionEnded(stopApp)
}
