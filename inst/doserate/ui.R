#' Dose Rate Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- function(request) {
  fluidPage(
    navbarPage(
      "Dose Rate",
      module_import_ui("import"),
      module_energy_ui("energy"),
      module_dose_ui("dose"),
      module_settings_ui("settings"),
      module_about_ui("about")
    )
  )
}
