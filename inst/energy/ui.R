#' Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- fluidPage(
  navbarPage(
    "Energy",
    module_import_ui("import"),
    module_energy_ui("energy"),
    module_settings_ui("settings"),
    module_about_ui("about")
  )
)
