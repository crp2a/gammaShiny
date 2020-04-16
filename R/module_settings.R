#' Settings
#'
#' @param id TODO.
#' @family module
#' @export
mod_ui_settings <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
  "Settings",
  icon = icon("gear"),
  fluidPage(
    fluidRow(
      column(
        width = 6,
        h5("Energy presets"),
        helpText(
          "You can define channel-energy pairs (in keV) to pre-fill",
          "the values to be used for the energy scale calibration.",
          "Values must be separated by a blank space,",
          "each pair must be on its own line.",
          "A tolerance (in chanel) must be set to provide the limits",
          "between which we can expect to find the specified chanels."
        ),
        column(
          width = 6,
          textAreaInput(
            "options_energy_pairs",
            "Chanel-energy pairs",
            value = "", width = NULL,
            rows = 8, placeholder = "76 238"
          ),
          numericInput("options_energy_tolerance", "Tolerance (in chanel)",
                       value = 5, min = 1, max = 50, step = 1)
        ),
        column(
          width = 6,
          tableOutput("options_table_pairs")
        )
      ),
      column(
        width = 3,
        h5("Print options"),
        numericInput("options_digits", "Significant digits",
                     value = 2, min = 1, max = 7, step = 1)
      ),
      column(
        width = 3,
        h5("Graphical output"),
        numericInput("options_fig_width", "Figure width", value = 7),
        numericInput("options_fig_height", "Figure height", value = 5),
        selectInput("options_fig_units", "Figure units",
                    choices = c("in", "cm", "mm"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        h5("Session information"),
        verbatimTextOutput("options_session")
      )
    )
  )
)
}
