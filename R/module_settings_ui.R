#' Settings UI
#'
#' @param id XXX.
#' @seealso \link{module_settings_server}
#' @family module
#' @export
module_settings_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "Settings",
    icon = icon("gear"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          h4("Bookmarking"),
          bookmarkButton(),
          tags$p(textOutput(outputId = ns("last_saved"))),
          tags$br(),
          h4("Print options"),
          numericInput(
            inputId = ns("digits"),
            label = "Significant digits",
            value = 2, min = 1, max = 7, step = 1
          ),
          tags$br(),
          h4("Graphical output"),
          numericInput(
            inputId = ns("fig_width"),
            label = "Figure width",
            value = 7
          ),
          numericInput(
            inputId = ns("fig_height"),
            label = "Figure height",
            value = 5
          ),
          selectInput(
            inputId = ns("fig_units"),
            label = "Figure units",
            choices = c("in", "cm", "mm")
          ),
          selectInput(
            inputId = ns("fig_colour"),
            label = "Colour scale",
            selected = "default",
            choices = c("default", "bright", "contrast", "vibrant", "muted",
                        "pale", "dark", "light", "discrete rainbow")
          )
        ),
        column(
          width = 9,
          h4("Session information"),
          verbatimTextOutput(outputId = ns("session"))
        )
      )
    )
  )
}
