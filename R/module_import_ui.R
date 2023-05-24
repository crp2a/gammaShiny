#' Import UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_import_server()]
#' @family modules
#' @export
module_import_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    title = "1. Import",
    icon = icon("upload"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            fileInput(
              inputId = ns("files"),
              label = "Import spectrum file(s)",
              multiple = TRUE,
              accept = c(".cnf", ".CNF", ".tka", ".TKA")
            ),
            tags$hr(),
            shinyWidgets::pickerInput(
              inputId = ns("select"),
              label = "Select",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              )
            ),
            checkboxInput(
              inputId = ns("log_scale"),
              label = "Log scale",
              value = TRUE
            ),
            radioButtons(
              inputId = ns("xaxis"),
              label = "X axis",
              choices = c("channel", "energy")
            ),
            radioButtons(
              inputId = ns("yaxis"),
              label = "Y axis",
              choices = c("count", "rate")
            )
          )
        ),
        column(
          width = 8,
          column(
            width = 12,
            downloadButton(outputId = ns("export_plot"), "Export plot"),
            downloadButton(outputId = ns("export_table"), "Export summary")
          ),
          column(
            width = 12,
            style = "margin-top: 25px;",
            plotly::plotlyOutput(outputId = ns("plot"))
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          tableOutput(outputId = ns("summary"))
        )
      )
    )
  )
}
