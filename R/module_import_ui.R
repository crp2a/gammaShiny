#' Import UI
#'
#' @param id XXX.
#' @seealso \link{module_import_server}
#' @family module
#' @export
module_import_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "1. Import",
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
              accept = c('.cnf', '.CNF', '.tka', '.TKA')
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
              inputId = ns("facet"),
              label = "Display in a grid",
              value = FALSE
            ),
            radioButtons(
              inputId = ns("xaxis"),
              label = "X axis",
              choices = c("chanel", "energy")
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
          htmlOutput(outputId = ns("summary"))
        )
      )
    )
  )
}