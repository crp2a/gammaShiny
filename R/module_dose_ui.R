#' Dose Rate UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_dose_server()]
#' @family modules
#' @export
module_dose_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "3. Dose Rate Estimation",
    icon = icon("hourglass"),
    fluidPage(
      fluidRow(
        column(
          width = 3,
          shinyWidgets::pickerInput(
            inputId = ns("select"),
            label = "Select spectra",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              size = 10,
              `selected-text-format` = "count > 3"
            )
          )
        ),
        column(
          width = 1,
          style = "margin-top: 25px;",
          icon("chevron-right", "fa-2x")
        ),
        column(
          width = 3,
          selectInput(
            inputId = ns("select_curve"),
            label = "Select a calibration curve",
            selected = NULL,
            choices = list(
              Choose = "",
              CRP2A = c(BDX_LaBr = "BDX_LaBr_1"),
              CEREGE = c(AIX_NaI = "AIX_NaI_1")
            )
          )
        ),
        column(
          width = 1,
          style = "margin-top: 25px;",
          icon("chevron-right", "fa-2x")
        ),
        column(
          width = 3,
          style = "margin-top: 25px;",
          downloadButton(outputId = ns("export_table"), "Export results")
        )
      ),
      fluidRow(
        column(
          width = 4,
          conditionalPanel(
            ns = ns,
            condition = "input.curve != ''",
            wellPanel(
              numericInput(
                inputId = ns("sigma"),
                label = "Sigma",
                min = 1, max = 7, value = 2, step = 1
              ),
              numericInput(
                inputId = ns("epsilon"),
                label = "Energy calibration error (%)",
                min = 0, max = 100, value = 3, step = 1
              )
            )
          ),
          plotOutput(outputId = ns("curve")),
          uiOutput(outputId = ns("info")),
          fileInput(
            inputId = ns("files"),
            label = "Import a calibration curve",
            multiple = FALSE,
            accept = c(".rds", ".RDS")
          )
        ),
        column(
          width = 8,
          htmlOutput(outputId = ns("energy"))
        ),
        column(
          width = 8,
          htmlOutput(outputId = ns("results"))
        )
      )
    )
  )
}
