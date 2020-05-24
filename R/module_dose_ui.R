#' Dose Rate UI
#'
#' @param id XXX.
#' @seealso \link{module_dose_server}
#' @family module
#' @export
module_dose_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "3. Dose Rate Estimation",
    icon = icon("hourglass-half"),
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
            inputId = ns("curve"),
            label = "Select a calibration curve",
            selected = 1,
            choices = list(
              Choose = "",
              IRAMAT = c(BDX_LaBr_1 = "BDX_LaBr_1"),
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
          uiOutput(outputId = ns("info")),
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
                min = 0, max = 100, value = 0, step = 1
              )
            )
          )
        ),
        column(
          width = 8,
          htmlOutput(outputId = ns("results"))
        )
      )
    )
  )
}
