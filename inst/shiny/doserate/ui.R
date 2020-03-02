#' Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- fluidPage(
  includeCSS("www/style.css"),
  navbarPage(
    "gamma",
    module_import_spectra("dose_import_spectra"),
    module_energy("dose_energy_calibration"),
    tabPanel(
      "3. Dose Rate Estimation",
      icon = icon("hourglass-half"),
      fluidPage(
        fluidRow(
          column(
            width = 3,
            shinyWidgets::pickerInput(
              inputId = "dose_select",
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
          column(1, style = "margin-top: 25px;", icon("chevron-right", "fa-2x")),
          column(
            width = 3,
            selectInput("dose_curve", "Select a calibration curve", selected = 1,
                        choices = list(
                          Choose = "",
                          IRAMAT = c(BDX_LaBr_1 = "BDX_LaBr_1_curve",
                                     BDX_LaBr_2 = "BDX_LaBr_2_curve",
                                     BDX_NaI = "BDX_NaI_curve"),
                          CEREGE = c(AIX_NaI = "AIX_NaI_curve")
                        )
            )
          ),
          column(1, style = "margin-top: 25px;", icon("chevron-right", "fa-2x")),
          column(
            width = 3,
            style = "margin-top: 25px;",
            downloadButton("dose_export", "Export results")
          )
        ),
        fluidRow(
          column(
            width = 4,
            uiOutput("dose_info"),
            conditionalPanel(
              tags$hr(),
              condition = "input.dose_curve != ''",
              radioButtons("dose_threshold", "Threshold",
                           choices = c(`Count (Ni)` = "Ni",
                                       `Energy (Ni.Ei)` = "NiEi")),
              numericInput("dose_error", "Energy calibration error (%)",
                           min = 0, max = 100, value = 3, step = 1)
            )
          ),
          column(
            width = 8,
            plotOutput("dose_plot_curve", hover = "dose_plot_hover")
          )
        ),
        fluidRow(
          column(
            width = 12,
            htmlOutput("dose_table_dose")
          )
        )
      )
    ),
    module_settings("dose_settings"),
    module_about("dose_about")
  )
)
