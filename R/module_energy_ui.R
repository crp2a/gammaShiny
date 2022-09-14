#' Energy Calibration UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @seealso [module_energy_server()]
#' @family modules
#' @export
module_energy_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "2. Energy Calibration",
    icon = icon("bolt"),
    fluidPage(
      tabsetPanel(
        tabPanel(
          "Energy Scale Adjustment",
          icon = icon("arrows-left-right"),
          fluidRow(
            style = "margin-top: 25px;",
            column(
              width = 4,
              wellPanel(
                helpText(
                  tags$p(
                    "The following channels were detected as peaks candidates",
                    "(you can adjust the detection parameters",
                    "in the corresponding tab)."
                  ),
                  tags$p(
                    "To adjust the energy scale of your spectrum,",
                    "select the three lines corresponding to 212Pb, 40K",
                    "and 208Tl in the table below, then click on",
                    dQuote("calibrate.")
                  ),
                  tags$p(
                    "These three channels will be assigned the following",
                    "energies: 238 keV, 1461 keV and 2615 keV."
                  )
                ),
                actionButton(inputId = ns("action"), "Calibrate"),
                actionButton(inputId = ns("reset"), "Restore"),
                tags$hr(),
                dataTableOutput(outputId = ns("input_lines"))
              )
            ),
            column(
              width = 8,
              column(
                width = 4,
                selectInput(
                  inputId = ns("select"),
                  label = "Select a spectrum",
                  choices = NULL,
                  selected = NULL,
                  multiple = FALSE
                )
              ),
              column(
                width = 6,
                style = "margin-top: 25px;",
                downloadButton(outputId = ns("export_data"), "Export spectra"),
                downloadButton(outputId = ns("export_plot"), "Export plot")
              ),
              column(
                width = 12,
                htmlOutput(outputId = ns("calibration"))
              ),
              column(
                width = 12,
                style = "margin-top: 25px;",
                plotly::plotlyOutput(outputId = ns("plot_spectrum")),
                checkboxInput(
                  inputId = ns("log_scale"),
                  label = "Log scale",
                  value = TRUE
                )
              )
            )
          )
        ), # End tabPanel
        tabPanel(
          "Peak Detection",
          icon = icon("chart-bar"),
          fluidRow(
            style = "margin-top: 25px;float: right;",
            downloadButton(
              outputId = ns("export_lines"),
              label = "Export lines"
            ),
            downloadButton(
              outputId = ns("export_baseline"),
              label = "Export plot"
            ),
            checkboxInput(inputId = ns("show_baseline"),
                          label = "Show baseline", value = FALSE)
          ),
          fluidRow(
            style = "clear: both;",
            conditionalPanel(
              ns = ns,
              condition = "input.show_baseline",
              plotly::plotlyOutput(outputId = ns("plot_baseline"))
            ),
            conditionalPanel(
              ns = ns,
              condition = "!input.show_baseline",
              plotly::plotlyOutput(outputId = ns("plot_peaks"))
            )
          ),
          fluidRow(
            column(
              width = 3,
              h4("1. Drop channels"),
              sliderInput(
                inputId = ns("slice_range"), label = "Channels to keep",
                min = 1, max = 2048, value = c(1, 2048), step = 5
              ),
              h4("2. Stabilize signal"),
              selectInput(
                inputId = ns("stabilize_method"),
                label = "Method",
                selected = "sqrt",
                choices = list(none = "none", `square root` = "sqrt")
              )
            ),
            column(
              width = 3,
              h4("3. Smooth signal"),
              selectInput(
                inputId = ns("smooth_method"),
                label = "Method",
                selected = 1,
                choices = list("savitzky", "unweighted", "weighted")
              ),
              numericInput(
                inputId = ns("smooth_m"),
                label = "Window size",
                value = 21, min = 3, max = 50, step = 2
              ),
              numericInput(
                inputId = ns("smooth_p"),
                label = "Polynomial degree",
                min = 1, max = 6, value = 2, step = 1
              )
            ),
            column(
              width = 3,
              h4("4. Remove baseline"),
              selectInput(
                inputId = ns("baseline_method"),
                label = "Method",
                selected = 1,
                choices = list("SNIP", "rubberband")
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.baseline_method == 'SNIP'",
                checkboxInput(
                  inputId = ns("baseline_snip_lls"),
                  label = "LLS",
                  value = FALSE
                ),
                checkboxInput(
                  inputId = ns("baseline_snip_decreasing"),
                  label = "Decreasing",
                  value = FALSE
                ),
                numericInput(
                  inputId = ns("baseline_snip_n"),
                  label = "Iterations",
                  value = 100, min = 10, max = 500, step = 10
                )
              ),
              conditionalPanel(
                ns = ns,
                condition = "input.baseline_method == 'rubberband'",
                numericInput(
                  inputId = ns("baseline_rubber_noise"),
                  label = "Noise",
                  value = 0, min = 0, max = 100, step = 1
                ),
                checkboxInput(
                  inputId = ns("baseline_rubber_spline"),
                  label = "Spline interpolation",
                  value = TRUE
                )
              )
            ),
            column(
              width = 3,
              h4("5. Detect peaks"),
              selectInput(
                inputId = ns("peak_method"),
                label = "Method",
                selected = 1,
                choices = list("MAD")
              ),
              numericInput(
                inputId = ns("peak_snr"),
                label = "Signal-to-noise-ratio",
                value = 2, min = 1, max = 5, step = 1
              ),
              sliderInput(
                inputId = ns("peak_span"),
                label = "Half window size",
                min = 0, max = 100, value = 5, step = 1
              )
            )
          )
        ) # End tabPanel
      ) # End tabsetPanel
    )
  )
}
