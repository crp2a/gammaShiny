module_energy <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "2. Energy Calibration",
    icon = icon("bolt"),
    fluidPage(
      tabsetPanel(
        tabPanel(
          "Energy Scale Adjustment",
          icon = icon("arrows-alt-h"),
          fluidRow(
            style = "margin-top: 25px;",
            column(
              width = 4,
              wellPanel(
                helpText(
                  "Set the energy values (keV) corresponding to at least 3",
                  "of the channels below, then click on", dQuote("calibrate.")
                ),
                uiOutput("calib_input_peaks"),
                actionButton("calib_action", "Calibrate"),
                actionButton("calib_reset", "Restore")
              )
            ),
            column(
              width = 8,
              column(
                width = 4,
                selectInput("calib_select", "Select a spectrum",
                            choices = NULL, selected = NULL,
                            multiple = FALSE)
              ),
              column(
                width = 8,
                style = "margin-top: 25px;",
                downloadButton("calib_export_table", "Export data"),
                downloadButton("calib_export_plot", "Export plot")
              ),
              column(
                width = 12,
                htmlOutput("calib_ok")
              ),
              column(
                width = 12,
                style = "margin-top: 25px;",
                plotOutput(
                  "calib_plot_peaks",
                  dblclick = "calib_plot_dblclick",
                  brush = brushOpts(id = "calib_plot_brush", resetOnNew = TRUE)
                )
              )
            )
          )
        ),
        tabPanel(
          "Peak Detection",
          icon = icon("chart-bar"),
          fluidRow(
            style = "margin-top: 25px;",
            plotOutput("calib_plot_baseline")
          ),
          fluidRow(
            column(
              width = 3,
              h5("1. Drop chanels"),
              sliderInput("calib_slice_range", "Chanels to keep",
                          min = 1, max = 2048, value = c(1, 2048), step = 5
              ),
              h5("2. Transform signal"),
              selectInput("calib_stabilize_method", "Method", selected = 1,
                          choices = list(none = "none", `square root` = "sqrt"))
            ),
            column(
              width = 3,
              h5("3. Smooth signal"),
              selectInput("calib_smooth_method", "Method", selected = 1,
                          choices = list("savitzky", "unweighted", "weighted")),
              numericInput("calib_smooth_m", "Window size",
                           value = 21, min = 3, max = 50, step = 2),
              numericInput("calib_smooth_p", "Polynomial degree",
                           min = 1, max = 6, value = 2, step = 1)
            ),
            column(
              width = 3,
              h5("4. Remove baseline"),
              selectInput("calib_baseline_method", "Method", selected = 1,
                          choices = list("SNIP")),
              checkboxInput("calib_baseline_lls", "LLS", value = FALSE),
              checkboxInput("calib_baseline_decreasing", "Decreasing", value = FALSE),
              numericInput("calib_baseline_k", "Iterations",
                           value = 100, min = 10, max = 500, step = 10)
            ),
            column(
              width = 3,
              h5("5. Detect peaks"),
              selectInput("calib_peak_method", "Method", selected = 1,
                          choices = list("MAD")),
              numericInput("calib_peak_snr", "Signal-to-noise-ratio",
                           value = 2, min = 1, max = 5, step = 1),
              sliderInput("calib_peak_span", "Half window size",
                          min = 0, max = 100, value = 5, step = 1)
            )
          )
        )
      )
    )
  )
}
