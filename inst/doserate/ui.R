#' Shiny App User Interface Object
#'
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_ui <- fluidPage(
  includeCSS("www/style.css"),
  navbarPage(
    "gamma",
    tabPanel(
      "1. Import",
      icon = icon("upload"),
      fluidPage(
        fluidRow(
          column(
            width = 4,
            wellPanel(
              fileInput(
                "import_files", "Import spectrum file(s)",
                multiple = TRUE,
                accept = c('.cnf', '.CNF', '.tka', '.TKA')
              ),
              tags$hr(),
              shinyWidgets::pickerInput(
                inputId = "import_select",
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
              checkboxInput("import_facet", "Display in a grid", value = FALSE),
              radioButtons("import_xaxis", "X axis", choices = c("chanel", "energy")),
              radioButtons("import_yaxis", "Y axis", choices = c("count", "rate"))
            )
          ),
          column(
            width = 8,
            column(
              width = 12,
              downloadButton("import_export_plot", "Export plot"),
              downloadButton("import_export_table", "Export summary")
            ),
            column(
              width = 12,
              style = "margin-top: 25px;",
              plotOutput(
                "import_plot",
                dblclick = "import_plot_dblclick",
                brush = brushOpts(id = "import_plot_brush", resetOnNew = TRUE)
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            htmlOutput("import_summary")
          )
        )
      )
    ),
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
                h5("2. Stabilize signal"),
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
    ),
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
          column(
            width = 1,
            style = "margin-top: 25px;",
            icon("chevron-right", "fa-2x")
          ),
          column(
            width = 3,
            selectInput(
              "dose_curve",
              label = "Select a calibration curve",
              selected = 1,
              choices = list(
                Choose = "",
                IRAMAT = c(BDX_LaBr_1 = "BDX_LaBr_1_curve",
                           BDX_LaBr_2 = "BDX_LaBr_2_curve",
                           BDX_NaI = "BDX_NaI_curve"),
                CEREGE = c(AIX_NaI = "AIX_NaI_curve")
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
              radioButtons(
                "dose_threshold",
                label = "Threshold",
                choices = c(`Count (Ni)` = "Ni", `Energy (NiEi)` = "NiEi")
              ),
              numericInput(
                "dose_error",
                label = "Energy calibration error (%)",
                min = 0, max = 100, value = 3, step = 1
              )
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
    ), # End: tabPanel
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
                rows = 8, placeholder = "76 238.63\n465 1460.82\n830 2614.51"
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
    ),
    tabPanel(
      "About",
      icon = icon("info-circle"),
      fluidRow(
        column(
          width = 8,
          align = "center",
          offset = 2,
          wellPanel(
            img(src = "/figures/logo.png", width = "120px", alt = "gamma"),
            h4(paste("gamma", utils::packageVersion("gamma"), sep = " ")),
            tags$br(),
            tags$p("Source code:",
                   tags$a(href = "https://github.com/crp2a/gamma",
                          "https://github.com/crp2a/gamma")),
            tags$p(
              "This program is free software: you can redistribute it and/or
              modify it under the terms of the GNU General Public License as
              published by the Free Software Foundation, either version 3 of
              the License, or (at your option) any later version."
            ),
            tags$p(
              "This program is distributed in the hope that it will be useful,
              but WITHOUT ANY WARRANTY; without even the implied warranty of
              MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
              GNU General Public License for more details."
            ),
            tags$br(),
            tags$p(format(utils::citation("gamma"), bibtex = FALSE)[[1]]),
            tags$p(format(utils::citation("gamma"), bibtex = FALSE)[[2]]),
            tags$br(),
            tags$p(
              "This work received a state financial support
              managed by the Agence Nationale de la Recherche (France)
              throught the program Investissements d'avenir (ref. ",
              tags$a(href = "https://lascarbx.labex.u-bordeaux.fr/",
                     "10-LABX-0052", .noWS = c("before", "after")),
              " and ",
              tags$a(href = "https://amidex.univ-amu.fr/",
                     "11-IDEX-0001", .noWS = c("before", "after")),
              ")."
            )
          )
        )
      )
    )
  ) # End: navbarPage
)
