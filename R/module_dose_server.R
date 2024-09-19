#' Dose Rate Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a [shiny::reactiveValues()] list with the
#'  following elements: "`code`", "`names`" and "`raw`".
#' @param user_settings a [shiny::reactiveValues()] list.
#' @seealso [module_dose_ui()]
#' @family modules
#' @keywords internal
#' @export
module_dose_server <- function(input, output, session,
                               user_data, user_settings) {
  # Set an environment to store the calibration curve
  env_calibration <- new.env()
  utils::data("BDX_LaBr_1", package = "gamma", envir = env_calibration)
  utils::data("AIX_NaI_1", package = "gamma", envir = env_calibration)

  user_files <- reactive({
    req(input$files)
    input$files
  })

  user_spectra <- reactive({
    req(user_data$spectra, input$select)
    user_data$spectra[input$select]
  })

  user_curve <- reactive({
    req(input$select_curve)
    get(input$select_curve, envir = env_calibration)
  })

  user_dose <- reactive({
    req(input$sigma, input$epsilon, user_spectra(), user_curve())
    withCallingHandlers(
      {
        dose_predict(
          object = user_curve(),
          spectrum = user_spectra(),
          sigma = input$sigma,
          epsilon = input$epsilon / 100
        )
      },
      warning = function(e) {
        warn <- gsub("\n|\\*", "", e$message)
        showNotification(warn, duration = 10, type = "warning")
        invokeRestart("muffleWarning")
      }
    )
  })

  plot_curve <- reactive({
    req(user_curve())
    gamma::plot(user_curve(), error_ellipse = TRUE, error_bar = FALSE,
                energy = FALSE, level = 0.95, n = 50) +
      ggplot2::labs(title = "Calibration curve") +
      ggplot2::theme_bw()
  })

  observe({
    cv_name <- tools::file_path_sans_ext(user_files()$name)
    cv_data <- readRDS(user_files()$datapath)
    if (!methods::is(cv_data, "CalibrationCurve")) {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Calibration Curve",
        text = "Your file is not a valid calibration curve.",
        type = "error"
      )
    } else {
      # Assign to environment
      assign(cv_name, cv_data, env_calibration)
      # Update UI
      updateSelectInput(
        session,
        inputId = "select_curve",
        choices = ls(envir = env_calibration),
        selected = cv_name
      )
    }
  })

  observeEvent(user_data$spectra, {
    req(user_data$spectra)
    # Update UI
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select",
      choices = user_data$names,
      selected = user_data$names
    )
  })

  # Render
  output$curve <- renderPlot({
    plot_curve()
  })

  output$info <- renderUI({
    info <- user_curve()[["details"]]
    tags$dl(
      lapply(
        X = seq_along(info),
        FUN = function(i, info) {
          tagList(tags$dt(names(info)[[i]]), tags$dd(as.character(info[[i]])))
        },
        info
      ),
      style = "margin-top: 0.5em"
    )
  })

  output$energy <- renderUI({
    req(user_spectra())
    if (!all(has_calibration(user_spectra()))) {
      tags$h4(
        tags$span(icon("triangle-exclamation"), style = "color: orange;"),
        "The energy scale of one or more spectra must be adjusted.",
        "Check your data!"
      )
    }
  })

  output$integration <- gt::render_gt({
    req(user_spectra())
    req(user_dose())
    energy_calib <- has_calibration(user_spectra())

    tbl <- user_dose()
    col <- which(startsWith(colnames(tbl), "signal"))

    tbl <- tbl[, c(1, col)]
    tbl |>
      gt::gt(
        rowname_col = "name"
      ) |>
      gt::tab_spanner(
        label = "Integration (Ni)",
        columns = gt::ends_with("Ni")
      ) |>
      gt::tab_spanner(
        label = "Integration (NiEi)",
        columns = gt::ends_with("NiEi")
      ) |>
      gt::cols_label(
        gt::starts_with("signal_Ni") ~ "Value",
        gt::starts_with("signal_err") ~ "Error"
      ) |>
      gt::fmt_number(
        decimals = user_settings$digits
      ) |>
      gt::data_color(
        rows = which(!energy_calib),
        palette = "orange"
      ) |>
      gt::tab_options(table.width = "100%")
  })

  output$results <- gt::render_gt({
    req(user_spectra())
    req(user_dose())
    energy_calib <- has_calibration(user_spectra())

    tbl <- user_dose()
    col <- which(startsWith(colnames(tbl), "dose"))

    tbl <- tbl[, c(1, col)]
    tbl |>
      gt::gt(
        rowname_col = "name"
      ) |>
      gt::tab_spanner(
        label = "Dose rate (Ni)",
        columns = gt::ends_with("Ni")
      ) |>
      gt::tab_spanner(
        label = "Dose rate (NiEi)",
        columns = gt::ends_with("NiEi")
      ) |>
      gt::tab_spanner(
        label = "Mean dose rate",
        columns = gt::ends_with("final")
      ) |>
      gt::cols_label(
        gt::starts_with(c("dose_Ni", "dose_final")) ~ "Value",
        gt::starts_with(c("dose_err", "dose_err_final")) ~ "Error"
      ) |>
      gt::fmt_number(
        decimals = user_settings$digits
      ) |>
      gt::data_color(
        rows = which(!energy_calib),
        palette = "orange"
      ) |>
      gt::tab_options(table.width = "100%")
  })

  output$export_table <- downloadHandler(
    filename = "dose_rate.csv",
    content = function(file) {
      utils::write.csv(user_dose(), file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
}
