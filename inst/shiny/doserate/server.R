#' Shiny App Server Function
#'
#' @param input provided by Shiny.
#' @param output provided by Shiny.
#' @param session provided by Shiny.
#' @author N. Frerebeau
#' @keywords internal
#' @noRd
shiny_server <- function(input, output, session) {
  # Set reactive values ========================================================
  myData <- reactiveValues(spectra = NULL, names = NULL, raw = NULL)
  myRangesCalib <- reactiveValues(x = NULL, y = NULL, expand = TRUE)
  myRangesImport <- reactiveValues(x = NULL, y = NULL, expand = TRUE)
  # Import =====================================================================
  # Event ----------------------------------------------------------------------
  observeEvent(input$import_files, {
    file <- input$import_files
    # Return a GammaSpectra object
    spc_name <- tools::file_path_sans_ext(file$name)
    spc_data <- read(file$datapath)
    spc_data <- methods::as(spc_data, "GammaSpectra")
    set_names(spc_data) <- spc_name
    # Store data
    myData$spectra <- spc_data
    myData$names <- spc_name
    myData$raw <- spc_data
    # Update UI
    shinyWidgets::updatePickerInput(session, "import_select",
                                    choices = spc_name, selected = spc_name)
    updateSelectInput(session, "calib_select",
                      choices = spc_name, selected = spc_name[[1]])
    shinyWidgets::updatePickerInput(session, "dose_select",
                                    choices = spc_name, selected = spc_name)
  })
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$import_plot_dblclick, {
    brush <- input$import_plot_brush
    if (!is.null(brush)) {
      myRangesImport$x <- c(brush$xmin, brush$xmax)
      myRangesImport$y <- c(brush$ymin, brush$ymax)
      myRangesImport$expand <- FALSE
    } else {
      myRangesImport$x <- NULL
      myRangesImport$y <- NULL
      myRangesImport$expand <- TRUE
    }
  })
  # Reactive -------------------------------------------------------------------
  mySpectrum <- reactive({
    # req(myData$spectra)
    validate(
      need(!is.null(myData$spectra), "Please import one or more spectra."),
      need(!is.null(input$import_select) &&
             input$import_select != "" &&
             input$import_select %in% myData$names,
           "Please select at least one spectrum.")
    )
    list(
      summary = summarise(myData$spectra[input$import_select]),
      name = get_names(myData$spectra[input$import_select]),
      plot = plot(myData$spectra,
                  xaxis = input$import_xaxis,
                  yaxis = input$import_yaxis,
                  select = input$import_select,
                  facet = input$import_facet) +
        ggplot2::theme_bw()
    )
  })
  # Render ---------------------------------------------------------------------
  output$import_plot <- renderPlot({
    mySpectrum()$plot +
      ggplot2::coord_cartesian(xlim = myRangesImport$x,
                               ylim = myRangesImport$y,
                               expand = myRangesImport$expand)
  })
  output$import_summary <- renderText({
    tbl <- knitr::kable(
      x = mySpectrum()$summary,
      digits = input$options_digits,
      row.names = FALSE,
      col.names = c("Name", "Date", "Live time [s]", "Real time [s]", "Chanels",
                    "Min.", "Max.")
    )
    tbl <- kableExtra::kable_styling(
      kable_input = tbl,
      bootstrap_options = c("striped", "hover"),
      full_width = TRUE, fixed_thead = TRUE
    )
    kableExtra::add_header_above(
      kable_input = tbl,
      header = c(" " = 5, "Energy Range [keV]" = 2)
    )
  })
  output$import_export_plot <- downloadHandler(
    filename = function() {
      ifelse(
        length(mySpectrum()$name) == 1,
        paste0(mySpectrum()$name, ".pdf"),
        "spectra.pdf"
      )
    },
    content = function(file) {
      ggplot2::ggsave(file, plot = mySpectrum()$plot,
                      width = input$options_fig_width,
                      height = input$options_fig_height,
                      units = input$options_fig_units)
    },
    contentType = "application/pdf"
  )
  output$import_export_table <- downloadHandler(
    filename = "summary.csv",
    content = function(file) {
      utils::write.csv(mySpectrum()$summary, file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  # Energy calibration =========================================================
  # Reactive -------------------------------------------------------------------
  myPeaks <- reactive({
    # Validation
    req(myData$spectra, input$calib_select)
    validate(
      need(!is.null(input$calib_smooth_m) && input$calib_smooth_m != "",
           "The window size must be set (smoothing)."),
      need(input$calib_smooth_m %% 2 != 0,
           "The window size must be an odd integer (smoothing)."),
      need(!is.null(input$calib_smooth_p) && input$calib_smooth_p != "",
           "The polynomial degree must be set (smoothing)."),
      need(!is.null(input$calib_baseline_k) && input$calib_baseline_k != "",
           "The number of iteration must be set (baseline)."),
      need(!is.null(input$calib_peak_snr) && input$calib_peak_snr != "",
           "The signal-to-noise-ratio must be set (peak searching).")
    )
    # Get a GammaSpectrum object
    spc_raw <- myData$spectra[[input$calib_select]]
    spc_chanels <- get_chanels(spc_raw)
    # Drop chanels
    n <- input$calib_slice_range
    index <- seq(from = n[[1]], to = n[[2]], by = 1)
    spc_sliced <- slice_signal(spc_raw, index)
    # Transform intensities
    trans <- switch(input$calib_stabilize_method,
                    none = function(x) x,
                    sqrt = sqrt)
    spc_transformed <- stabilize_signal(spc_sliced, transformation = trans)
    # Smooth intensities
    spc_smooted <- smooth_signal(
      spc_transformed,
      method = input$calib_smooth_method,
      m = input$calib_smooth_m,
      p = input$calib_smooth_p
    )
    # Remove baseline
    spc_baseline <- remove_baseline(
      spc_smooted,
      method = input$calib_baseline_method,
      LLS = input$calib_baseline_lls,
      decreasing = input$calib_baseline_decreasing,
      k = input$calib_baseline_k
    )
    # Detect peaks
    spc_peaks <- find_peaks(
      spc_baseline,
      method = input$calib_peak_method,
      SNR = input$calib_peak_snr,
      span = input$calib_peak_span * spc_chanels / 100
    )
    # Plot
    gg_spectrum <- plot(spc_sliced, spc_peaks) + ggplot2::theme_bw()
    gg_baseline <- plot(spc_baseline, spc_peaks) +
      ggplot2::labs(title = get_names(spc_raw)) +
      ggplot2::theme_bw()

    list(
      spectrum = spc_raw,
      peaks = spc_peaks,
      chanels = spc_chanels,
      name = input$calib_select,
      data = methods::as(spc_raw, "data.frame"),
      plot_spectrum = gg_spectrum,
      plot_baseline = gg_baseline
    )
  })
  # Event ----------------------------------------------------------------------
  observeEvent(input$calib_select, {
    req(myData$spectra, input$calib_select)
    max_chanel <- get_chanels(myData$spectra[[input$calib_select]])
    updateSliderInput(session, "calib_slice_range",
                      max = max_chanel, value = c(60, max_chanel))
  })
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$calib_plot_dblclick, {
    brush <- input$calib_plot_brush
    if (!is.null(brush)) {
      myRangesCalib$x <- c(brush$xmin, brush$xmax)
      myRangesCalib$y <- c(brush$ymin, brush$ymax)
      myRangesCalib$expand <- FALSE
    } else {
      myRangesCalib$x <- NULL
      myRangesCalib$y <- NULL
      myRangesCalib$expand <- TRUE
    }
  })
  observeEvent(input$calib_action, {
    req(myPeaks())
    spc <- myPeaks()$spectrum
    chanel <- myPeaks()$peaks@chanel
    energy <- vapply(X = chanel, FUN = function(i) {
      input[[paste0("calib_peak_", i)]]
    }, FUN.VALUE = numeric(1))
    peaks <- methods::initialize(myPeaks()$peaks, energy = energy)
    # Calibrate energy scale
    spc_calib <- try(calibrate_energy(spc, peaks))
    # Update spectrum
    if (class(spc_calib) == "try-error") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Energy Calibration",
        text = spc_calib,
        type = "error"
      )
    } else {
      myData$spectra[[input$calib_select]] <- spc_calib
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Energy Calibration",
        text = "The enegy scale has been adjusted.",
        type = "success"
      )
    }
  })
  observeEvent(input$calib_reset, {
    myData$spectra[[input$calib_select]] <- myData$raw[[input$calib_select]]
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Restore Data",
      text = "The energy scale has been restored to its original values.",
      type = "info"
    )
  })
  # Render ---------------------------------------------------------------------
  output$calib_plot_peaks <- renderPlot({
    myPeaks()$plot_spectrum +
      ggplot2::coord_cartesian(xlim = myRangesCalib$x, ylim = myRangesCalib$y,
                               expand = myRangesCalib$expand)
  })
  output$calib_plot_baseline <- renderPlot({
    myPeaks()$plot_baseline
  })
  output$calib_ok <- renderUI({
    spc <- myData$spectra[[input$calib_select]]
    if (is_calibrated(spc)) {
      coef <- spc[["calibration"]]$coefficients
      if (length(coef) != 0) {
        tags$div(
          tags$span(icon("check-circle"), style = "color: #225522;"),
          sprintf("The energy scale of the spectrum %s has been adjusted.",
                  input$calib_select)
        )
      } else {
        tags$div(
          tags$span(icon("exclamation-triangle"), style = "color: #666633;"),
          sprintf("The spectrum %s has an energy scale, but has not been adjusted.",
                  input$calib_select)
        )
      }
    } else {
      tags$div(
        tags$span(icon("times-circle"), style = "color: #663333;"),
        sprintf("The spectrum %s does not have an energy scale.",
                input$calib_select)
      )
    }
  })
  output$calib_input_peaks <- renderUI({
    req(myPeaks())
    peaks <- methods::as(myPeaks()$peaks, "data.frame")
    peaks$energy <- rep(NA_real_, nrow(peaks))
    if (nrow(myPairs()) != 0) {
      req(input$options_energy_tolerance)
      tol <- input$options_energy_tolerance
      for (i in seq_len(nrow(myPairs()))) {
        b <- myPairs()$chanel[i]
        k <- which.min(abs(peaks$chanel - b))
        a <- peaks$chanel[k]
        if (a >= b - tol && a <= b + tol) {
          peaks$energy[k] <- myPairs()$energy[i]
        }
      }
      # peaks <- stats::na.omit(peaks)
    }
    lapply(
      X = seq_len(nrow(peaks)),
      FUN = function(i, peaks) {
        chanel <- peaks$chanel[[i]]
        energy <- peaks$energy[[i]]
        numericInput(inputId = paste0("calib_peak_", chanel),
                     label = paste0("Chanel ", chanel), value = energy)
      },
      peaks
    )
  })
  output$calib_export_table <- downloadHandler(
    filename = function() paste0(myPeaks()$name, ".csv"),
    content = function(file) {
      utils::write.csv(myPeaks()$data, file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  output$calib_export_plot <- downloadHandler(
    filename = function() paste0(myPeaks()$name, ".pdf"),
    content = function(file) {
      ggplot2::ggsave(file, plot = myPeaks()$plot_spectrum,
                      width = input$options_fig_width,
                      height = input$options_fig_height,
                      units = input$options_fig_units)
    },
    contentType = "application/pdf"
  )
  # Dose rate prediction =======================================================
  signal_value <- reactive({
    req(input$dose_threshold)
    paste0(input$dose_threshold, "_signal")
  })
  signal_error <- reactive({
    req(input$dose_threshold)
    paste0(input$dose_threshold, "_error")
  })
  doseCurve <- reactive({
    req(input$dose_curve)
    get(input$dose_curve, envir = env_calibration)
  })
  doseData <- reactive({
    req(doseCurve(), myData$spectra, input$dose_error, input$dose_threshold)
    withCallingHandlers(
      {
        predict_dose(doseCurve(), myData$spectra,
                     epsilon = input$dose_error / 100,
                     threshold = input$dose_threshold, simplify = TRUE)
      },
      warning = function(e) {
        warn <- gsub("\n|\\*", "", e$message)
        showNotification(warn, duration = 10, type = "warning")
        invokeRestart("muffleWarning")
      }
    )
  })
  # Render ---------------------------------------------------------------------
  output$dose_info <- renderUI({
    info <- doseCurve()[["details"]]
    tags$dl(
      lapply(
        X = seq_along(info),
        FUN = function(i, info) {
          tagList(tags$dt(names(info)[[i]]), tags$dd(as.character(info[[i]])))
        },
        info
      ),
      class = "gamma-details"
    )
  })
  output$dose_plot_curve <- renderPlot({
    req(input$dose_select)
    extra <- doseData()[input$dose_select, ]
    plot(doseCurve(), threshold = input$dose_threshold) +
      ggplot2::geom_pointrange(
        data = extra,
        mapping = ggplot2::aes(
          ymin = .data$gamma_dose - .data$gamma_error,
          ymax = .data$gamma_dose + .data$gamma_error
        )
      ) +
      ggplot2::geom_errorbarh(
        data = extra,
        mapping = ggplot2::aes(
          xmin = .data[[signal_value()]] - .data[[signal_error()]],
          xmax = .data[[signal_value()]] + .data[[signal_error()]]
        ),
        height = 0) +
      ggplot2::theme_bw()
  })
  output$dose_table_dose <- renderText({
    req(input$dose_select)
    extra <- nearPoints(
      doseData()[input$dose_select, ], input$dose_plot_hover,
      xvar = signal_value(), yvar = "gamma_dose",
      threshold = 10, maxpoints = 1, allRows = TRUE
    )
    tbl <- knitr::kable(
      extra[, -ncol(extra)], digits = input$options_digits,
      row.names = FALSE,
      col.names = c("Name", "Value", "Error", "Value", "Error")
    )
    tbl <- kableExtra::kable_styling(
      kable_input = tbl,
      bootstrap_options = c("striped", "hover"),
      full_width = TRUE, fixed_thead = TRUE
    )
    tbl <- kableExtra::add_header_above(
      kable_input = tbl,
      header = c(" " = 1, "Signal" = 2, "Dose Rate [\u03BCGy/y]" = 2)
    )
    kableExtra::row_spec(tbl, row = which(extra[[ncol(extra)]]),
                         bold = TRUE, background = "#CCDDAA")
  })
  output$dose_export <- downloadHandler(
    filename = "dose_rate.csv",
    content = function(file) {
      req(input$dose_select)
      utils::write.csv(doseData()[input$dose_select, ], file,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  # Settings ===================================================================
  # Reactive -------------------------------------------------------------------
  # mySettings <- reactive({})
  myPairs <- reactive({
    try(read.table(
      header = FALSE, sep = " ", dec = ".",
      strip.white = TRUE, blank.lines.skip = TRUE,
      col.names = c("chanel", "energy"),
      colClasses = c("integer", "numeric"),
      text = input$options_energy_pairs
    ), silent = TRUE)
  })
  # Render ---------------------------------------------------------------------
  output$options_table_pairs <- renderTable({
    if (class(myPairs()) != "try-error")
        myPairs()
  })
  output$options_session <- renderPrint({
    sessionInfo()
  })
  # Help =======================================================================
  # https://stackoverflow.com/questions/49210495/show-documentation-pages-in-shiny-app
}
