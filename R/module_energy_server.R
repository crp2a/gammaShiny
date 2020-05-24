#' Energy Calibration Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a \code{\link[shiny]{reactiveValues}} list with the
#'  following elements: "\code{spectra}", "\code{names}" and "\code{raw}".
#' @param user_settings a \code{\link[shiny]{reactiveValues}} list.
#' @seealso \link{module_energy_ui}
#' @family module
#' @export
module_energy_server <- function(input, output, session,
                                 user_data, user_settings) {
  user_spectrum <- reactive({
    # Validation
    req(user_data$spectra, input$select)
    user_data$spectra[[input$select]]
  })
  user_peaks <- reactive({
    validate(
      need(!is.null(input$smooth_m) && input$smooth_m != "",
           "The window size must be set (smoothing)."),
      need(input$smooth_m %% 2 != 0,
           "The window size must be an odd integer (smoothing)."),
      need(!is.null(input$smooth_p) && input$smooth_p != "",
           "The polynomial degree must be set (smoothing)."),
      need(!is.null(input$baseline_k) && input$baseline_k != "",
           "The number of iteration must be set (baseline)."),
      need(!is.null(input$peak_snr) && input$peak_snr != "",
           "The signal-to-noise-ratio must be set (peak search).")
    )

    # Get a GammaSpectrum object
    spc_raw <- user_spectrum()

    # Drop chanels
    n <- input$slice_range
    index <- seq(from = n[[1]], to = n[[2]], by = 1)
    spc <- slice_signal(spc_raw, index)

    # Transform intensities
    fun <- switch(
      input$stabilize_method,
      none = function(x) x,
      sqrt = sqrt
    )
    spc <- stabilize_signal(spc, transformation = fun)

    # Smooth intensities
    spc <- smooth_signal(spc, method = input$smooth_method,
                         m = input$smooth_m, p = input$smooth_p)

    # Remove baseline
    spc <- remove_baseline(spc, method = input$baseline_method,
                           LLS = input$baseline_lls,
                           decreasing = input$baseline_decreasing,
                           k = input$baseline_k)

    # Detect peaks
    pks <- find_peaks(spc, method = input$peak_method, SNR = input$peak_snr,
                      span = input$peak_span * get_chanels(spc) / 100)

    list(
      spectrum = spc_raw,
      baseline = spc,
      peaks = pks,
      name = input$select,
      data = as.data.frame(spc_raw)
    )
  })
  user_lines <- reactive({
    try(
      utils::read.table(
        header = FALSE, sep = " ", dec = ".",
        strip.white = TRUE, blank.lines.skip = TRUE,
        col.names = c("chanel", "energy"),
        colClasses = c("integer", "numeric"),
        text = input$lines
      ),
      silent = TRUE
    )
  })
  plot_spectrum <- reactive({
    plot(user_peaks()$spectrum, user_peaks()$peaks) +
      ggplot2::labs(title = user_peaks()$name) +
      ggplot2::theme_bw()
  })
  plot_baseline <- reactive({
    plot(user_peaks()$baseline, user_peaks()$peaks) +
      ggplot2::labs(title = user_peaks()$name) +
      ggplot2::theme_bw()
  })
  observeEvent(user_data$spectra, {
    req(user_data$spectra)
    # Update UI
    updateSelectInput(
      session,
      inputId = "select",
      choices = user_data$names,
      selected = user_data$names[[1L]]
    )
  })
  observeEvent(input$select, {
    # Update UI
    req(input$select)
    spc <- user_spectrum()
    max_chanel <- max(get_chanels(spc))
    updateSliderInput(session, inputId = "slice_range",
                      max = max_chanel, value = c(60, max_chanel))
  })
  observeEvent(input$action, {
    spc <- user_peaks()$spectrum
    pks <- user_peaks()$peaks
    chanel <- get_chanels(pks)
    energy <- vapply(
      X = chanel,
      FUN = function(i) input[[paste0("peak_", i)]],
      FUN.VALUE = numeric(1)
    )
    set_energy(pks) <- energy

    # Calibrate energy scale
    spc_calib <- try(calibrate_energy(spc, pks))
    # Update spectrum
    if (class(spc_calib) == "try-error") {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Energy Calibration",
        text = spc_calib,
        type = "error"
      )
    } else {
      user_data$spectra[[input$select]] <- spc_calib
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Energy Calibration",
        text = "The enegy scale has been adjusted.",
        type = "success"
      )
    }
  })
  observeEvent(input$reset, {
    user_data$spectra[[input$select]] <- user_data$raw[[input$select]]
    shinyWidgets::sendSweetAlert(
      session = session,
      title = "Restore Data",
      text = "The energy scale has been restored to its original values.",
      type = "info"
    )
  })
  # Render
  output$plot_peaks <- plotly::renderPlotly({
    plotly::ggplotly(plot_spectrum())
  })
  output$plot_baseline <- plotly::renderPlotly({
    plotly::ggplotly(plot_baseline())
  })
  output$calibration <- renderUI({
    spc <- user_spectrum()
    if (has_energy(spc)) {
      if (!is.null(spc[["calibration"]])) {
        tags$div(
          tags$span(icon("check-circle"), style = "color: green;"),
          sprintf("The energy scale of the spectrum %s has been adjusted.",
                  input$select)
        )
      } else {
        tags$div(
          tags$span(icon("exclamation-triangle"), style = "color: orange;"),
          sprintf("The spectrum %s has an energy scale, but has not been adjusted.",
                  input$select)
        )
      }
    } else {
      tags$div(
        tags$span(icon("times-circle"), style = "color: red;"),
        sprintf("The spectrum %s does not have an energy scale.",
                input$select)
      )
    }
  })
  output$input_peaks <- renderUI({
    peaks <- as.data.frame(user_peaks()$peaks)
    peaks$energy <- rep(NA_real_, nrow(peaks))

    lines <- user_lines()
    if (nrow(lines) != 0) {
      req(input$lines_tolerance)
      tol <- input$lines_tolerance
      for (i in seq_len(nrow(lines))) {
        b <- lines$chanel[i]
        k <- which.min(abs(peaks$chanel - b))
        a <- peaks$chanel[k]
        if (a >= b - tol && a <= b + tol) {
          peaks$energy[k] <- lines$energy[i]
        }
      }
    }
    lapply(
      X = seq_len(nrow(peaks)),
      FUN = function(i, peaks) {
        ns <- session$ns
        chanel <- peaks$chanel[[i]]
        energy <- peaks$energy[[i]]
        numericInput(
          inputId = ns(paste0("peak_", chanel)),
          label = paste0("Chanel ", chanel),
          value = energy
        )
      },
      peaks
    )
  })
  output$export_data <- downloadHandler(
    filename = "spectra.rds",
    content = function(file) {
      saveRDS(user_data$spectra, file)
    },
    contentType = "NA"
  )
  output$export_lines <- downloadHandler(
    filename = function() paste0(user_peaks()$name, ".csv"),
    content = function(file) {
      data <- as.data.frame(user_peaks()$peaks)
      utils::write.csv(data, file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
  output$export_plot <- downloadHandler(
    filename = function() paste0(user_peaks()$name, ".pdf"),
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_spectrum(),
                      width = user_settings$fig_width,
                      height = user_settings$fig_height,
                      units = user_settings$fig_units)
    },
    contentType = "application/pdf"
  )
  output$export_baseline <- downloadHandler(
    filename = function() paste0(user_peaks()$name, ".pdf"),
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_baseline(),
                      width = user_settings$fig_width,
                      height = user_settings$fig_height,
                      units = user_settings$fig_units)
    },
    contentType = "application/pdf"
  )
  output$lines_table <- renderTable({
    if (class(user_lines()) != "try-error") user_lines()
  })
}
