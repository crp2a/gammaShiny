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
  user_lines <- reactiveVal(data.frame())
  user_spectrum <- reactive({
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
      need(!is.null(input$baseline_snip_n) && input$baseline_snip_n != "",
           "The number of iteration must be set (baseline)."),
      need(!is.null(input$baseline_rubber_noise) &&
             input$baseline_rubber_noise != "",
           "The noise must be set (baseline)."),
      need(!is.null(input$peak_snr) && input$peak_snr != "",
           "The signal-to-noise-ratio must be set (peak search).")
    )

    # Get a GammaSpectrum object
    spc_raw <- user_spectrum()

    # Drop chanels
    n <- input$slice_range
    index <- seq(from = n[[1]], to = n[[2]], by = 1)
    spc <- signal_slice(spc_raw, index)

    # Transform intensities
    fun <- switch(
      input$stabilize_method,
      none = function(x) x,
      sqrt = sqrt
    )
    spc <- signal_stabilize(spc, f = fun)

    # Smooth intensities
    spc <- signal_smooth(spc, method = input$smooth_method,
                         m = input$smooth_m, p = input$smooth_p)

    # Estimate and remove baseline
    bsl <- baseline(
      spc,
      method = input$baseline_method,
      LLS = input$baseline_snip_lls,
      decreasing = input$baseline_snip_decreasing,
      n = input$baseline_snip_n,
      noise = input$baseline_rubber_noise,
      spline = input$baseline_rubber_spline
    )
    spc <- spc - bsl

    # Detect peaks
    pks <- peaks_find(spc, method = input$peak_method, SNR = input$peak_snr,
                      span = input$peak_span * get_chanels(spc) / 100)

    lines <- as.data.frame(pks)
    lines$energy <- NA_real_
    user_lines(lines)

    list(
      spectrum = spc_raw,
      baseline = bsl,
      lines = spc,
      peaks = pks,
      name = input$select
    )
  })
  plot_spectrum <- reactive({
    plot(user_peaks()$spectrum, user_peaks()$peaks) +
      ggplot2::labs(title = user_peaks()$name) +
      ggplot2::theme_bw()
  })
  plot_baseline <- reactive({
    bsl <- user_peaks()$baseline
    set_names(bsl) <- "Baseline"
    plot(user_peaks()$spectrum, bsl) +
      ggplot2::labs(title = user_peaks()$name) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "none")
  })
  plot_peaks <- reactive({
    plot(user_peaks()$lines, user_peaks()$peaks) +
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
  observeEvent({
    user_data$spectra
    input$select
    input$presets_lines
    }, {
      presets <- try(
      utils::read.table(
        header = FALSE, sep = " ", dec = ".",
        strip.white = TRUE, blank.lines.skip = TRUE,
        col.names = c("chanel", "energy"),
        colClasses = c("integer", "numeric"),
        text = input$presets_lines
      ),
      silent = TRUE
    )
    if (!inherits(presets, "try-error")) {
      tmp <- user_lines()
      if (nrow(tmp) > 0 && nrow(presets) == 0) {
        tmp$energy <- NA_real_
      } else if (nrow(tmp) > 0 &&nrow(presets) > 0) {
        req(input$presets_tolerance)
        tol <- input$presets_tolerance
        for (i in seq_len(nrow(presets))) {
          b <- presets$chanel[i]
          k <- which.min(abs(tmp$chanel - b))
          a <- tmp$chanel[k]
          if (a >= b - tol && a <= b + tol) {
            tmp$energy[k] <- presets$energy[i]
          }
        }
      }
      user_lines(tmp)
    }
  })
  observeEvent(input$input_lines_cell_edit, {
    tmp <- user_lines()
    tmp$energy <- as.numeric(input$input_lines_cell_edit$value)
    user_lines(tmp)
  })
  observeEvent(input$action, {
    spc <- user_peaks()$spectrum
    pks <- user_lines()
    pks <- stats::na.omit(pks)

    # Calibrate energy scale
    spc_calib <- try(energy_calibrate(spc, pks))
    # Update spectrum
    if (inherits(spc_calib, "try-error")) {
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
  output$plot_spectrum <- plotly::renderPlotly({
    plotly::ggplotly(plot_spectrum())
  })
  output$plot_baseline <- plotly::renderPlotly({
    plotly::ggplotly(plot_baseline())
  })
  output$plot_peaks <- plotly::renderPlotly({
    plotly::ggplotly(plot_peaks())
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
  output$input_lines <- DT::renderDT({
    DT::datatable(
      data = user_lines(),
      options = list("searching" = FALSE,
                     "paging" = FALSE),
      editable = list(target = "column",
                      disable = list(columns = 1))
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
      fig <- if (input$show_baseline) plot_baseline() else plot_peaks()
      ggplot2::ggsave(file, plot = fig,
                      width = user_settings$fig_width,
                      height = user_settings$fig_height,
                      units = user_settings$fig_units)
    },
    contentType = "application/pdf"
  )
}
