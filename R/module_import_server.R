#' Import Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a [shiny::reactiveValues()] list with the
#'  following elements: "`code`", "`names`" and "`raw`".
#' @param user_settings a [shiny::reactiveValues()] list.
#' @seealso [module_import_ui()]
#' @family modules
#' @export
module_import_server <- function(input, output, session,
                                 user_data, user_settings) {
  user_spectra <- reactive({
    req(user_data$spectra, input$select)
    user_data$spectra[input$select]
  })

  user_table <- reactive({
    req(user_spectra())
    summarise(user_spectra())
  })

  plot_spectra <- reactive({
    validate(
      need(user_data$spectra, "Please import one or more spectra."),
      need(input$select, "Please select one or more spectra.")
    )

    gg_log <- if (input$log_scale) ggplot2::scale_y_log10() else NULL

    gamma::plot(user_spectra(), xaxis = input$xaxis, yaxis = input$yaxis,
                select = input$select) +
      ggplot2::theme_bw() +
      gg_log +
      user_settings$fig_scale
  })

  observeEvent(input$files, {
    req(input$files)

    # Get previous data
    old <- user_data

    # Return a GammaSpectra object
    spc_name <- tools::file_path_sans_ext(input$files$name)
    spc_data <- gamma::read(input$files$datapath)
    set_names(spc_data) <- spc_name

    # Keep existing data
    new <- list(
      spc_data = methods::as(c(old$spectra, spc_data), "GammaSpectra"),
      spc_names = c(old$names, spc_name),
      spc_raw = methods::as(c(old$raw, spc_data), "GammaSpectra")
    )

    # Store data
    user_data$spectra <- new$spc_data
    user_data$names <- new$spc_name
    user_data$raw <- new$spc_raw

    # Update UI
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select",
      choices = new$spc_name,
      selected = new$spc_name
    )
  })

  # Render
  output$plot <- plotly::renderPlotly({
    plotly::ggplotly(plot_spectra())
  })

  output$summary <- gt::render_gt({
    tbl <- user_table()
    id <- tbl$name
    pal <- unclass(user_settings$fig_colour(length(id)))

    tbl |>
      gt::gt() |>
      gt::tab_spanner(
        label = "Energy Range",
        columns = gt::starts_with("energy")
      ) |>
      gt::cols_label(
        name = "Name", date = "Date", live_time = "Live time",
        real_time = "Real time", channels = "Channels",
        energy_min = "Min.", energy_max = "Max."
      ) |>
      gt::fmt_number(
        decimals = user_settings$digits
      ) |>
      gt::data_color(
        columns = 1,
        method = "factor",
        palette = pal
      ) |>
      gt::tab_options(table.width = "100%")
  })

  output$export_plot <- downloadHandler(
    filename = "spectra.pdf",
    content = function(file) {
      ggplot2::ggsave(file, plot = plot_spectra(),
                      width = user_settings$fig_width,
                      height = user_settings$fig_height,
                      units = user_settings$fig_units)
    },
    contentType = "application/pdf"
  )

  output$export_table <- downloadHandler(
    filename = "summary.csv",
    content = function(file) {
      utils::write.csv(user_table(), file, row.names = FALSE,
                       fileEncoding = "utf-8")
    },
    contentType = "text/csv"
  )
}
