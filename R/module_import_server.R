#' Import Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a \code{\link[shiny]{reactiveValues}} list with the
#'  following elements: "\code{spectra}", "\code{names}" and "\code{raw}".
#' @param user_settings a \code{\link[shiny]{reactiveValues}} list.
#' @seealso \link{module_import_ui}
#' @family module
#' @export
module_import_server <- function(input, output, session,
                                 user_data, user_settings) {
  user_files <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$files, message = FALSE))
    input$files
  })
  user_spectra <- reactive({
    validate(
      need(!is.null(user_data$spectra), message = FALSE),
      need(!is.null(input$select) && input$select != "", message = FALSE)
    )
    user_data$spectra[input$select]
  })
  user_table <- reactive({ summarise(user_spectra()) })
  plot_spectra <- reactive({
    plot(user_spectra(), xaxis = input$xaxis, yaxis = input$yaxis,
         select = input$select, facet = input$facet) +
      ggplot2::theme_bw() +
      user_settings$fig_scale
  })
  observe({
    # Return a GammaSpectra object
    spc_name <- tools::file_path_sans_ext(user_files()$name)
    spc_data <- gamma::read(user_files()$datapath)
    spc_data <- methods::as(spc_data, "GammaSpectra")
    set_names(spc_data) <- spc_name

    # Store data
    user_data$spectra <- spc_data
    user_data$names <- spc_name
    user_data$raw <- spc_data

    # Update UI
    shinyWidgets::updatePickerInput(
      session,
      inputId = "select",
      choices = spc_name,
      selected = spc_name
    )
  })
  # Render
  output$plot <- plotly::renderPlotly({
    plotly::ggplotly(plot_spectra())
  })
  output$summary <- renderText({
    tbl <- user_table()
    id <- tbl$name
    tbl$name <- kableExtra::cell_spec(
      x = tbl$name,
      format = "html",
      bold = TRUE,
      color = "white",
      background = factor(id, id, user_settings$fig_colour(length(id)))
    )
    tbl <- knitr::kable(
      x = tbl,
      format = "html",
      escape = FALSE,
      digits = user_settings$digits,
      row.names = FALSE,
      col.names = c("Name", "Date", "Live time", "Real time", "Chanels",
                    "Min.", "Max.")
    )
    tbl <- kableExtra::kable_styling(
      kable_input = tbl,
      bootstrap_options = c("striped", "hover"),
      full_width = TRUE, fixed_thead = TRUE
    )
    kableExtra::add_header_above(
      kable_input = tbl,
      header = c(" " = 5, "Energy Range" = 2)
    )
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
