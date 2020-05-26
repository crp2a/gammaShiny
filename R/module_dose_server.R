#' Dose Rate Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_data a \code{\link[shiny]{reactiveValues}} list with the
#'  following elements: "\code{spectra}", "\code{names}" and "\code{raw}".
#' @param user_settings a \code{\link[shiny]{reactiveValues}} list.
#' @seealso \link{module_dose_ui}
#' @family module
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
    req(input$sigma, input$epsilon)
    withCallingHandlers(
      {
        predict_dose(user_curve(), user_spectra(),
                     sigma = input$sigma, epsilon = input$epsilon / 100)
      },
      warning = function(e) {
        warn <- gsub("\n|\\*", "", e$message)
        showNotification(warn, duration = 10, type = "warning")
        invokeRestart("muffleWarning")
      }
    )
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
  output$results <- renderText({
    tbl <- knitr::kable(
      user_dose(),
      digits = user_settings$digits,
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
      header = c(" " = 1, "Ni" = 2, "NiEi" = 2)
    )
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
