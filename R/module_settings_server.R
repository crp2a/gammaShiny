#' Settings Server
#'
#' @param input provided by \pkg{Shiny}.
#' @param output provided by \pkg{Shiny}.
#' @param session provided by \pkg{Shiny}.
#' @param user_settings a \code{\link[shiny]{reactiveValues}} list.
#' @seealso \link{module_settings_ui}
#' @family module
#' @export
module_settings_server <- function(input, output, session, user_settings) {
  observe({
    req(input$digits, input$fig_width, input$fig_height, input$fig_units)
    user_settings$digits <- input$digits
    user_settings$fig_width <- input$fig_width
    user_settings$fig_height <- input$fig_height
    user_settings$fig_units <- input$fig_units
  })
  # Render
  output$session <- renderPrint({ utils::sessionInfo() })
}
