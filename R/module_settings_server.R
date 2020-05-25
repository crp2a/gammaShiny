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
    colour <- try(khroma::colour(input$fig_colour), silent = TRUE)
    if (inherits(colour, "try-error")) {
      # Emulate ggplot2 default colours
      colour <- function(n) {
        hues = seq(15, 375, length = n + 1)
        grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
      }
    } else {
      colour <- colour
    }

    scale <- switch(
      input$fig_colour,
      `discrete rainbow` = khroma::scale_color_discreterainbow(),
      bright = khroma::scale_color_bright(),
      contrast = khroma::scale_color_contrast(),
      vibrant = khroma::scale_color_vibrant(),
      muted = khroma::scale_color_muted(),
      pale = khroma::scale_color_pale(),
      dark = khroma::scale_color_dark(),
      light = khroma::scale_color_light(),
      NULL
    )

    user_settings$digits <- input$digits
    user_settings$fig_width <- input$fig_width
    user_settings$fig_height <- input$fig_height
    user_settings$fig_units <- input$fig_units
    user_settings$fig_colour <- colour
    user_settings$fig_scale <- scale
  })
  # Render
  output$session <- renderPrint({ utils::sessionInfo() })
}
