#' About UI
#'
#' @param id A [`character`] vector to be used for the namespace.
#' @family modules
#' @export
module_about_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "About",
    icon = icon("circle-info"),
    fluidRow(
      column(
        width = 8,
        align = "center",
        offset = 2,
        wellPanel(
          img(src = "/logo.png", width = "120px", alt = "gamma"),
          h4(
            paste("gamma", utils::packageVersion("gamma"), sep = " "),
            tags$br(),
            paste("gammaShiny", utils::packageVersion("gammaShiny"), sep = " ")
          ),
          tags$br(),
          tags$p(
            icon("github"), "Source code:", tags$br(),
            tags$a(href = "https://github.com/crp2a/gamma",
                   "https://github.com/crp2a/gamma"), tags$br(),
            tags$a(href = "https://github.com/crp2a/gammaShiny",
                   "https://github.com/crp2a/gammaShiny")
          ),
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
}
