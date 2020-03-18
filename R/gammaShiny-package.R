#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab gammaShiny \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 0.2.0 \cr
#'  \strong{License:} \tab GPL-3 \cr
#'  \strong{DOI:} \tab \href{https://doi.org/xxx}{xxx}
#' }
#' @author
#' \strong{Full list of authors and contributors} (alphabetic order)
#'
#' \tabular{ll}{
#'  Nicolas Frerebeau \tab \emph{Université Bordeaux Montaigne, France} \cr
#'  Brice Lebrun \tab \emph{Université Bordeaux Montaigne, France}
#' }
#'
#' \strong{Package maintainer}
#'
#' Nicolas Frerebeau\cr
#' \email{nicolas.frerebeau@@u-bordeaux-montaigne.fr}
#'
#' IRAMAT-CRP2A (UMR 5060)\cr
#' Maison de l'Archéologie\cr
#' Université Bordeaux Montaigne\cr
#' F-33607 Pessac cedex\cr
#' France
#' @note
#' This work received a state financial support managed by the Agence Nationale
#' de la Recherche (France) throught the program \emph{Investissements d'avenir}
#' (ref. \href{https://lascarbx.labex.u-bordeaux.fr}{ANR-10-LABX-52} and
#' \href{https://amidex.univ-amu.fr/}{11-IDEX-0001}).
#' @name gammaShiny-package
#' @aliases gammaShiny-package gammaShiny
#' @docType package
#' @keywords internal
"_PACKAGE"

#' @import gamma
#' @importFrom ggplot2 aes coord_cartesian facet_wrap geom_errorbar
#' geom_errorbarh geom_path geom_point geom_pointrange geom_segment geom_vline
#' ggplot ggsave labs scale_x_continuous sec_axis theme_bw vars waiver
#' @importFrom kableExtra add_header_above kable_styling row_spec
#' @importFrom knitr kable
#' @importFrom shiny NS actionButton brushOpts checkboxInput column
#' downloadButton fileInput fluidPage fluidRow h4 h5 helpText htmlOutput icon
#' img numericInput plotOutput radioButtons selectInput sliderInput tabPanel
#' tableOutput tabsetPanel tags textAreaInput uiOutput
#' verbatimTextOutput wellPanel
#' @importFrom shinyWidgets pickerInput
NULL
