#' @details
#' \tabular{ll}{
#'  \strong{Package:} \tab gammaShiny \cr
#'  \strong{Type:} \tab Package \cr
#'  \strong{Version:} \tab 0.1.0 \cr
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
#' Brice Lebrun\cr
#' \email{brice.lebrun@@u-bordeaux-montaigne.fr}
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
#' @importFrom DT datatable dataTableOutput editData renderDT
#' @importFrom ggplot2 theme_bw
#' @importFrom grDevices hcl
#' @importFrom kableExtra add_header_above cell_spec kable_styling row_spec
#' spec_color
#' @importFrom knitr kable
#' @importFrom khroma colour scale_color_discreterainbow scale_color_bright
#' scale_color_contrast scale_color_vibrant scale_color_muted scale_color_pale
#' scale_color_dark scale_color_light
#' @importFrom methods as is
#' @importFrom plotly ggplotly renderPlotly plotlyOutput
#' @importFrom shiny numericInput NS actionButton checkboxInput column
#' conditionalPanel downloadButton downloadHandler fileInput fluidPage fluidRow
#' h4 h5 helpText htmlOutput icon img need observe observeEvent radioButtons
#' reactive reactiveVal reactiveValues renderPrint renderText renderUI req
#' selectInput showNotification sliderInput tabPanel tabsetPanel tagList tags
#' textAreaInput uiOutput updateSelectInput updateSliderInput validate
#' verbatimTextOutput navbarPage wellPanel enableBookmarking callModule
#' tableOutput stopApp bookmarkButton onBookmark onRestore textOutput
#' @importFrom shinyWidgets dropdownButton pickerInput
NULL
