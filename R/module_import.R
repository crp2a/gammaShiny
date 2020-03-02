module_import_spectra <- function(id) {

  # Create a namespace function using the provided id
  ns <- NS(id)

  tabPanel(
    "1. Import",
    icon = icon("upload"),
    fluidPage(
      fluidRow(
        column(
          width = 4,
          wellPanel(
            fileInput(
              "import_files", "Import spectrum file(s)",
              multiple = TRUE,
              accept = c('.cnf', '.CNF', '.tka', '.TKA')
            ),
            tags$hr(),
            shinyWidgets::pickerInput(
              inputId = "import_select",
              label = "Select",
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                size = 10,
                `selected-text-format` = "count > 3"
              )
            ),
            checkboxInput("import_facet", "Display in a grid", value = FALSE),
            radioButtons("import_xaxis", "X axis", choices = c("chanel", "energy")),
            radioButtons("import_yaxis", "Y axis", choices = c("count", "rate"))
          )
        ),
        column(
          width = 8,
          column(
            width = 12,
            downloadButton("import_export_plot", "Export plot"),
            downloadButton("import_export_table", "Export summary")
          ),
          column(
            width = 12,
            style = "margin-top: 25px;",
            plotOutput(
              "import_plot",
              dblclick = "import_plot_dblclick",
              brush = brushOpts(id = "import_plot_brush", resetOnNew = TRUE)
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          htmlOutput("import_summary")
        )
      )
    )
  )
}
