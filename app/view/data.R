#' @concept This script is the data import module. The user will select the
#'  dataset to be used for the case-control matching
#' Will import user-seleted dataset. Can be any type (e.g. - .xlsx, .csv, .sas7bdat, etc.)
#' @example test_data.sas7bdat

box::use(
  bslib[tooltip],
  rio[import],
  shiny[fileInput, moduleServer, NS, reactive],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  fileInput(ns("file1"), "Choose data to import.", accept = NULL) |>
    tooltip(
      "Is the data coded 0=\"Control\" and 1=\"Case\"?",
      placement = "top"
    )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, server) {
    newFile <- reactive({
      if (is.null(input$file1)) {
        return(NULL)
      }
      rio::import(input$file1$datapath)
    })

    return(newFile)
  })
}
