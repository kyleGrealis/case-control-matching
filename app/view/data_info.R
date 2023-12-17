#' @concept This script will display information about the selected dataset.

box::use(
  glue[glue],
  shiny[moduleServer, NS, renderTable, renderUI, tableOutput, tagList, uiOutput],
  utils[head]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tableSummaryMessage")),
    tableOutput(ns("contents")),
    uiOutput(ns("tableFootnote"))
  )
}

#' @export
server <- function(id, newFile) {
  moduleServer(id, function(input, output, server) {

    output$contents <- renderTable({
      if (is.null(newFile())) {
        return(NULL)
      }
      head(newFile(), n = 20)
    })

    output$tableSummaryMessage <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "The table has {nrow(newFile())} rows and {ncol(newFile())} columns."
      )
    })

    output$tableFootnote <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "Displaying only the first 20 rows."
      )
    })

  })
}
