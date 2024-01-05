#' @concept This script will display information about the selected dataset.

box::use(
  dplyr[across, mutate, where],
  glue[glue],
  shiny[moduleServer, NS, renderTable, renderUI, tableOutput, tagList, uiOutput],
  stringr[str_to_title],
  utils[head]
)

box::use(
  app/logic/functions[format_numbers]
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

      # Apply the formatting to the numeric columns
      df <- newFile() |>
        mutate(across(where(is.numeric), format_numbers))

      # Make all variable names title case
      colnames(df) <- stringr::str_to_title(colnames(df))

      head(df, n = 20)
    })

    output$tableSummaryMessage <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "The dataset contains {nrow(newFile())} rows and {ncol(newFile())} columns."
      )
    })

    output$tableFootnote <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "Displaying the first 20 rows."
      )
    })

  })
}
