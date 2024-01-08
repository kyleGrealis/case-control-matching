#' @concept This script will display information about the selected dataset.

box::use(
  dplyr[across, mutate, where],
  glue[glue],
  shiny[moduleServer, NS, renderTable, renderUI, tableOutput, tagList, uiOutput,
        withProgress],
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

      withProgress(
        message = "Uploading data", {
          # Create pause to see message
          Sys.sleep(2)

          # Apply the formatting to the numeric columns and display head
          df <- newFile() |>
            mutate(across(where(is.numeric), format_numbers)) |>
            head(20)
        }
      )
    })

    output$tableSummaryMessage <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "The dataset contains {nrow(newFile())} participants
        and {ncol(newFile())} variable columns."
      )
    })

    output$tableFootnote <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "Displaying the first 20 participants."
      )
    })

  })
}
