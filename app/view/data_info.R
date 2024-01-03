#' @concept This script will display information about the selected dataset.

box::use(
  dplyr[across, mutate, where],
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

    # Custom function to format numbers -- was originally adding decimals to
    # whole numbers. May need to be re-addressed
    format_numbers <- function(x) {
      if (all(x == floor(x))) {
        return(format(x, nsmall = 0, scientific = FALSE))
      } else {
        return(x)
      }
    }

    output$contents <- renderTable({
      if (is.null(newFile())) {
        return(NULL)
      }

      # Apply the formatting to the numeric columns
      df <- newFile() |>
        mutate(across(where(is.numeric), format_numbers))

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
