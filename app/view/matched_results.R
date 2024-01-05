#' This module will be designed to run the matching algorithm. It will then
#' send the results to the appropriate tabs on the main app script.
#' The main matching results will go to one tab in the "results" section
#' while the unmatched cases & controls will go to their respective tabs.

box::use(
  dplyr[across, mutate, where],
  shiny[moduleServer, NS, renderTable, req, tableOutput, tagList,
        renderText, textOutput],
  utils[head]
)

box::use(
  app/logic/functions[format_numbers],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("instructions")),
    tableOutput(ns("matched"))
  )
}

#' @export
server <- function(id, results) {
  moduleServer(id, function(input, output, session) {

    output$instructions <- renderText({
      req(results())
      if (is.null(results())) {
        return(NULL)
      }
      "Displaying the first 20 rows of successful matches."
    })

    output$matched <- renderTable({
      req(results())
      if (is.null(results())) {
        return(NULL)
      }
      results() |>
        mutate(across(where(is.numeric), format_numbers)) |>
        head(20)
    })

  })
}
