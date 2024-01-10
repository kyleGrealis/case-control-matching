#' This module will be designed to run the matching algorithm. It will then
#' send the results to the appropriate tabs on the main app script.
#' The main matching results will go to one tab in the "results" section
#' while the unmatched cases & controls will go to their respective tabs.

box::use(
  dplyr[across, mutate, where],
  glue[glue],
  reactable[reactable, reactableOutput, renderReactable],
  shiny[moduleServer, NS, req, tagList, renderText, textOutput],
)

box::use(
  app/logic/functions[format_numbers, my_tooltip],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("instructions")),
    reactableOutput(ns("matched"))
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
      glue::glue(
        "There are {nrow(results())} successful matches."
      ) |>
        my_tooltip()
    })

    output$matched <- renderReactable({
      req(results())
      if (is.null(results())) {
        return(NULL)
      }
      results() |>
        mutate(across(where(is.numeric), format_numbers)) |>
        reactable()
    })

  })
}
