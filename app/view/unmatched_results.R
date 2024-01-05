#' This module will be designed to run the matching algorithm. It will then
#' send the results to the appropriate tabs on the main app script.
#' The main matching results will go to one tab in the "results" section
#' while the unmatched cases & controls will go to their respective tabs.

box::use(
  dplyr[across, filter, mutate, select, where],
  glue[glue],
  shiny[moduleServer, NS, reactive, renderTable, renderText, req,
        tableOutput, tagList, textOutput],
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
    tableOutput(ns("unmatched"))
  )
}

#' @export
server <- function(id, newFile, inputs, results, the_filter) {
  moduleServer(id, function(input, output, session) {

    # remove the unmatched cases/controls
    unmatched <- reactive({
      req(newFile(), inputs(), results())

      # convert the strings to symbols
      idVar <- rlang::sym(inputs()$idVariable)
      case_control <- rlang::sym(inputs()$caseControl)

      if (the_filter == "cases") {
        unmatched <- newFile() |>
          filter(
            !{{ idVar }} %in% results()$case_id,
            {{ case_control }} == 1
          )
      } else {
        unmatched <- newFile() |>
          filter(
            !{{ idVar }} %in% results()$control_id,
            {{ case_control }} == 0
          )
      }

      unmatched |>
        select(-{{ case_control }}) |>
        mutate(across(where(is.numeric), format_numbers))
    })

    # count the number of unmatched cases/controls
    how_many <- reactive({
      nrow(unmatched())
    })

    # render the text output
    output$instructions <- renderText({
      req(results())
      if (is.null(unmatched())) {
        return(NULL)
      }
      glue::glue(
        "Displaying the first 20 rows of {how_many()} total
        unsuccessfully matched {the_filter}."
      )
    })

    # render the table output
    output$unmatched <- renderTable({
      req(results())
      if (is.null(unmatched())) {
        return(NULL)
      }
      unmatched() |>
        head(20)
    })

  })
}
