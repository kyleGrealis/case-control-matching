#' This module will be designed to run the matching algorithm. It will then
#' send the results to the appropriate tabs on the main app script.
#' The main matching results will go to one tab in the "results" section
#' while the unmatched cases & controls will go to their respective tabs.

box::use(
  bsicons[bs_icon],
  dplyr[across, filter, mutate, select, where],
  glue[glue],
  reactable[reactable, reactableOutput, renderReactable],
  shiny[downloadButton, downloadHandler, div, moduleServer, NS, reactive,
        renderUI, req, tagList, uiOutput],
)

# MUST use renderUI & uiOutput in order to use the tooltip with glue!

box::use(
  app/logic/functions[format_numbers, my_tooltip],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; justify-content: space-between;",
      uiOutput(ns("instructions")),
      uiOutput(ns("download"))
    ),
    reactableOutput(ns("unmatched"))
  )
}

#' @export
server <- function(id, newFile, inputs, results, the_filter) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    output$instructions <- renderUI({
      req(results())
      if (is.null(unmatched())) {
        return(NULL)
      } else {
        glue::glue(
          "Displaying unmatched {the_filter}."
        ) |>
          my_tooltip()
      }
    })

    # this is for the download button
    output$download <- renderUI({
      req(results())
      if (is.null(results())) {
        return(NULL)
      }
      downloadButton(
        ns("downloadData"),
        # label = "csv"
        label = NULL
      )
    })

    # this is for the actual downloaded file
    output$downloadData <- downloadHandler(
      filename = glue::glue("unmatched-{the_filter}-{Sys.Date()}.csv"),
      content = function(file) {
        rio::export(unmatched(), file)
      }
    )

    # render the table output
    output$unmatched <- renderReactable({
      req(results())
      if (is.null(unmatched())) {
        return(NULL)
      }
      unmatched() |>
        reactable()
    })

  })
}
