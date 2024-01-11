#' This module will be designed to run the matching algorithm. It will then
#' send the results to the appropriate tabs on the main app script.
#' The main matching results will go to one tab in the "results" section
#' while the unmatched cases & controls will go to their respective tabs.

box::use(
  bsicons[bs_icon],
  dplyr[across, filter, mutate, n_distinct, summarize, where],
  glue[glue],
  reactable[reactable, reactableOutput, renderReactable],
  rlang[sym],
  shiny[downloadButton, downloadHandler, div, moduleServer, NS, reactive, renderUI,
        req, tagList, uiOutput],
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
    reactableOutput(ns("matched"))
  )
}

#' @export
server <- function(id, newFile, inputs, results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reusing this from the app/view/algo module to calculate the number of cases
    # find total number of cases in users dataset
    total_cases <- reactive({
      req(newFile(), inputs())
      # browser()
      newFile <- newFile()
      caseControl <- rlang::sym(inputs()$caseControl)
      newFile |>
        filter({{ caseControl }} == 1) |>
        nrow()
    })

    cases_used <- reactive({
      req(results())
      results <- results()
      results |>
        summarize(
          n_cases = n_distinct(case_id),
          n_controls = n_distinct(control_id)
        )
    })

    output$instructions <- renderUI({
      req(results())
      if (is.null(results())) {
        return(NULL)
      }
      glue::glue(
        "{cases_used()$n_cases} of {total_cases()} total cases successfully matched to {nrow(results())} controls."
      ) |>
        my_tooltip()
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
      filename = glue::glue("Case-Control-matches-{Sys.Date()}.csv"),
      content = function(file) {
        rio::export(results(), file)
      }
    )

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
