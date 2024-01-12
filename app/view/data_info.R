#' @concept This script will display information about the selected dataset.

box::use(
  dplyr[across, mutate, where],
  glue[glue],
  reactable[reactable, reactableOutput, renderReactable],
  shiny[moduleServer, NS, renderUI, tagList, uiOutput],
)

# MUST use renderUI & uiOutput in order to use the tooltip with glue!

box::use(
  app/logic/functions[format_numbers, my_tooltip],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tableSummaryMessage")),
    reactableOutput(ns("contents"))
  )
}

#' @export
server <- function(id, newFile) {
  moduleServer(id, function(input, output, server) {

    output$tableSummaryMessage <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "The dataset contains {nrow(newFile())} participants
        and {ncol(newFile())} variable columns."
      ) |>
        my_tooltip()
    })

    output$contents <- renderReactable({
      if (is.null(newFile())) {
        return(NULL)
      }

      # Apply the formatting to the numeric columns and create reactable
      newFile() |>
        mutate(across(where(is.numeric), format_numbers)) |>
        reactable(defaultPageSize = 15)
    })

  })
}
