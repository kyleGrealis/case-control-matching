#' @concept This script will display information about the selected dataset.

box::use(
  bslib[tooltip],
  dplyr[across, mutate, where],
  glue[glue],
  shiny[moduleServer, NS, renderUI, tagList, uiOutput],
  reactable[reactable, reactableOutput, renderReactable],
)

box::use(
  app/logic/functions[format_numbers]
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

    output$contents <- renderReactable({
      if (is.null(newFile())) {
        return(NULL)
      }

      # Apply the formatting to the numeric columns and create reactable
      newFile() |>
        mutate(across(where(is.numeric), format_numbers)) |>
        reactable()
    })

    output$tableSummaryMessage <- renderUI({
      if (is.null(newFile())) {
        return(NULL)
      }
      glue::glue(
        "The dataset contains {nrow(newFile())} participants
        and {ncol(newFile())} variable columns."
      ) |>
        tooltip(
          "Sort the table by clicking on the variable names.",
          placement = "top"
        )
    })

  })
}
