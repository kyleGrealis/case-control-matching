#' This module will be designed to run the matching algorithm. It will then
#' send the results to the appropriate tabs on the main app script.
#' The main matching results will go to one tab in the "results" section
#' while the unmatched cases & controls will go to their respective tabs.

box::use(
  shiny[moduleServer, NS, observe, reactiveVal]
)

box::use(
  app/logic/matching_algo,
)

#' @export
server <- function(id, newFile, inputs) {
  moduleServer(id, function(input, output, session) {
    results <- reactiveVal()  # Create a reactive value for results

    observe({
      if (inputs()$matchButton) {
        results(matching_algo$do_matching(
          newFile,
          inputs()$idVariable, inputs()$caseControl,
          inputs()$numericVariable, inputs()$numRange,
          inputs()$categoricalVariable, inputs()$ratio,
          inputs()$thirdVariable
        ))
      }
    })

    return(results)  # Return the reactive value
  })
}