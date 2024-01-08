#' This module will be designed to run the matching algorithm. It will return
#' the dataset that produces the most matches and render a status update to
#' the user.

box::use(
  dplyr[bind_cols, case_when, filter, mutate, n_distinct, pull, rename,
        select, slice_sample, summarize],
  glue[glue],
  purrr[map_dfr],
  rlang[sym],
  shiny[isolate, moduleServer, NS, observeEvent, reactiveVal, reactiveValues,
        renderText, tagList, verbatimTextOutput],
  utils[head],
)

box::use(
  app/logic/matching_algo,
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("iteration_results"))
  )
}

#' @export
server <- function(id, newFile, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    best_matched_data <- reactiveVal()
    # for text output
    rv <- reactiveValues(data = "")

    observeEvent(inputs()$matchButton, {

      best_matched_data(NULL)

      # start computation timer
      start_time <- proc.time()

      # matching loop
      for(i in 1:1) {

        # provide feedback to the user
        print(glue::glue("Starting iteration {i}..."))

        # get the results of the best iteration
        if (!is.null(best_matched_data())) {
          best_match_results <- best_matched_data() |>
            summarize(
              n_cases = n_distinct(case_id),
              n_controls = n_distinct(control_id)
            )
        }

        # run the matching algorithm
        matched_data <- matching_algo$mitter_match(
          newFile,
          inputs()$idVariable, inputs()$caseControl,
          inputs()$numericVariable, inputs()$numRange,
          inputs()$categoricalVariable, inputs()$ratio,
          inputs()$thirdVariable
        )

        # count the number of unique cases and controls
        match_results <- matched_data |>
          summarize(
            n_cases = n_distinct(case_id),
            n_controls = n_distinct(control_id)
          )

        # provide feedback to the user
        print(
          glue::glue(
            "\nIteration {i} produced {nrow(matched_data)} rows and
            {match_results$n_cases} matched cases.
            \n------------------------------------------------------"
          )
        )

        # compare the results to the best results
        if (is.null(best_matched_data())) {
          best_matched_data(matched_data)
        } else {
          if (match_results$n_cases > best_match_results$n_cases) {
            best_matched_data(matched_data)
          } else if (match_results$n_cases == best_match_results$n_cases &&
                     nrow(matched_data) > nrow(best_matched_data())) {
            best_matched_data(matched_data)
          }
        }

      }

      # end of computation time
      end_time <- proc.time()
      comp_time <- end_time - start_time
      print(glue::glue("\n\nTotal matching time: {round(comp_time[3], 2)} seconds"))

      # return(best_matched_data)

      # for (i in 1:1) {
      #   # Provide feedback to the user
      #   new_line <- glue::glue("Starting iteration {i}...")
      #   message(glue::glue("Starting iteration {i}..."))
      #
      #   # run the matching algorithm
      #   results(
      #     matching_algo$mitter_match(
      #       newFile,
      #       inputs()$idVariable, inputs()$caseControl,
      #       inputs()$numericVariable, inputs()$numRange,
      #       inputs()$categoricalVariable, inputs()$ratio,
      #       inputs()$thirdVariable
      #     )
      #   )
      #
      #   message(glue::glue("Ending iteration {i}"))
      #   # Provide feedback to the user
      #   result_line <- glue::glue(
      #     "\nIteration {i} produced {i} rows and XYZ matched cases.
      #     \n------------------------------------------------------"
      #   )
      #
      #   # Store the lines in the reactive value
      #   rv$data <- paste(rv$data, new_line, result_line, sep = "\n")
      # }


    }, once = TRUE)

    # output$iteration_results <- renderText({
    #   rv$data
    # })

    # return(results)
    return(best_matched_data)
  })
}
