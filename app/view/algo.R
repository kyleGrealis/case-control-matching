#' This module will be designed to run the matching algorithm. It will return
#' the dataset that produces the most matches and render a status update to
#' the user.

box::use(
  dplyr[bind_cols, case_when, filter, mutate, n_distinct, pull, rename,
        select, slice_sample, summarize],
  glue[glue],
  purrr[map_dfr],
  rlang[sym],
  shiny[incProgress, moduleServer, NS, observeEvent, reactive, reactiveVal,
        reactiveValues, renderText, req, tagList, textOutput, withProgress],
  utils[head],
)

box::use(
  app/logic/matching_algo,
)

# Set the number of iterations here so it's easier to possibly adjust later
iterations <- 3

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("iteration_results"))
  )
}

#' @export
server <- function(id, newFile, inputs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

    # set empty reactive value for the dataset that will be presented in
    # the Results > Mached Data tab
    best_matched_data <- reactiveVal()

    # for text output that will display the final tally of matches and cases
    rv <- reactiveValues(data = "")

    # this handles the main matching portion of the entire app
    # the progress indicator is updated before and within the for loop
    observeEvent(inputs()$matchButton, {

      # this sets the message that will let the user know how the results came
      # to be presented that way
      rv$data <- paste(
        rv$data, glue::glue(
          "The algorithm retains the iteration that produces the greatest number of matched cases.
          There are {total_cases()} total cases in the uploaded dataset.
          ------------------------------------------------------"
        ), sep = "\n"
      )

      # the initial value is NULL since the loop hasn't started yet
      best_matched_data(NULL)

      # start computation timer
      start_time <- proc.time()

      # creating the progress notification. this must be wrapped around
      # the matching algorithm so that the progress bar and message is
      # properly updated and displayed.
      withProgress(
        message = "Finding the best possible case-control matches. Thank you for your patience...",
        style = "notification",
        detail = "Iteration 1",
        min = 0, max = iterations+1,
        value = 1, {
          # matching loop
          for(i in 1:iterations) {

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

            # increment the progress bar
            if (i < iterations) {
              incProgress(1, detail = glue::glue("Iteration {i+1}"))
            } else {
              incProgress(1, detail = "Finished!")
            }

            # count the number of unique cases and controls
            match_results <- matched_data |>
              summarize(
                n_cases = n_distinct(case_id),
                n_controls = n_distinct(control_id)
              )

            # update feedback to the user
            rv$data <- paste(
              rv$data, glue::glue(
                "\nIteration {i} matched {nrow(matched_data)} controls to {match_results$n_cases} cases.\n------------------------------------------------------"
              ), sep = "\n"
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
        }
      )

      # end of computation time
      end_time <- proc.time()
      comp_time <- end_time - start_time
      rv$data <- paste(
        rv$data, glue::glue(
          "\n\nTotal matching time: {round(comp_time[3], 2)} seconds"
        ), sep = "\n"
      )

    }, once = TRUE)

    # return the feedback to the user under the matching results
    output$iteration_results <- renderText({
      rv$data
    })

    return(best_matched_data)
  })
}
