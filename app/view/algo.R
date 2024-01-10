#' This module will be designed to run the matching algorithm. It will return
#' the dataset that produces the most matches and render a status update to
#' the user.

box::use(
  dplyr[bind_cols, case_when, filter, mutate, n_distinct, pull, rename,
        select, slice_sample, summarize],
  glue[glue],
  purrr[map_dfr],
  rlang[sym],
  shiny[isolate, moduleServer, NS, observeEvent,
        Progress, reactive,
        reactiveVal, reactiveValues,
        renderText, req, showNotification, tagList, verbatimTextOutput],
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
    verbatimTextOutput(ns("iteration_results"))
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

      # This is setting up the user notification for the matching progress
      # adapting this from ?shiny::Progress example
      progress <- Progress$new(session, min = 0, max = iterations)
      on.exit(progress$close)
      progress$set(
        message = "Finding the best possible case-control matches",
        detail  = "Thank you for your patience..."
      )

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

        # using the shiny::Progress, set the incremental value based on loop index
        # if this is placed higher in the loop sequence, the indicator bar appears
        # to be full before the final matching algorithm is actually ran
        # moved here because it looked weird above
        progress$set(value = i)

      }

      # end of computation time
      end_time <- proc.time()
      comp_time <- end_time - start_time
      print(glue::glue("\n\nTotal matching time: {round(comp_time[3], 2)} seconds"))
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
