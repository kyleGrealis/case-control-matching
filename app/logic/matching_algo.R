#' This is the case-control matching algorithm. It will perform matching on
#' 2 or 3 variables based on the user input. The algorithm will iterate
#' through the algorithm 5 times and keep the final dataset yielding the
#' most number of matched cases.


box::use(
  dplyr[bind_cols, case_when, filter, mutate, n_distinct, pull, rename,
        select, slice_sample, summarize],
  glue[glue],
  purrr[map_dfr],
  rlang[sym],
  utils[head]
)


# build the function ------------------------------------------------------
mitter_match <- function(dat, idVar, caseControl, numVar, numRange, catVar, ratio, thirdVar = NULL) {
  browser()
  # Convert the variable names to symbols
  idVar <- rlang::sym(idVar)
  caseControl <- rlang::sym(caseControl)
  catVar <- rlang::sym(catVar)
  numVar <- rlang::sym(numVar)
  if (thirdVar != "blank") {
    thirdVar <- rlang::sym(thirdVar)
  }

  # Split the data into cases and controls
  if (thirdVar != "blank") {
    # matching on 3 variables
    case <- dat |>
      filter({{ caseControl }} == 1) |>
      rename(
        case_id = {{ idVar }},
        case_num_var = {{ numVar }},
        case_cat_var = {{ catVar }},
        case_third_var = {{ thirdVar }}
      ) |>
      select(case_id, case_num_var, case_cat_var, case_third_var)
  } else {
    # matching on 2 variables
    case <- dat |>
      filter({{ caseControl }} == 1) |>
      rename(
        case_id = {{ idVar }},
        case_num_var = {{ numVar }},
        case_cat_var = {{ catVar }}
      ) |>
      select(case_id, case_num_var, case_cat_var)
  }

  if (thirdVar != "blank") {
    # matching on 3 variables
    control <- dat |>
      filter({{ caseControl }} == 0) |>
      mutate(control_usage = 0) |>
      rename(
        control_id = {{ idVar }},
        control_num_var = {{ numVar }},
        control_cat_var = {{ catVar }},
        control_third_var = {{ thirdVar }}
      ) |>
      select(
        control_id, control_num_var, control_cat_var,
        control_third_var, control_usage
      )
  } else {
    # matching on 2 variables
    control <- dat |>
      filter({{ caseControl }} == 0) |>
      mutate(control_usage = 0) |>
      rename(
        control_id = {{ idVar }},
        control_num_var = {{ numVar }},
        control_cat_var = {{ catVar }}
      ) |>
      select(control_id, control_num_var, control_cat_var, control_usage)
  }


  # Set this to be an initial empty value
  combined_rows <- NULL

  # find all matches
  find_matches <- function(this_case) {

    # Get the case row
    case_row <- case |> filter(case_id == this_case)

    # Access the numVar and catVar columns in the case_row data frame
    numeric_value <- case_row |> pull(case_num_var)
    categorical_value <- case_row |> pull(case_cat_var)

    if (thirdVar != "blank") {

      ## only for matching on 3 variables
      third_value <- case_row |> pull(case_third_var)
      possible_controls <- control |>
        filter(
          control_cat_var == categorical_value,
          control_num_var >= numeric_value - numRange,
          control_num_var <= numeric_value + numRange,
          control_third_var == third_value,
          # Only consider controls that have not been used
          !control_id %in% combined_rows$control_id,
          control_usage == 0
        )
    } else {
      ## only for matching on 2 variables
      possible_controls <- control |>
        filter(
          control_cat_var == categorical_value,
          control_num_var >= numeric_value - numRange,
          control_num_var <= numeric_value + numRange,
          # Only consider controls that have not been used
          !control_id %in% combined_rows$control_id,
          control_usage == 0
        )
    }

    # If there are no possible matches, return NULL
    if (nrow(possible_controls) == 0) {
      return(NULL)
    }

    # Select two controls at random
    matched_controls <- possible_controls |>
      slice_sample(n = ratio)

    # Combine the case and its matched controls into a single row
    if (thirdVar != "blank") {
      # matching on 3 variables
      combined_rows <<- bind_cols(case_row, matched_controls) |>
        select(
          case_id, case_num_var,
          control_id, control_num_var,
          case_cat_var, case_third_var
        )
    } else {
      # matching on 2 variables
      combined_rows <<- bind_cols(case_row, matched_controls) |>
        select(
          case_id, case_num_var,
          control_id, control_num_var,
          case_cat_var
        )
    }

    # Update the control usage count
    control <<- control |>
      mutate(
        control_usage = case_when(
          control_id %in% combined_rows$control_id ~ 1,
          TRUE ~ control_usage
        )
      )

    return(combined_rows)

  } # end of find_matches()

  # Apply the function & calculate computation time
  start_time <- proc.time()
  matched_data <- case$case_id |> map_dfr(find_matches)
  end_time <- proc.time()
  comp_time <- end_time - start_time

  return(matched_data)
}


#' @export
# use matching algorithm with loop ----------------------------------------
do_matching <- function(dat, idVar, caseControl, numVar, numRange, catVar, ratio, thirdVar = NULL) {

  # added from Copilot
  dat <- dat()

  best_matched_data <- NULL

  # start computation timer
  start_time <- proc.time()

  # matching loop
  for(i in 1:3) {

    # provide feedback to the user
    print(glue::glue("Starting iteration {i}..."))

    # get the results of the best iteration
    if (!is.null(best_matched_data)) {
      best_match_results <- best_matched_data |>
        summarize(
          n_cases = n_distinct(case_id),
          n_controls = n_distinct(control_id)
        )
    }

    # run the matching algorithm
    matched_data <- mitter_match(
      dat,
      idVar,
      caseControl,
      numVar,
      numRange,
      catVar,
      ratio,
      thirdVar
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
        "\nIteration {i} produced {nrow(matched_data)} rows and {match_results$n_cases} matched cases.
        \n------------------------------------------------------"
      )
    )

    # compare the results to the best results
    if (is.null(best_matched_data)) {
      best_matched_data <- matched_data
    } else {
      if (match_results$n_cases > best_match_results$n_cases) {
        best_matched_data <- matched_data
      } else if (match_results$n_cases == best_match_results$n_cases &&
                 nrow(matched_data) > nrow(best_matched_data)) {
        best_matched_data <- matched_data
      } else {
        NULL
      }
    }

  }

  # end of computation time
  end_time <- proc.time()
  comp_time <- end_time - start_time
  print(glue::glue("\n\nTotal matching time: {round(comp_time[3], 2)} seconds"))

  return(best_matched_data)
}

