
# algorithm setup ---------------------------------------------------------
# working out the matching algorithm
library(dplyr)
library(purrr)

# simulated dataset and inputs
dat <- rio::import("~/Downloads/test_set.sas7bdat")
idVar <- "participant_id"
caseControl <- "event"
numVar <- "age"
numRange <- 2
catVar <- "gender"
ratio <- 2
# thirdVar <- NULL
thirdVar <- "ethnicity"

# Convert the variable names to symbols
idVar <- rlang::sym(idVar)
caseControl <- rlang::sym(caseControl)
catVar <- rlang::sym(catVar)
numVar <- rlang::sym(numVar)
if (!is.null(thirdVar)) {
  thirdVar <- rlang::sym(thirdVar)
}

# Split the data into cases and controls
case <- dat |>
  filter({{ caseControl }} == 1) |>
  rename(
    case_id = {{ idVar }},
    case_num_var = {{ numVar }},
    case_cat_var = {{ catVar }},
    case_third_var = {{ thirdVar }}
  ) |>
  select(-c({{ caseControl }}))

control <- dat |>
  filter({{ caseControl }} == 0) |>
  # Add control_usage as a column
  mutate(control_usage = 0) |>
  rename(
    control_id = {{ idVar }},
    control_num_var = {{ numVar }},
    control_cat_var = {{ catVar }},
    control_third_var = {{ thirdVar }}
  ) |>
  select(-c({{ caseControl }}))

# set this to be an initial empty value
combined_rows <- NULL


# Define a function to find two controls for each case and return a single row
# that includes the case and its matched controls
find_matches <- function(this_case) {

  # test as the function is built
  # this_case <- case[2,1]

  # Get the case row
  case_row <- case |> filter(case_id == this_case)

  # Access the numVar and catVar columns in the case_row data frame
  numeric_value <- case_row |> pull(case_num_var)
  categorical_value <- case_row |> pull(case_cat_var)
  if (!is.null(thirdVar)) {
    third_value <- case_row |> pull(case_third_var)
  }

  # Check if there are any possible matches for this case
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
  # browser()

  # If there are no possible matches, return NULL
  if (nrow(possible_controls) == 0) {
    return(NULL)
  }

  # Select two controls at random
  matched_controls <- possible_controls |>
    slice_sample(n = ratio)

  # Combine the case and its matched controls into a single row
  combined_rows <- bind_cols(case_row, matched_controls) |>
    select(case_id, case_num_var, case_cat_var, case_third_var, control_id)
  # browser()

  # Update the control usage count
  control <<- control |>
    mutate(
      control_usage = case_when(
        control_id %in% combined_rows$control_id ~ 1,
        TRUE ~ control_usage
      )
    )
  # browser()

  return(combined_rows)
  # return(list(combined_rows = combined_rows, control = control))
} # end of find_matches()


# test find_matches() on one case
this_case <- case[2,1]
result <- find_matches(this_case)

# test find_matches on all cases and calculate computation time
start_time <- proc.time()
matched_data <- case$case_id |> map_dfr(find_matches)
end_time <- proc.time()
end_time - start_time
# works to this point -----------------------------------------------------


# build the function ------------------------------------------------------

# set up the empty function
mitter_match <- function(dat, idVar, caseControl, numVar, numRange, catVar, ratio, thirdVar = NULL) {

  # Convert the variable names to symbols
  idVar <- rlang::sym(idVar)
  caseControl <- rlang::sym(caseControl)
  catVar <- rlang::sym(catVar)
  numVar <- rlang::sym(numVar)
  if (!is.null(thirdVar)) {
    thirdVar <- rlang::sym(thirdVar)
  }

  # Split the data into cases and controls
  case <- dat |>
    filter({{ caseControl }} == 1) |>
    rename(
      case_id = {{ idVar }},
      case_num_var = {{ numVar }},
      case_cat_var = {{ catVar }},
      case_third_var = {{ thirdVar }}
    ) |>
    select(-c({{ caseControl }}))

  control <- dat |>
    filter({{ caseControl }} == 0) |>
    # Add control_usage as a column
    mutate(control_usage = 0) |>
    rename(
      control_id = {{ idVar }},
      control_num_var = {{ numVar }},
      control_cat_var = {{ catVar }},
      control_third_var = {{ thirdVar }}
    ) |>
    select(-c({{ caseControl }}))

  # set this to be an initial empty value
  combined_rows <- NULL

  # Define a function to find two controls for each case and return a single row
  # that includes the case and its matched controls
  find_matches <- function(this_case) {

    # Get the case row
    case_row <- case |> filter(case_id == this_case)

    # Access the numVar and catVar columns in the case_row data frame
    numeric_value <- case_row |> pull(case_num_var)
    categorical_value <- case_row |> pull(case_cat_var)
    if (!is.null(thirdVar)) {
      third_value <- case_row |> pull(case_third_var)
    }

    # Check if there are any possible matches for this case
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

    # If there are no possible matches, return NULL
    if (nrow(possible_controls) == 0) {
      return(NULL)
    }

    # Select two controls at random
    matched_controls <- possible_controls |>
      slice_sample(n = ratio)

    # Combine the case and its matched controls into a single row
    combined_rows <<- bind_cols(case_row, matched_controls) |>
      select(
        case_id, case_num_var, case_cat_var, case_third_var,
        control_id, control_num_var
      )

    # Update the control usage count
    control <<- control |>
      mutate(
        control_usage = case_when(
          control_id %in% combined_rows$control_id ~ 1,
          TRUE ~ control_usage
        )
      )
    # browser()

    return(combined_rows)
    # return(list(combined_rows = combined_rows, control = control))
  } # end of find_matches()

  # Apply the function
  matched_data <- case$case_id |> map_dfr(find_matches)

  return(matched_data)
}


# use matching algorithm with loop
do_matching <- function(dat, idVar, caseControl, numVar, numRange, catVar, ratio, thirdVar = NULL) {

  # # Convert the variable names to symbols
  # idVar <- rlang::sym(idVar)
  # caseControl <- rlang::sym(caseControl)
  # catVar <- rlang::sym(catVar)
  # numVar <- rlang::sym(numVar)
  # if (!is.null(thirdVar)) {
  #   thirdVar <- rlang::sym(thirdVar)
  # }

  best_matched_data <- NULL
  for(i in 1:3) {
    print(glue::glue("Starting iteration {i}..."))
    # get the results of the best iteration
    if (!is.null(best_matched_data)) {
      best_match_results <- best_matched_data |>
        summarise(
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
      summarise(
        n_cases = n_distinct(case_id),
        n_controls = n_distinct(control_id)
      )
    # print the results
    print(
      glue::glue(
        "Iteration {i} has {nrow(matched_data)} rows and {match_results$n_cases} used cases."
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

  return(best_matched_data)
}

my_matches <- do_matching(
  dat,
  idVar,
  caseControl,
  numVar,
  numRange,
  catVar,
  ratio,
  thirdVar
)
