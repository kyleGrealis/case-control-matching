
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
thirdVar <- "ethnicity"

# Apply the function
matched_data <- mitter_match(
  dat,
  idVar,
  caseControl,
  numVar,
  numRange,
  catVar,
  ratio,
  thirdVar = NULL
)

# set up the empty function
mitter_match <- function(dat, idVar, caseControl, numVar, numRange, catVar, ratio, thirdVar = NULL) {
  # fill in with pieces as they are working

  # Convert the variable names to symbols
  idVar <- rlang::sym(idVar)
  caseControl <- rlang::sym(caseControl)
  catVar <- rlang::sym(catVar)
  numVar <- rlang::sym(numVar)
  if (!is.null(thirdVar)) {
    thirdVar <- rlang::sym(thirdVar)
  }
}



# Split the data into cases and controls
case <- dat |>
  filter({{ caseControl }} == 1) |>
  # Add case_usage as a column
  mutate(case_usage = 0) |>
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

# Define a function to find two controls for each case and return a single row
# that includes the case and its matched controls
# find_controls <- function(idVar) {
find_controls <- function(this_case) {

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
      # Only consider controls that have been used once or not at all
      control_usage <= ratio
    )

  # If there are no possible matches, return NULL
  if (nrow(possible_controls) == 0) {
    return(NULL)
  }

  # Select two controls at random
  matched_controls <- possible_controls |>
    slice_sample(n = ratio)

  # Update the case_usage column for the matched controls
  case$case_usage[case$case_id %in% matched_controls$case_id] <-
    case$case_usage[case$case_id %in% matched_controls$case_id] + 1

  # Update the control_usage column for the matched controls
  control$control_usage[control$control_id %in% matched_controls$control_id] <-
    control$control_usage[control$control_id %in% matched_controls$control_id] + 1

  # Combine the case and its matched controls into a single row
  combined_row <- bind_cols(case_row, matched_controls) |>
    select(case_id, case_num_var, case_cat_var, case_third_var, control_id)

  return(combined_row)
}

# works to this point -----------------------------------------------------

# filter the cases from the `dat` dataset that have not met the `ratio`
# number of matches

  # Try to find matches for each case
  iteration <- 0
  while (iteration < 100) {
    new_matches <- case$case_id |> map_dfr(find_controls)
    # browser()
    if (nrow(new_matches) > 0) {
      break
    }
    iteration <- iteration + 1
  }

  # If no new matches were found, return the original data
  if (nrow(new_matches) == 0) {
    warning("No new matches found after max_iterations. Returning original data.")
    return(dat)
  }

  # Combine the original data with the new matches
  combined_data <- bind_rows(dat, new_matches)

  return(combined_data)



# testing area ------------------------------------------------------------





# count the number of unique cases and controls
matched_data |>
  summarise(
    n_cases = n_distinct(case_id),
    n_controls = n_distinct(control_id)
  )

# find if any case is listed more than `ratio` times
matched_data |>
  group_by(case_id) |>
  summarise(n = n()) |>
  filter(n > ratio) |>
  nrow()

# find any cases that were not matched
case |>
  filter(!idVar %in% matched_data$case_id) |>
  nrow()

# find any control that were not matched
control |>
  filter(!idVar %in% matched_data$control_id) |>
  nrow()

# find if any cases were matched only once
matched_data |>
  group_by(case_id) |>
  summarise(n = n()) |>
  filter(n == 1) |>
  nrow()

# find if any controls were matched more than once
matched_data |>
  group_by(control_id) |>
  summarise(n = n()) |>
  filter(n > 1) |>
  nrow()






# original working function -----------------------------------------------


mitter_match_original <- function(dat, numRange, ratio) {
  # Split the data into cases and controls
  case <- dat |>
    dplyr::filter(event == 1)
  control <- dat |>
    dplyr::filter(event == 0) |>
    mutate(control_usage = 0)  # Add control_usage as a column

  # Initialize usage counts for cases
  case_usage <- rep(0, nrow(case))

  # Define a function to check if there are any possible matches for a case
  check_possible_matches <- function(idVar, age, gender, ethnicity, event, control) {
    # Create a tibble for the case row
    case_row <- tibble(
      case_id = idVar,
      age = age,
      gender = gender,
      ethnicity = ethnicity,
      event = event
    )

    possible_controls <- control |>
      filter(
        ethnicity == case_row$ethnicity,
        age >= case_row$age - numRange,
        age <= case_row$age + numRange,
        control_usage <= 1  # Only consider controls that have been used once or not at all
      )

    # Return TRUE if there are any possible controls, FALSE otherwise
    has_match <- nrow(possible_controls) > 0
    if (is.na(has_match)) {
      FALSE
    } else {
      has_match
    }
  }

  # Define a function to find two controls for each case and return a single row
  # that includes the case and its matched controls
  find_controls <- function(idVar, age, gender, ethnicity, event) {
    # Create a tibble for the case row
    case_row <- tibble(
      case_id = idVar,
      age = age,
      gender = gender,
      ethnicity = ethnicity,
      event = event
    )

    # Check if there are any possible matches for this case
    case_usage_count <- ifelse(!is.na(case_usage[case_row$case_id]), case_usage[case_row$case_id], 0)
    if (check_possible_matches(idVar, age, gender, ethnicity, event, control) && case_usage_count < ratio) {
      matched_controls <- control |>
        filter(
          ethnicity == case_row$ethnicity,
          age >= case_row$age - numRange,
          age <= case_row$age + numRange,
          control_usage <= 1  # Only consider controls that have been used once or not at all
        ) |>
        mutate(random_num = runif(n(), 0, 1)) |>
        arrange(random_num) |>
        sample_n(size = min(n(), ratio)) |>
        rename(
          control_id = idVar,
          control_age = age,
          control_gender = gender,
          control_ethnicity = ethnicity,
          control_event = event
        )

      # Update usage counts
      case_usage[case_row$case_id] <<- case_usage[case_row$case_id] + 1
      control <<- control |>
        mutate(control_usage = ifelse(idVar %in% matched_controls$control_id, control_usage + 1, control_usage))

      # Remove the matched controls from the controls data frame
      control <<- control |>
        filter(!idVar %in% matched_controls$control_id)

      # Combine the case and its matched controls into a single row
      combined_row <- bind_cols(case_row, matched_controls)

      combined_row
    } else {
      # If there are no possible matches, return NULL
      NULL
    }
  }

  # Initialize matched_data as an empty tibble
  matched_data <- tibble()

  # While there are still unmatched cases, try to find matches
  while(nrow(case) > 0) {
    # Try to find matches for each case
    new_matches <- pmap_dfr(case, find_controls)

    # If no new matches were found, break the loop
    if(nrow(new_matches) == 0) {
      break
    }

    # Add the new matches to the matched data
    matched_data <- bind_rows(matched_data, new_matches)

    # Remove the matched cases from the cases data frame
    case <- case |>
      filter(!idVar %in% new_matches$case_id)
  }

  # Remove NULL rows
  matched_data <- matched_data[!is.na(matched_data$case_id), ]

  matched_data
}

mitter_match_original(
  dat = dat,
  numRange = numRange,
  ratio = ratio
)









