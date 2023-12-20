
# algorithm setup ---------------------------------------------------------
# working out the matching algorithm
library(dplyr)
library(purrr)

# simulated dataset and inputs
dat <- rio::import("~/Downloads/test_set.sas7bdat")
idVariable <- rlang::sym("participant_id")
caseControl <- rlang::sym("event")
numericVariable <- rlang::sym("age")
numRange <- 2
categoricalVariable <- rlang::sym("gender")
thirdVariable <- rlang::sym("ethnicity")
ratio <- 1


# split the dataset -------------------------------------------------------
case <- dat |>
  dplyr::filter(event == 1)
control <- dat |>
  dplyr::filter(event == 0)


# using the copilot suggestion --------------------------------------------
# this one works
# mitter_match <- function(dat, numRange, ratio) {
#   # Split the data into cases and controls
#   case <- dat |>
#     dplyr::filter(event == 1)
#   control <- dat |>
#     dplyr::filter(event == 0)
#
#   # Define a function to find two controls for each case and return a single row
#   # that includes the case and its matched controls
#   find_controls <- function(participant_id, age, gender, ethnicity, event) {
#     # Create a tibble for the case row
#     case_row <- tibble(
#       case_id = participant_id,
#       age = age,
#       gender = gender,
#       ethnicity = ethnicity,
#       event = event
#     )
#
#     matched_controls <- control |>
#       filter(
#         ethnicity == case_row$ethnicity,
#         age >= case_row$age - numRange,
#         age <= case_row$age + numRange
#       ) |>
#       sample_n(size = min(n(), ratio)) |>
#       rename(
#         control_id = participant_id,
#         control_age = age,
#         control_gender = gender,
#         control_ethnicity = ethnicity,
#         control_event = event
#       )
#
#     # Remove the matched controls from the controls data frame
#     control <<- control |>
#       filter(!participant_id %in% matched_controls$control_id)
#
#     # Combine the case and its matched controls into a single row
#     combined_row <- bind_cols(case_row, matched_controls)
#
#     combined_row
#   }
#
#   # Apply the function to each case
#   matched_data <- pmap_dfr(case, find_controls)
#
#   matched_data
# }


# second copilot ----------------------------------------------------------
# this one does work
# mitter_match <- function(dat, numRange, ratio) {
#   # Split the data into cases and controls
#   case <- dat |>
#     dplyr::filter(event == 1)
#   control <- dat |>
#     dplyr::filter(event == 0)
#
#   # Define a function to find two controls for each case and return a single row
#   # that includes the case and its matched controls
#   find_controls <- function(participant_id, age, gender, ethnicity, event) {
#     # Create a tibble for the case row
#     case_row <- tibble(
#       case_id = participant_id,
#       age = age,
#       gender = gender,
#       ethnicity = ethnicity,
#       event = event
#     )
#
#     matched_controls <- control |>
#       filter(
#         ethnicity == case_row$ethnicity,
#         age >= case_row$age - numRange,
#         age <= case_row$age + numRange
#       ) |>
#       mutate(random_num = runif(n(), 0, 1)) |>
#       arrange(random_num) |>
#       sample_n(size = min(n(), ratio)) |>
#       rename(
#         control_id = participant_id,
#         control_age = age,
#         control_gender = gender,
#         control_ethnicity = ethnicity,
#         control_event = event
#       )
#
#     # Remove the matched controls from the controls data frame
#     control <<- control |>
#       filter(!participant_id %in% matched_controls$control_id)
#
#     # Combine the case and its matched controls into a single row
#     combined_row <- bind_cols(case_row, matched_controls)
#
#     combined_row
#   }
#
#   # Apply the function to each case
#   matched_data <- pmap_dfr(case, find_controls)
#
#   matched_data
# }


# third copilot with logic check within -----------------------------------

mitter_match <- function(dat, numRange, ratio) {
  # Split the data into cases and controls
  case <- dat |>
    dplyr::filter(event == 1)
  control <- dat |>
    dplyr::filter(event == 0) |>
    mutate(control_usage = 0)  # Add control_usage as a column

  # Initialize usage counts for cases
  case_usage <- rep(0, nrow(case))

  # Define a function to check if there are any possible matches for a case
  check_possible_matches <- function(participant_id, age, gender, ethnicity, event, control) {
    # Create a tibble for the case row
    case_row <- tibble(
      case_id = participant_id,
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
  find_controls <- function(participant_id, age, gender, ethnicity, event) {
    # Create a tibble for the case row
    case_row <- tibble(
      case_id = participant_id,
      age = age,
      gender = gender,
      ethnicity = ethnicity,
      event = event
    )

    # Check if there are any possible matches for this case
    case_usage_count <- ifelse(!is.na(case_usage[case_row$case_id]), case_usage[case_row$case_id], 0)
    if (check_possible_matches(participant_id, age, gender, ethnicity, event, control) && case_usage_count < ratio) {
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
          control_id = participant_id,
          control_age = age,
          control_gender = gender,
          control_ethnicity = ethnicity,
          control_event = event
        )

      # Update usage counts
      case_usage[case_row$case_id] <<- case_usage[case_row$case_id] + 1
      control <<- control |>
        mutate(control_usage = ifelse(participant_id %in% matched_controls$control_id, control_usage + 1, control_usage))

      # Remove the matched controls from the controls data frame
      control <<- control |>
        filter(!participant_id %in% matched_controls$control_id)

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
      filter(!participant_id %in% new_matches$case_id)
  }

  # Remove NULL rows
  matched_data <- matched_data[!is.na(matched_data$case_id), ]

  matched_data
}


# adding more flexibility -------------------------------------------------

mitter_match <- function(dat, numRange, ratio, caseControl, numericVariable, categoricalVariable, thirdVariable = NULL) {
  # Split the data into cases and controls
  case <- dat |>
    dplyr::filter(!!sym(caseControl) == 1)
  control <- dat |>
    dplyr::filter(!!sym(caseControl) == 0) |>
    mutate(control_usage = 0)  # Add control_usage as a column

  # Initialize usage counts for cases
  case_usage <- rep(0, nrow(case))

  # Define a function to check if there are any possible matches for a case
  check_possible_matches <- function(participant_id, ...) {
    # Create a tibble for the case row
    case_row <- tibble(
      case_id = participant_id,
      !!numericVariable := !!sym(numericVariable),
      !!categoricalVariable := !!sym(categoricalVariable),
      event = !!sym(caseControl)
    )

    # Add thirdVariable if it's not NULL
    if (!is.null(thirdVariable)) {
      case_row <- case_row |>
        mutate(!!thirdVariable := !!sym(thirdVariable))
    }

    possible_controls <- control |>
      filter(
        !!sym(categoricalVariable) == case_row[[categoricalVariable]],
        !!sym(numericVariable) >= case_row[[numericVariable]] - numRange,
        !!sym(numericVariable) <= case_row[[numericVariable]] + numRange,
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
  find_controls <- function(participant_id, case, numericVariable, categoricalVariable, thirdVariable) {
    # Get the case row
    case_row <- case[case$participant_id == participant_id, ]

    # Access the numericVariable and categoricalVariable columns in the case_row data frame
    numeric_value <- case_row[[numericVariable]]
    categorical_value <- case_row[[categoricalVariable]]

    # Add thirdVariable if it's not NULL
    if (!is.null(thirdVariable)) {
      case_row <- case_row |>
        mutate(!!thirdVariable := !!sym(thirdVariable))
    }

    # Check if there are any possible matches for this case
    case_usage_count <- ifelse(!is.na(case_usage[case_row$case_id]), case_usage[case_row$case_id], 0)
    if (check_possible_matches(participant_id, age = case_row[[numericVariable]], gender = case_row[[categoricalVariable]], event = case_row$event, control) && case_usage_count < ratio) {
      matched_controls <- control |>
        filter(
          !!sym(categoricalVariable) == case_row[[categoricalVariable]],
          !!sym(numericVariable) >= case_row[[numericVariable]] - numRange,
          !!sym(numericVariable) <= case_row[[numericVariable]] + numRange,
          control_usage <= 1  # Only consider controls that have been used once or not at all
        ) |>
        mutate(random_num = runif(n(), 0, 1)) |>
        arrange(random_num) |>
        sample_n(size = min(n(), ratio)) |>
        rename(
          control_id = participant_id,
          control_age = !!sym(numericVariable),
          control_gender = !!sym(categoricalVariable),
          control_event = !!sym(caseControl)
        )

      # Update usage counts
      case_usage[case_row$case_id] <<- case_usage[case_row$case_id] + 1
      control <<- control |>
        mutate(control_usage = ifelse(participant_id %in% matched_controls$control_id, control_usage + 1, control_usage))

      # Remove the matched controls from the controls data frame
      control <<- control |>
        filter(!participant_id %in% matched_controls$control_id)

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
    new_matches <- purrr::map_dfr(case$participant_id, find_controls, case = case, numericVariable = numericVariable, categoricalVariable = categoricalVariable, thirdVariable = thirdVariable)

    # If no new matches were found, break the loop
    if(nrow(new_matches) == 0) {
      break
    }

    # Add the new matches to the matched data
    matched_data <- bind_rows(matched_data, new_matches)

    # Remove the matched cases from the cases data frame
    case <- case |>
      filter(!participant_id %in% new_matches$case_id)
  }

  # Remove NULL rows
  matched_data <- matched_data[!is.na(matched_data$case_id), ]

  matched_data
}


# testing area ------------------------------------------------------------



# Apply the function
matched_data <- mitter_match(
  dat,
  caseControl = "event",
  numericVariable = "age",
  numRange = 2,
  categoricalVariable = "gender",
  thirdVariable = NULL,
  ratio = 1
)


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
  filter(!participant_id %in% matched_data$case_id) |>
  nrow()

# find any control that were not matched
control |>
  filter(!participant_id %in% matched_data$control_id) |>
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























# END OF COPILOT SUGGESTION -----------------------------------------------
mitter_match <- function(dat, numRange) {
  # Split the data into cases and controls
  case <- dat |>
    dplyr::filter(event == 1)
  control <- dat |>
    dplyr::filter(event == 0)

  # match on age and one other matching variable
  control_range <- control |>
    # create the low-high range for the numeric variable
    mutate(
      num_low  = age - numRange,
      num_high = age + numRange
    )

  # join the case and control datasets. This will create a dataset with all
  # possible matches. It's a large dataset. The test data has 33069 rows
  cases_and_avail_matches <- case |>
    left_join(
      control_range,
      by = c("gender"),
      suffix = c("_case", "_control")
    ) |>
    # filter the dataset keep matches where the case's num var is within the range
    filter(between(age_case, num_low, num_high)) |>
    select(
      case_id = participant_id_case,
      control_id = participant_id_control,
      case_age = age_case,
      control_age = age_control,
      gender
    ) |>
    # count the number of control subjects for each case subject
    arrange(case_id) |>
    group_by(case_id) |>
    mutate(available_controls = n()) |>
    ungroup()

  # Define a function to find two controls for each case and return a single row
  # that includes the case and its matched controls
  find_controls <- function(case_id, age, gender, ethnicity, event) {
    # Create a tibble for the case row
    case_row <- tibble(
      case_id = case_id,
      age = age,
      gender = gender,
      ethnicity = ethnicity,
      event = event
    )

    matched_controls <- cases_and_avail_matches |>
      filter(case_id == case_row$case_id) |>
      sample_n(size = min(n(), 2)) |>
      rename(
        control_id = control_id,
        control_age = control_age,
        control_gender = gender,
        control_ethnicity = ethnicity,
        control_event = event
      )

    # Remove the matched controls from the controls data frame
    control <<- control |>
      filter(!participant_id %in% matched_controls$control_id)

    # Combine the case and its matched controls into a single row
    combined_row <- bind_cols(case_row, matched_controls)

    combined_row
  }

  # Apply the function to each case
  matched_data <- pmap_dfr(case, find_controls)

  matched_data
}



# another copilot suggestion ----------------------------------------------






# this is only for matching on 1 categorical variable ---------------------
# match on age and one other matching variable
control_range <- control |>
  # create the low-high range for the numeric variable
  mutate(
    num_low  = age - numRange,
    num_high = age + numRange
  )

# join the case and control datasets. This will create a dataset with all
# possible matches. It's a large dataset. The test data has 33069 rows
cases_and_avail_matches <- case |>
  left_join(
    control_range,
    by = c("gender"),
    suffix = c("_case", "_control")
  ) |>
  # filter the dataset keep matches where the case's num var is within the range
  filter(between(age_case, num_low, num_high)) |>
  select(
    case_id = participant_id_case,
    control_id = participant_id_control,
    case_age = age_case,
    control_age = age_control,
    gender
  ) |>
# counting and first matching ---------------------------------------------
# Order the control subjects by the number of matches they have with the case
# subjects. Then keep the matches for the low frequency control subjects first.

# count the number of control subjects for each case subject
  arrange(case_id) |>
  group_by(case_id) |>
  mutate(available_controls = n()) |>
  ungroup()

head(cases_and_avail_matches)

# NOTE: this is good through line 217 in SAS code!



# iterating matching algorithm --------------------------------------------
# Evaluate the matching process by repeating the matching algorithm. Keep the
# dataset with the maximum number of rows. This helps to eliminate bias by
# finding the dataset with the best matching attributes.

# this will eventually be looped 100 times
loop <- 1

# this will create a dataset by randomly assigning a number to each pair, arranging
# the pairs by the number of available matches and the random number, and then
# keeping the first match for each control_id. this will be done 100 times.
random <- cases_and_avail_matches |>
  mutate(
    iteration = loop,
    # create a random number for each case-control pair
    random_num = runif(n(), 0, 1)
  ) |>
  # arrange the data now by prioritizing controls. this will keep the controls
  # with the fewest matches available first. then, it will prioritize the random
  # number.
  arrange(control_id, available_controls, random_num)

head(random)

# keep the first match by the control_id
take_first_controls <- random |>
  group_by(control_id) |>
  slice(1) |>
  ungroup() |>
  arrange(case_id, random_num)

head(take_first_controls)
# SAS line 242

# these are all of the possible matches for this iteration. the number of rows
# in this dataset will always change because of the random number used to group
# the cases and controls above. this is main reason why the algorithm will
# iterate 100 times and keep the dataset with the most rows.
matched_groups <- take_first_controls |>
  # group by case_id and give then count the number of times each case_id appears
  group_by(case_id) |>
  mutate(possible_matches = n()) |>
  mutate(num = row_number()) |>
  slice(1:ratio) |>
  # mutate(case_count = n()) |>
  ungroup() #|>
  # arrange(case_count) |>
  # mutate(controls_needed = ratio - case_count) |>
  # arrange(desc(controls_needed)) |>
  # select(-c(available_controls, random_num))

head(matched_groups)

take_first_controls |>
  filter(
    !case_id %in% matched_groups$case_id,
    !control_id %in% matched_groups$control_id
  )

matched_groups |>
  summarize(
    total_cases = n_distinct(case_id),
    total_controls = n_distinct(control_id),
    total_used = n_distinct(case_id) + n_distinct(control_id)
  )


random <- random |>
  group_by(case_id) |>
  mutate(case_cout = row_number()) |>
  ungroup() |>
  arrange(case_id)
