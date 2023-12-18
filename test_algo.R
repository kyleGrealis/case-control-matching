# working out the matching algorithm
library(tidyverse)

# simulated dataset and inputs
dat <- rio::import("~/Downloads/test_set.sas7bdat")
idVariable <- dat$participant_id
caseControl <- dat$event
numericVariable <- dat$age
numVarRange <- 1
categoricalVariable <- dat$gender
thirdVariable <- dat$ethnicity

# split the dataset into cases and controls
case <- dat |>
  dplyr::filter(event == 1)
control <- dat |>
  dplyr::filter(event == 0)

# match on age and other matching variable
control_range <- control |>
  # create the low-high range for the numeric variable
  mutate(
    num_low  = numericVariable - numVarRange,
    num_high = numericVariable + numVarRange
  )
