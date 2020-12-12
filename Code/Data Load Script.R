### The purpose of this script is to load all of the various .csv files
# containing polling place data into R.

# Start by loading packages
pacman::p_load(tidyverse,
               here)

# The Data folder has 32 states represented in it, so we'll need to read
# each one in

read_state_files <- function(folder) {
  # Determine the state name
  states <- list.files(path = folder)

  # Initialize an empty datafra,e
  df <- data.frame(state_name = character(),
                   state = character(),
                   election_date
                   jurisdiction_type = character(),
                     double(),
                   Ints=integer(),
                   Factors=factor(),
                   Logicals=logical(),
                   Characters=character(),
                   stringsAsFactors=FALSE)

  # Within each data folder, there are multiple .csv files, one for each election
  # Let's loop through each of the states to read and add our .csv files in
  }

