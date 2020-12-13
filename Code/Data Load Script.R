### The purpose of this script is to load and clean all of the various
# .csv files containing polling place data into R.

# Start by loading packages
pacman::p_load(tidyverse,
               here,
               stringr,
               readr,
               praise,
               SmartEDA,
               stringi)

# The Data folder has 32 states represented in it, so we'll need to read
# each one in

# Determine the different state folders in our dataset and remove any
# already existing csv files
all_files <- list.files(here("Data/"))
csv_files <- list.files(here("Data/"), pattern = ".csv$")
states <- setdiff(all_files, csv_files)

# Initialize an empty dataframe
polling_places <- data.frame(state_name = character(),
                             state = character(),
                             election_date = as.Date(character()),
                             jurisdiction_type = character(),
                             jurisdiction = character(),
                             county_name = character(),
                             precinct_name = character(),
                             precinct_id = character(),
                             polling_place_type = character(),
                             municipality = character(),
                             name = character(),
                             address = character(),
                             year = double(),
                             stringsAsFactors=FALSE)

csvs <- vector()

# Start with the outer loop, which goes through all of the state folders
for (state_ in states) {
  print(state_)
  # Within each data folder, there are multiple .csv files, one for each election
  # Let's pull out each of these files
  state_contents <- list.files(paste(here("Data/"), state_, sep = "/"))
  cat("About to parse through the following files in the", state_, "folder:\n",
      state_contents)

  # Let's loop through each of the states to read and add our .csv files in
  for (i in state_contents) {
    # print(i)
    # If the file is a csv file, then let's read it in
    if (str_detect(i, ".csv") ) {
      print("Parsing...")
      # cat("About to read in", i, "\n")
      csvs <- c(csvs, i)

      # Find the file path name for the csv
      csv_name <- paste("Data", state_, i, sep = "/")
      # print(paste("File is located here:", csv_name))

      # Let's read in the file as a temporary dataframe
      temp_df <- read_csv(csv_name, col_types = cols(
        .default = col_character(),
        election_date = col_date(format = ""),
        state = col_character(),
        jurisdiction = col_character(),
        county_name = col_character(),
        municipality = col_character(),
        jurisdiction_type = col_character(),
        polling_place_type = col_character(),
        precinct_id = col_character(),
        precinct_name = col_character(),
        name = col_character(),
        address = col_character()
      ))

      # We want the state name from earlier to be brought in as the column
      # temp_df$state_name <- "NJ"
      temp_df <- temp_df %>%
        mutate(state_name = state_,
               year = as.integer(str_extract(i, "\\d+"))
        )

      # Lastly, let's append this to the bottom of our master dataset
      polling_places <- bind_rows(polling_places, temp_df)
    }
  }
  cat("===========================\n===========================\n")
}

# WE'RE DONE. THANK MOSES.
cat(praise(), "WE'RE DONE!!!")


#### Data Cleaning ####
# Now we'll clean up our data so it's all interpretable
polling_cleaned <- polling_places %>%
  # Tibbles > Dataframes
  as_tibble() %>%
  # Make sure we only have distinct rows. It looks like there was some data
  # entry duplication, so let's get rid of that
  distinct() %>%
  mutate(
    # Replace the "_" in the state names with a space so it's more readable
    state = str_replace(state_name, "_", " "),
    # It looks like polling_place_type and polling_location_type are the
    # same thing, but must've been shared across two separate datasets
    polling_place_type = coalesce(polling_location_type, polling_place_type),
    # Let's get zip code out of the mailing address, using the stringi package
    zip_code = as.numeric(stri_extract_last_regex(address, "[[:digit:]]{5}")),
    county_fips = as.numeric(county_fips),
    ward = as.numeric(ward)
  ) %>%
  # Remove notes since that only has one distinct value
  select(-c(notes, ghost_precinct, state_name, polling_location_type, polling_place_id, ward)) %>%
  # State came in super weird, so let's replace this with state_abbreviation
  left_join(tibble(state = state.name, state_abb = state.abb), by = "state") %>%
  print()


# Let's save our final dataset, now that it's fully combined and cleaned
readr::write_csv(polling_cleaned, "Data/all_states_cleaned.csv")
