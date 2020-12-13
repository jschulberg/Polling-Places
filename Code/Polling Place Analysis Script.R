###########################################################################
###########################################################################

#######                    Polling Places Analysis                  #######

###########################################################################
###########################################################################
# In this script, I will analyze the a dataset of U.S. polling places included
# in the data folder of this repository. I will use a variety of exploratory
# and modeling techniques to answer the following questions:

# Which states provide the most polling places?
# How do these compare to population centers in each state?
# How have the number of polling places trended over time?


###########################################################################
## Set Up -----------------------------------------------------------------
###########################################################################
# Bring in packages
suppressMessages(library("pacman"))
pacman::p_load("tidyverse", # Used for data wrangling,
               "tidyr", # Used for data cleaning,
               "ggplot2", # Used for visualizations,
               "maps", # Used for map-based visualizations
               "readxl", # Used for loading excel files,
               "readr", # Used for working with files,
               "pander", # Used for pretty tables,
               "kableExtra", # Used for RMarkdown formatting
               "lubridate", # Used for fixing dates,
               "praise", # Used for positive reinforcement,
               "janitor", # Used for data cleaning,
               "pdftools", # Used for reading PDF files in,
               "gganimate", # Used for interactive graphic visualizations,
               "mapproj", # Used for visualizing maps
               "transformr", # Used to animate maps
               "gifski", # Used to create animated gifs
               "forecast", # Used for time series analysis,
               "tseries")  # Used for time series analysis


# Bring in the data, taking advantage of the project structure
# Our base dataset
polling_places <- readr::read_csv(here::here("Data/all_states_cleaned.csv"))
census_data <- readr::read_csv(here::here("Data/Census_Structured.csv"))


# Convert to a tibble, my preferred data structure
(polling_places <- as_tibble(polling_places))


########################################################################
## Wrangle Data --------------------------------------------------------
########################################################################
# Let's start by bringing our census data in
polling_joined <- polling_places %>%
  left_join(census_data, by = c("state", "year")) %>%
  select(-state_code) %>%
  print()


########################################################################
## Viz Time ------------------------------------------------------------
########################################################################
# Let's start by finding our 10 states which have the most polling
# places in each year, normalized by population
pp_counts <- polling_joined %>%
  group_by(state, year, population) %>%
  summarise(polling_sites = n()) %>%
  mutate(
    # Calculate our percent change YoY
    percent_change = round(100*((polling_sites - lag(polling_sites)) / lag(polling_sites)), 2),
    # Pull in our population data to calculate the per capita rate of polling places
    ps_per_capita = 1000000 * polling_sites / population
    ) %>%
  print()

# Now that we have our dataset, let's build 4 scatter plots (one for each year)
# and look for any outliers.
pp_counts %>%
ggplot(aes(x = polling_sites, y = population)) +
  # Make it a scatter plot
  geom_point(color = "slateblue", alpha = .8) +
  geom_text(aes(label = state), # label by state
            color = "slateblue", # Make our color match
            size = 3, # shrink the size
            alpha = .8, # add some transparency
            check_overlap = T) # avoid overlabelling
            # nudge_y = 150) + # nudge the text a bit off center
  theme_classic() +
  # Create separate scatter plots for each year
  facet_wrap(~ year) +
  # Let's change the names of the axes and title
  labs(title = "Number of Polling Places by Population",
       subtitle = paste("Data is broken out across", n_distinct(pp_counts$state), "states."),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                     "https://github.com/publici/us-polling-places")
       ) +
  xlab("Number of Polling Sites") +
  ylab("Population") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"))
