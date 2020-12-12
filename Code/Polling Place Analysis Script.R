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
female_politicians_data <- readr::read_csv(here::here("Data/women_in_politics.csv"))
census_data <- readr::read_csv(here::here("Data/Census_Structured.csv"))

# Convert to a tibble, my preferred data structure
(female_politicians_data <- as_tibble(female_politicians_data))
