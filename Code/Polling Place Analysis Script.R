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
               "here", # Used for navigating project structure
               "maps", # Used for map-based visualizations
               "readxl", # Used for loading excel files,
               "readr", # Used for working with files,
               "pander", # Used for pretty tables,
               "kableExtra", # Used for RMarkdown formatting
               "lubridate", # Used for fixing dates,
               "usmap", # Used for plotting US states
               "praise", # Used for positive reinforcement,
               "janitor", # Used for data cleaning,
               "pdftools", # Used for reading PDF files in,
               "gganimate", # Used for interactive graphic visualizations,
               "gridExtra", # Used for putting multiple plots next to each other
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
  group_by(state, state_abb, year, population) %>%
  summarise(polling_sites = n()) %>%
  mutate(
    # Calculate our percent change YoY
    percent_change = round(100*((polling_sites - lag(polling_sites)) / lag(polling_sites)), 2),
    # Pull in our population data to calculate the per capita rate of polling places
    ps_per_capita = round(1000000 * polling_sites / population, 0)
    ) %>%
  print()

# Now that we have our dataset, let's build 4 scatter plots (one for each year)
# and look for any outliers.
pp_scatter <- pp_counts %>%
ggplot(aes(x = polling_sites, y = population)) +
  # Make it a scatter plot
  geom_point(color = "slateblue", alpha = .8, size = 2) +
  geom_text(aes(label = state), # label by state
            color = "slateblue", # Make our color match
            size = 4, # shrink the size
            alpha = .9, # add some transparency
            check_overlap = T, # avoid overlabelling
            nudge_y = 500000) + # nudge the text a bit off center
  theme_classic() +
  # Create separate scatter plots for each year
  facet_wrap(~ year) +
  # Let's change the names of the axes and title
  labs(title = "Number of Polling Places by Population",
       subtitle = paste("*Data broken out across", n_distinct(pp_counts$state), "states,",
                        "represents the number of polling places per 1,000,000 people."),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                       "https://github.com/publici/us-polling-places")
       ) +
  xlab("Number of Polling Sites") +
  ylab("Population") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"))

ggsave(here("Viz/Polling Places Scatter Plot.jpg"), pp_scatter)

# Let's try two more visualizations to help look for patterns
# Start by creating a dataframe of our top 10 states per year
top_10_pp <- pp_counts %>%
  # Group by level and state
  group_by(year) %>%
  # Pick our top 10
  top_n(10, ps_per_capita) %>%
  # Rearrange our dataset
  arrange(year, desc(ps_per_capita)) %>%
  ungroup() %>%
  print()


pp_bar <- top_10_pp %>%
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = ps_per_capita,
             # Reorder the variable so they are arrange descending for each year plot
             y = forcats::fct_reorder(paste(state, year, sep = "_"), ps_per_capita)
             )) +
  geom_bar(stat = "identity", fill = "slateblue", na.rm = T) +
  # Create a separate chart, with a flexible y-axis, for each level of office
  facet_wrap(~year, scales = "free_y") +
  # Add a label by recreating our data build from earlier
  geom_label(aes(label = paste(state_abb, ps_per_capita, sep = "-")),
             size = 3,
             # Scooch the labels over a smidge
             hjust = .25) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Polling Sites per Capita*") +
  ylab("States") +
  labs(title = "Number of Polling Places by Population",
       subtitle = paste("*Data broken out across", n_distinct(pp_counts$state), "states,",
                        "represents the number of polling places per 1,000,000 people."),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                       "https://github.com/publici/us-polling-places")
  ) +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(here("Viz/Top Polling Places Bar Chart.jpg"), pp_bar)


# Let's take a look at the same data on a map to look for differences
# geographically.
pp_map <- plot_usmap(data = pp_counts,
           values = "ps_per_capita",
           color = "gray",
           # Only include states in our dataset
           include = unique(pp_counts$state_abb)) +
  scale_fill_continuous(name = "Polling Sites per Capita",
                        label = scales::comma,
                        low = "white",
                        high = "slateblue",
                        # Manually set the limits so its easier to see differences
                        limits = c(0, 1500)) +
  facet_wrap(~ year, nrow = 1) +
  labs(# title = "Number of Polling Places by Population",
       # subtitle = paste("*Data broken out across", n_distinct(pp_counts$state), "states,",
       #                  "represents the number of polling places per 1,000,000 people."),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                       "https://github.com/publici/us-polling-places")
  ) +
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

pp_bar_one_row <- top_10_pp %>%
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = ps_per_capita,
             # Reorder the variable so they are arrange descending for each year plot
             y = forcats::fct_reorder(paste(state, year, sep = "_"), ps_per_capita)
  )) +
  geom_bar(stat = "identity", fill = "slateblue", na.rm = T) +
  # Create a separate chart, with a flexible y-axis, for each level of office
  facet_wrap(~year, scales = "free_y", nrow = 1) +
  # Add a label by recreating our data build from earlier
  geom_label(aes(label = paste(state_abb, ps_per_capita, sep = "-")),
             size = 3,
             # Scooch the labels over a smidge
             hjust = .25) +
  # Change the theme to classic
  theme_classic() +
  # Let's change the names of the axes and title
  xlab("Polling Sites per Capita*") +
  ylab("States") +
  labs(title = "Number of Polling Places by Population",
       subtitle = paste("*Data broken out across", n_distinct(pp_counts$state), "states,",
                        "represents the number of polling places per 1,000,000 people.")
  ) +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

combined_pp_plot <- grid.arrange(pp_bar_one_row, pp_map)
combined_pp_plot

ggsave(here("Viz/Top Polling Places Bar and Map.jpg"), combined_pp_plot)

