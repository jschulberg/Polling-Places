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
               "ggrepel", # Used for labeling points in a ggplot viz
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
elections_data <- readr::read_csv(here::here("Data/Elections Data.csv"))


# Convert to a tibble, my preferred data structure
(polling_places <- as_tibble(polling_places))


########################################################################
## Wrangle Data --------------------------------------------------------
########################################################################
# Let's start by bringing our census and state elections data in
polling_joined <- polling_places %>%
  left_join(census_data, by = c("state", "year")) %>%
  left_join(elections_data, by = c("state_code", "year")) %>%
  select(-state_code) %>%
  # Create new columns based on the election data and filter out empty years
  mutate(total_vote = democratic_votes + republican_votes + other_votes) %>%
  filter(!is.na(total_vote)) %>%
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
  geom_text_repel(aes(label = state)) +
  theme_classic() +
  # Create separate scatter plots for each year
  facet_wrap(~ year) +
  # Let's change the names of the axes and title
  labs(title = "Number of Polling Places by Population",
       subtitle = paste("*Data is broken out across", n_distinct(pp_counts$state), "states."),
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
       subtitle = paste("*Data, broken out across", n_distinct(pp_counts$state), "states,",
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
                        limits = c(0, 3750)) +
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


########################################################################
## Elections Correlation -----------------------------------------------
########################################################################
# In this next section, I'd like to leverage elections data to look for trends
# in the number of polling places. The elections data has been pulled from
# [FEC.gov](https://www.fec.gov/introduction-campaign-finance/election-and-voting-information/)
# In the data, I look at the breakdown, by party, of the House, Senate, and Presidential
# races and the total number of votes for each in every general election between
# 2012 and 2018, to match the polling places data.

# Let's start by getting the dataset into a state-wide form.
state_results <- polling_joined %>%
  group_by(state, state_abb, year, population, election, democratic_votes,
           republican_votes, other_votes, total_vote) %>%
  summarise(polling_sites = n(),
            # Determine which party won each election
            party_winner = case_when(
              democratic_votes > republican_votes && democratic_votes > other_votes ~ "Democrat",
              republican_votes > democratic_votes && republican_votes > other_votes ~ "Republican",
              other_votes > republican_votes && other_votes > democratic_votes ~ "Third Party",
              TRUE ~ "ERROR!!!!"
            )) %>%
  ungroup() %>%
  mutate(
    # Pull in our population data to calculate the per capita rate of polling places
    ps_per_capita = round(1000000 * polling_sites / population, 0),
    # Calculate the % share of each party
    democratic_share = 100*round(democratic_votes / total_vote, 2),
    republican_share = 100*round(republican_votes / total_vote, 2),
    other_share = 100*round(other_votes / total_vote, 2)
  ) %>%
  arrange(state, year) %>%
  print()


# I'd also like to figure out who "owns" a state. That is, if Democrats won
# House, Senate, and President, they "own" that state. However, if Democrats
# only won House and Senate, but not President, the ownership is mixed
party_triumphs <- state_results %>%
  group_by(state, year) %>%
  summarise(elections = n()) %>%
  ungroup() %>%
  left_join(state_results) %>%
  group_by(state, year, elections, party_winner) %>%
  summarise(party_triumphs = n()) %>%
  ungroup() %>%
  mutate(percent_control = party_triumphs / elections,
         # If percent control is 1, the party swept the election. Otherwise,
         # it's a mixed result
         owner = case_when(
           percent_control < 1 ~ "Split Ticket",
           party_winner == "Republican" ~ "Republican-controlled",
           party_winner == "Democrat" ~ "Democrat-controlled",
           party_winner == "Third Party" ~ "Third Party-controlled",
           TRUE ~ "ERROR!!! ABORT!!!!!"
         )) %>%
  arrange(state, year) %>%
  print()

# Bring this data back into state_results
state_winners <- party_triumphs %>%
  select(state, year, elections, owner) %>%
  right_join(state_results, by = c("state", "year")) %>%
  distinct() %>%
  print()

# Create a dataframe of party colors that we can use for our visualizations
party_colors <- tibble(
  party_colors = c("#2E74C0", "#CB454A", "#999999"),
  owner = c("Democrat-controlled", "Republican-controlled", "Split Ticket")
)

# First, let's take a look at which party won each state over the past four
# election cycles: Democrats, Republicans, Third Party, or Split Ticket (i.e.
# Dems won the House and Republicans the Senate + Presidency)
election_map <- plot_usmap(data = state_winners,
                     values = "state_abb",
                     # color = state_winners$owner,
                     # Only include states in our dataset
                     include = unique(state_winners$state_abb)) +
  scale_fill_manual(
    name = "Party Winner of Federal Elections",
    values = c("#2E74C0", "#CB454A", "#999999"),
    labels = c("Democrat-controlled", "Republican-controlled", "Split Ticket")
  ) +
  facet_wrap(~ year) +
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
election_map


# Now that we have our dataset, let's build 4 scatter plots (one for each year)
# of polling places by number of total votes
votes_scatter <- state_winners %>%
  group_by(state, state_abb, year,  owner, polling_sites, population) %>%
  # There's a lot of issues with me doing this, but I'll keep it for now
  summarise(cumulative_vote = sum(total_vote)) %>%
  ggplot(aes(x = polling_sites, y = cumulative_vote, color = owner)) +
  geom_point(alpha = .8, size = 2) +
  geom_text_repel(aes(label = state)) +
  scale_color_manual(
    name = "Party Winner of Federal Elections",
    values = c("#2E74C0", "#CB454A", "#999999"),
    labels = c("Democrat-controlled", "Republican-controlled", "Split Ticket")
    ) +
  theme_classic() +
  # Create separate scatter plots for each year
  facet_wrap(~ year, scales = "free") +
  # Add a regression line so we can see how the general trend compares
  # geom_smooth(method = "lm", se = FALSE, alpha = .6, linetype = "dash") +
  # Let's change the names of the axes and title
  labs(title = "Number of Polling Places by Total Votes Cast",
       subtitle = paste("*Data is broken out across", n_distinct(pp_counts$state), "states"),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                       "https://github.com/publici/us-polling-places")
  ) +
  xlab("Number of Polling Sites") +
  ylab("Total Votes Cast") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"))
votes_scatter

ggsave(here("Viz/Polling Places by Votes Scatter Plot.jpg"), votes_scatter)

# Let's take another look at our top 10 counts from earlier to see if there's a correlation with
# which party won a given state
top_10_by_party <- party_triumphs %>%
  select(state, year, owner) %>%
  distinct() %>%
  right_join(top_10_pp, by = c("state", "year")) %>%
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = ps_per_capita,
             # Reorder the variable so they are arrange descending for each year plot
             y = forcats::fct_reorder(paste(state, year, sep = "_"), ps_per_capita),
             fill = owner
  )) +
  geom_bar(stat = "identity", na.rm = T) +
  # Create a separate chart, with a flexible y-axis, for each level of office
  facet_wrap(~year, scales = "free_y") +
  scale_fill_manual(
    name = "Party Winner of Federal Elections",
    values = c("#2E74C0", "#CB454A", "#999999"),
    labels = c("Democrat-controlled", "Republican-controlled", "Split Ticket")
  ) +
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
       subtitle = paste("*Data, broken out across", n_distinct(pp_counts$state), "states,",
                        "represents the states with the largest number of polling places per 1,000,000 people."),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                       "https://github.com/publici/us-polling-places")
  ) +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
top_10_by_party

ggsave(here("Viz/Top Polling Places by Party Winner.jpg"), top_10_by_party)

# Interestingly enough, in the most notable election covered by our dataset -- the 2016 election --
# states that ended up going full Republican had the most polling places per capita. The state with
# the most polling places per capita in 2016 -- Wisconsin -- was notable in that it swung for Trump,
# much to the Hillary campaign's surprise. To be honest, this finding surprises me a bit, especially
# given a lot of the rhetoric that Republican states go out of their way to make it more difficult
# for individuals to vote. Let's take a look at our "worst" 10 states to see if this trend holds.

bottom_10_by_party <- pp_counts %>%
  # Group by level and state
  group_by(year) %>%
  # Pick our top 10
  top_n(10, -ps_per_capita) %>%
  # Rearrange our dataset
  arrange(year, ps_per_capita) %>%
  ungroup() %>%
  left_join(party_triumphs %>%
              select(state, year, owner) %>%
              distinct(),
            by = c("state", "year")) %>%
  # Start our visualization, creating our groups by party affiliation
  ggplot(aes(x = ps_per_capita,
             # Reorder the variable so they are arrange descending for each year plot
             y = forcats::fct_reorder(paste(state, year, sep = "_"), -ps_per_capita),
             fill = owner
  )) +
  geom_bar(stat = "identity", na.rm = T) +
  # Create a separate chart, with a flexible y-axis, for each level of office
  facet_wrap(~year, scales = "free_y") +
  scale_fill_manual(
    name = "Party Winner of Federal Elections",
    values = c("#2E74C0", "#CB454A", "#999999"),
    labels = c("Democrat-controlled", "Republican-controlled", "Split Ticket")
  ) +
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
       subtitle = paste("*Data, broken out across", n_distinct(pp_counts$state), "states,",
                        "represents the states with the least number of polling places per 1,000,000 people."),
       caption = paste("Data is accredited to the work of the Center for Public Integrity\n",
                       "https://github.com/publici/us-polling-places")
  ) +
  # format our title and subtitle
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(hjust = 0, color = "slateblue2", size = 10),
        plot.caption = element_text(color = "dark gray", size = 10, face = "italic"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
bottom_10_by_party

ggsave(here("Viz/Bottom Polling Places by Party Winner.jpg"), bottom_10_by_party)

# Looking at the bottom polling places makes the argument a bit more muddled. It's overall a healthier
# mix of states that went full-Democrat and full-Republican (along with a fair share that had mixed
# results).


# Let's take a closer look at our House races. Specifically, let's tie together all three measures
# we've previously been discussing:
#   1. Number of Polling Sites
#   2. Number of Votes Cast
#   3. Total Popoulation
state_winners %>%
  filter(election == "House") %>%
  ggplot(aes(x = polling_sites, y = total_vote, color = party_winner, size = population)) +
  # Make it a scatter plot
  geom_point(alpha = .7) +
  # Control formatting of the point sizes
  scale_size_area(name = "Population") +
  # geom_text_repel(aes(label = state), size = 4) +
  scale_color_manual(
    name = "Party Winner of House Election",
    values = c("#2E74C0", "#CB454A", "#999999"),
    labels = c("Democrats", "Republicans", "Split Ticket")
  ) +
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
  ylab("Number of Votes Cast") +
  # Center the title and format the subtitle/caption
  theme(plot.title = element_text(hjust = 0, color = "slateblue4"),
        plot.subtitle = element_text(color = "slateblue1", size = 10),
        plot.caption = element_text(hjust = 1, face = "italic", color = "dark gray"))

# In terms of trend, I can't see much distinction between states where Republicans won the House
# versus Democrats. Let's test this using regression. In particular, let's fit a regression
# model for House elections, using party, number of votes cast, and population as predictors.
# We expect, based on the plot above, to see highly correlative relationships between number of votes
# cast, population, and number of polling places. The real question lies in party.
house_winners <- state_winners %>%
  filter(election == "House") %>%
  # Let's make it a bit easier to interpret our data
  mutate(votes_per_million = total_vote / 1000000,
         pop_per_million = population / 1000000)

polling_places_model <- lm(polling_sites ~ votes_per_million + pop_per_million + party_winner + year, data = house_winners)
pander(summary(polling_places_model))

# Looking at the model summary, there are a few interesting things to point out about the relationship
# between the number of polling sites in a given state.
#   1. **Population.** The most important thing to note, is that there is only one feature that adequately predicts
#     the number of polling sites: population. The model states that for every *additional* 577 people
#     per million in a state, there will be 1 additional polling site.
#   2. **Total Votes Cast.** Now, if you look back at the bubble plot above, you'd probably (like me) assume that the number
#     of total votes cast is also a strong predictor for number of polling sites. Why then is the p-value
#     (denoted in the model output as PR(>|t|) ) .6867 -- much higher than the usual .05 threshold used
#     to determine statistical significance? Well, it's also true that the number of total votes cast
#     is extremely highly correlated with the population of a state. States with larger populations naturally
#     have many more people turn out to vote. Thus, having both of these features in as predictors is
#     redundant.
#   3. **Party.** The question we've all been waiting for. Is it true that the party that wins
#     the House elections in a given state is determinative of the number of polling sites in that
#     state? Turns out, not so much. The p-value for party winner (denoted *party_winnerRepublican*,
#     representing the base case in which a state goes Republican) is .3694, well beyond the usual
#     threshold of .05. Thus, we cannot reject our initial null hypothesis that party winner predicts
#     number of polling sites.
