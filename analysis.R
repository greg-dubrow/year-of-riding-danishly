## analysis of strava data, specifically 2023 riding data

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text

# load data
strava_activities <- readRDS(file = "data/strava_activities.rds")

# filter for 2023
glimpse(strava_activities)
strava23 <- strava_activities %>%
	filter(activity_year == 2023)

strava23 %>%
	count(activity_type, activity_gear)

