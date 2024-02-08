## analysis of strava data, specifically 2023 riding data

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gt) # make some tables

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


## charts and table ideas

# number of rides, total km, avg km per ride by bike
kmall <- strava23 %>%
	summarise(km_mean = mean(distance_km),
						km_25th = quantile(distance_km, 0.25),
						km_median = median(distance_km),
						km_75th = quantile(distance_km, 0.75),
						km_min = min(distance_km),
						km_max = max(distance_km))

km_bybike <- strava23 %>%
	group_by(activity_gear) %>%
	summarise(rides = n(),
						km_mean = mean(distance_km),
						km_25th = quantile(distance_km, 0.25),
						km_median = median(distance_km),
						km75th = quantile(distance_km, 0.75),
						km_min = min(distance_km),
						km_max = max(distance_km))


# which bike

# type of ride

# work commutes

# class commutes

# errands

# distance - mean, med, 25th 75th, min max

