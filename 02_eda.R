## data produced on 01_data import via csv and 02_data import via API

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
# EDA tools
library(DataExplorer)
library(explore)
library(skimr)

# some custom functions
source("~/Data/r/basic functions.R")

# sets theme as default for all plots
theme_set(theme_light)

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text

# load data
strava_data <- readRDS("data/strava_activities_final.rds")


### EDA with DataExplorer, explore, skimr
strava_activities %>%
	#	select() %>%
	skim() %>%
	view()

strava_activities %>%
	filter(is.na(filename)) %>%
	glimpse()

## DataExplorer summary of completes, missings
eda1 <- introduce(strava_activities)
view(eda1)

## explorer summary
strava_activities %>%
	describe_tbl()

## dataexplorer plots
plot_bar(strava_activities)
plot_histogram(strava_activities, nrow = 5L)

## dataexplorer correlations
strava_activities %>%
	select(distance_km, elapsed_time, moving_time, max_speed, elevation_gain, elevation_loss, elevation_low,
				 elevation_high, average_grade, max_grade, average_watts, calories) %>%
	filter(!is.na(average_watts)) %>%
	plot_correlation(maxcat = 5L, type = "continuous", geom_text_args = list("size" = 4))

## dataexplorer scatterplots
strava_activities_filter <- strava_activities %>%
	select(distance_km, elapsed_time, moving_time, max_speed, elevation_gain, elevation_loss, elevation_low,
				 elevation_high, average_grade, max_grade, average_watts, calories) %>%
	filter(!is.na(average_watts))

strava_activities_filter %>%
	#	select() %>%
	skim() %>%
	view()

plot_scatterplot(
	strava_activities_filter,
	by = "distance_km", nrow = 6L)

## manual EDA
strava_activities %>%
	count(activity_type, activity_gear)

strava_activities %>%
	filter(is.na(activity_gear)) %>%
	filter(activity_type == "Ride") %>%
	count(activity_ymdhms_cet, distance_km)
## explorer shiny app
explore(DATA %>%
					select())
