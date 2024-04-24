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
strava_data <- readRDS("~/Data/r/year of riding danishly/data/strava_activities_final.rds")
glimpse(strava_data)

strava_data %>%
	count(location_country, timezone, activity_year)

strava_data %>%
	count(location_country, activity_year)

strava_data %>%
	select(distance_km, kilojoules) %>%
	ggplot(aes(kilojoules, distance_km)) +
	geom_point()


### EDA with DataExplorer, explore, skimr
strava_data %>%
	#	select() %>%
	skim() %>%
	select(skim_variable, skim_type, n_missing, complete_rate, numeric.hist, numeric.mean:numeric.p75) %>%
	arrange(desc(skim_type), complete_rate) %>%
	view()

## explorer summary
strava_data %>%
	describe_tbl()

## DataExplorer summary of completes, missings
# summary of completes, missings
introduce(strava_data)
plot_intro(strava_data)
plot_missing(strava_data)

library(patchwork)
library(DataExplorer)

plintro <- plot_intro(strava_data)
plmiss <- plot_missing(strava_data)

plintro + plmiss

## dataexplorer plots
plot_bar(strava_data, nrow = 5L)
plot_histogram(strava_data, nrow = 5L)

## dataexplorer correlations
strava_data %>%
	select(distance_km, elapsed_time, moving_time, max_speed, average_speed, elevation_gain, elevation_loss, elevation_low,
				 elevation_high, average_grade, max_grade, average_watts, calories, kilojoules) %>%
	filter(!is.na(average_watts)) %>%
	filter(!is.na(calories)) %>%
	plot_correlation(maxcat = 5L, type = "continuous", geom_text_args = list("size" = 4))

## dataexplorer scatterplots
strava_data_filter <- strava_data %>%
	select(distance_km, elapsed_time, moving_time, max_speed, average_speed, elevation_gain, elevation_loss, elevation_low,
				 elevation_high, average_grade, max_grade, average_watts, calories, kilojoules) %>%
	filter(!is.na(average_watts) )%>%
	filter(!is.na(calories))

plot_scatterplot(
	strava_data_filter,
	by = "distance_km", nrow = 6L)

ggsave("images/scatterplot_de.png", dpi = 300)

