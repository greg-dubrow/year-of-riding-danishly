## from strava archive request. zip fil in ~/data. unzipped files needed for analysis in data file here in project.
# gpx files still in the zip archive. will import a few in another script to analyse

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


### load data
strava_activities1 <- readr::read_csv("data/activities.csv") %>%
	clean_names() %>%
	as_tibble() %>%
	rename(elapsed_time = elapsed_time_6, distance = distance_7, max_heart_rate = max_heart_rate_8,
				 relative_effort = relative_effort_9, commute = commute_10, elapsed_time2 = elapsed_time_16,
				 distance2 = distance_18, relative_effort2 = relative_effort_38, commute2 = commute_51)
glimpse(strava_activities1)

## skimr summary
strava_activities1 %>%
	#	select() %>%
	skim() %>%
	view()

strava_activities1 %>%
	select(dirt_distance, distance, distance2) %>%
	view()

### clean data, redo as necessary after running basic EDA
strava_activities <- strava_activities1 %>%
	mutate(commute2 = ifelse(is.na(commute2) & commute == "FALSE", 0, commute2)) %>%
	select(activity_id, activity_date:activity_type, activity_gear, commute_txt = commute,
				 commute_n = commute2, distance_km = distance, distance_m = distance2, elapsed_time, moving_time,
				 average_speed, average_elapsed_speed, max_speed, elevation_gain:elevation_high, average_grade, max_grade,
				 average_watts, prefer_perceived_exertion, calories, filename)
glimpse(strava_activities)

strava_activities %>%
	count(distance_km)

### EDA with DataExplorer, explore, skimr

## DataExplorer summary of completes, missings
eda1 <- introduce(DATA)
view(eda1)

## explorer summary
whr23_fig2_1 %>%
	describe_tbl()



## dataexplorer plots
plot_bar(DATA)
plot_histogram(DATA, nrow = 5L)

## dataexplorer correlations
DATA %>%
	select(or deselect as needed) %>%
	filter(!is.na(if needed)) %>%
	plot_correlation(maxcat = 5L, type = "continuous", geom_text_args = list("size" = 4))

## dataexplorer scatterplots
plot_scatterplot(
	DATA %>% select(), by = "choose target for y axis", nrow = 3L)

## explorer shiny app
explore(DATA %>%
					select())

### continue with deeper analysis here or start new r script
