## from strava archive request. zip fil in ~/data. unzipped files needed for analysis in data file here in project.
# gpx files still in the zip archive. will import a few in another script to analyse
# data dictionary https://stravametro.zendesk.com/hc/en-us/articles/1500001573281-Glossary-Data-Dictionary
# api https://developers.strava.com/docs/reference/

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

## gets list of time zones
OlsonNames(tzdir = NULL)


### clean data, redo as necessary after running basic EDA
strava_activities <- strava_activities1 %>%
	#clean up NA in commute flag
	mutate(commute2 = ifelse(is.na(commute2) & commute == "FALSE", 0, commute2)) %>%
	# clean up missing gear type
	mutate(activity_gear = ifelse(activity_type == "Ride" & is.na(activity_gear),
																 "Commute bike", activity_gear)) %>%
	# clean up date column, parse out date to month, date, year, day of week, time
	mutate(activity_date = str_replace(activity_date, "Jan ", "January ")) %>%
	mutate(activity_date = str_replace(activity_date, "Feb ", "February ")) %>%
	mutate(activity_date = str_replace(activity_date, "Mar ", "March ")) %>%
	mutate(activity_date = str_replace(activity_date, "Apr ", "April ")) %>%
	mutate(activity_date = str_replace(activity_date, "May ", "May ")) %>%
	mutate(activity_date = str_replace(activity_date, "Jun ", "June ")) %>%
	mutate(activity_date = str_replace(activity_date, "Jul ", "July ")) %>%
	mutate(activity_date = str_replace(activity_date, "Aug ", "August ")) %>%
	mutate(activity_date = str_replace(activity_date, "Sep ", "September ")) %>%
	mutate(activity_date = str_replace(activity_date, "Oct ", "October ")) %>%
	mutate(activity_date = str_replace(activity_date, "Nov ", "November ")) %>%
	mutate(activity_date = str_replace(activity_date, "Dec ", "December ")) %>%
	mutate(activity_date2 = activity_date) %>%
	separate('activity_date2', paste("date", 1:3, sep="_"), sep=",", extra="drop") %>%
	mutate(activity_md = str_trim(date_1)) %>%
	separate('activity_md', paste("activity_md", 1:2, sep="_"), sep=" ", extra="drop") %>%
	mutate(activity_mdy = paste0(date_1, ",", date_2)) %>%
	mutate(activity_ymd = lubridate::mdy(activity_mdy)) %>%
	mutate(activity_ymdhms_t = paste0(activity_ymd, date_3)) %>%
	mutate(activity_ymdhms_dt = ymd_hms(activity_ymdhms_t)) %>%
	mutate(timezone = tz(activity_ymdhms_dt)) %>%
	mutate(activity_tz = case_when(
		activity_ymd >= "2022-06-28" ~ "Europe/Copenhagen",
		TRUE ~ "US/Pacific")) %>%
	mutate(activity_ymdhms_cet = as_datetime(activity_ymdhms_dt, tz = 'Europe/Copenhagen')) %>%
	mutate(activity_ymdhms_pst = as_datetime(activity_ymdhms_dt, tz = 'US/Pacific')) %>%
	mutate(date_3 = str_trim(date_3)) %>%
	mutate(activity_wday = case_when(
		activity_tz == "US/Pacific" ~ wday(activity_ymdhms_pst, label = TRUE, abbr = FALSE),
		activity_tz == "Europe/Copenhagen" ~ wday(activity_ymdhms_cet, label = TRUE, abbr = FALSE))) %>%
	mutate(activity_hms = format(as.POSIXct(activity_ymdhms_pst, format = '%I:%M:%S %p'), format = "%H:%M:%S")) %>%
	mutate(activity_hour = ifelse(activity_tz == "US/Pacific",
																hour(activity_ymdhms_pst), hour(activity_ymdhms_cet))) %>%
	mutate(activity_min =	minute(activity_ymdhms_dt)) %>%
	mutate(activity_hmt = paste0(activity_hour, ":", activity_min)) %>%
	mutate(activity_hm = hm(activity_hmt)) %>%
	mutate(activity_year = year(activity_ymdhms_dt)) %>%
	select(activity_id, activity_ymdhms_cet, activity_ymdhms_pst, activity_tz,
				 activity_year, activity_month = activity_md_1, activity_date = activity_md_2, activity_wday,
				 activity_hour, activity_min, activity_hmt, activity_hm,
				 activity_name:activity_type, activity_gear, commute_txt = commute,
				 commute_n = commute2, distance_km = distance, distance_m = distance2, elapsed_time, moving_time,
				 average_speed, average_elapsed_speed, max_speed, elevation_gain:elevation_high, average_grade, max_grade,
				 average_watts, prefer_perceived_exertion, calories, filename)
glimpse(strava_activities)

saveRDS(strava_activities, file = "data/strava_activities.rds")


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

### continue with deeper analysis here or start new r script
