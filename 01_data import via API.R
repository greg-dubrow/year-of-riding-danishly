# #TidyTuesday | January 2 | Bring your Own Data (BYOD)
# My running data from 2020, 2021, 2023, 2023 using Strava API

# load libraries ----------------------------------------------------------
library(tidyverse)
library(tidylog)
library(rStrava)
library(jsonlite)
library(httr)
library(showtext)
library(lubridate)
# library(ggchicklet)


# OAuth access token ------------------------------------------------------

stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])

myinfo <- get_athlete(stoken, id = '45999365')
head(myinfo)

# get Activity List -------------------------------------------------------

myact <- get_activity_list(stoken)
act_data <- compile_activities(myact) %>%
	as_tibble() %>%
	mutate(gear_name = case_when(gear_id == "b6298198" ~ "Univega",
															 gear_id == "b11963967" ~ "Commute bike",
															 TRUE ~ "Not a bike ride")) %>%
	mutate(activity_date = lubridate::as_datetime(start_date_local)) %>%
	mutate(activity_date_p = as.Date(start_date_local)) %>%
	mutate(activity_year = lubridate::year(start_date_local),
				 activity_month = lubridate::month(start_date_local),
				 activity_month_t = lubridate::month(start_date_local, label = TRUE, abbr = FALSE),
				 activity_day = lubridate::day(start_date_local),
				 activity_md = 	paste0(activity_month_t, " ", activity_day),
				 activity_wday = wday(activity_date_p, label = TRUE, abbr = FALSE),
				 activity_hour = lubridate::hour(activity_date),
				 activity_min = lubridate::minute(activity_date),
				 activity_hmt = paste0(activity_hour, ":", activity_min),
				 activity_hm = hm(activity_hmt),
				 moving_time_hms = hms::hms(moving_time),
				 elapsed_time_hms = hms::hms(elapsed_time)) %>%
	mutate(location_country = case_when(
		timezone == "(GMT+01:00) Europe/Copenhagen" ~ "Denmark",
		timezone == "(GMT+01:00) Europe/Paris" ~ "France",
		TRUE ~ "United States")) %>%
## random edits
	mutate(commute = ifelse((activity_year == 2023 & activity_md == "June 14" & name == "Morning commute"),
													TRUE, commute)) %>%
	mutate(commute = ifelse((activity_year == 2023 & activity_md == "September 19" & name == "Afternoon commute"),
													TRUE, commute)) %>%
	mutate(commute = ifelse((activity_year == 2023 & activity_md == "October 5" & name == "Morning Ride"),
													TRUE, commute)) %>%
	mutate(name = ifelse((activity_year == 2023 & activity_md == "October 4" & name == "Morning Ride"),
											 "Morning commute", name)) %>%
	mutate(name = ifelse((activity_year == 2023 & activity_md == "October 4" & name == "Evening Ride"),
											 "Evening commute", name)) %>%
	mutate(name = ifelse((activity_year == 2023 & activity_md == "October 5" & name == "Morning Ride"),
											 "Morning commute", name)) %>%
	mutate(name = ifelse(name == "Evening commmute", "Evening commute", name)) %>%
## adjust studieskolen vesterbro morning rides
	mutate(name = case_when(
		(activity_year == 2023 & (name == "Morning Ride" | name == "Rainy Morning Ride") &
			activity_md %in% c("October 24", "October 26", "October 31", "November 2", "November 7",
												 "November 9", "November 14", "November 16", "November 21", "November 23",
												 "November 28", "November 30", "December 5", "December 7",
												 "December 12", "December 14"))
		~ "To Studieskolen", TRUE ~ name)) %>%
	# adjust studieskolen vesterbro afternoon rides
	mutate(name = case_when(
		(activity_year == 2023 & (name == "Lunch Ride" | name == "Afternoon Ride") &
		 	activity_md %in% c("October 24", "October 26", "October 31", "November 7",
		 										 "November 9", "November 14", "November 16", "November 21", "November 23",
		 										 "November 30", "December 5", "December 12", "December 14"))
		~ "From Studieskolen", TRUE ~ name)) %>%
	mutate(name = ifelse((activity_year == 2023 & name == "From Studieskolen" &
											 	activity_md %in% c("November 23", "December 14") & activity_hour > 13),
											 "Afternoon Ride", name)) %>%
## adjust studieskolen KVUC rides
	mutate(name = case_when(
		(activity_year == 2023 & name == "Afternoon Ride" &
		 	activity_md %in% c("October 9", "October 11",
		 										 "October 23", "October 25", "October 30",  "November 1",
		 										 "November 6", "November 8", "November 13", "November 15",
		 										 "November 20", "November 22", "November 27", "November 29",
		 										 "December 4", "December 6", "December 11", "December 13",
		 										 "December 20")) ~ "To Studieskolen KVUC",
		TRUE ~ name)) %>%
	mutate(name = case_when(
		(activity_year == 2023 & name == "Evening Ride" &
		 	activity_md %in% c("October 9", "October 11",
		 										 "October 23", "October 25", "October 30",  "November 1",
		 										 "November 6", "November 8", "November 13", "November 15",
		 										 "November 20", "November 22", "November 27", "November 29",
		 										 "December 4", "December 6", "December 11", "December 13",
		 										 "December 20")) ~ "From Studieskolen KVUC",
		TRUE ~ name)) %>%
	mutate(name = ifelse(
		(activity_year == 2023 & name == "To Studieskolen KVUC" & activity_md == "December 20" & activity_hour == 16),
		"From Studieskolen KVUC", name)) %>%
	mutate(name = ifelse((commute == "TRUE" & grepl("Ride", name)),
																str_replace(name, "Ride", "commute"), name)) %>%
	mutate(ride_type = case_when(
		commute == "TRUE" ~ "Commute/Studieskolen",
		name %in% c("To Studieskolen", "From Studieskolen",
												 "To Studieskolen KVUC", "From Studieskolen KVUC")
		~ "Commute/Studieskolen",
		gear_name == "Univega" ~ "Workout",
		TRUE ~ "Other")) %>%
	select(activity_id = id, activity_date:activity_wday, activity_hm, activity_hour, activity_min, timezone,
				 activity_name = name, ride_type, sport_type, commute, gear_name, gear_id, distance_km = distance,
				 moving_time_hms, moving_time, elapsed_time_hms, elapsed_time, average_speed, max_speed, average_watts, kilojoules,
				 elevation_high = elev_high, elevation_low = elev_low, elevation_gain = total_elevation_gain, location_country,
				 lat_start = start_latlng1, lng_start = start_latlng2, lat_end = end_latlng1, lng_end = end_latlng2)

glimpse(act_data)

act_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_month >=10) %>%
	filter(activity_md %in% c("October 24", "October 26", "October 31", "November 2", "November 7",
										 "November 9", "November 14", "November 16", "November 21", "November 23",
										 "November 28", "November 30", "December 5", "December 7",
										 "December 12", "December 14")) %>%
	arrange(activity_month, activity_day, activity_hm) %>%
	select(activity_md, activity_year, activity_hm, activity_name, commute, distance_km, elapsed_time_hms) %>%
	view()

act_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_month <=10) %>%
	count(commute, activity_name) %>%
	view()

act_data %>%
	filter(activity_year == 2023) %>%
	count(activity_name) %>%
	view()

act_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_month <=10) %>%
	filter(grepl("commute", activity_name, ignore.case = TRUE)) %>%
	filter(commute == "FALSE") %>%
	select(activity_md, activity_year, activity_hm, activity_name, distance_km, elapsed_time_hms) %>%
	view()

act_data_csv <- readRDS("data/strava_activities_from_csv.rds")
glimpse(act_data_csv)

act_data_csv %>%
	count(average_elapsed_speed)

act_data_csv_ext <- act_data_csv %>%
	select(activity_id, calories, average_grade, max_grade, average_elapsed_speed, elevation_loss)
glimpse(act_data_csv_ext)

strava_activities_final <- act_data %>%
	merge(act_data_csv_ext) %>%
	select(activity_id:max_speed, average_elapsed_speed, elevation_gain, elevation_loss, elevation_high, elevation_low,
				 average_grade, max_grade, location_country:lng_end, average_watts, calories, kilojoules)

glimpse(strava_activities_final)

saveRDS(strava_activities_final, file = "data/strava_activities_final.rds")


## definitions
# average_speed = meters / second
# max_speed in meters / second

act_data %>%
	filter(id == 2673634672) %>%
	glimpse()

act_data_excel %>%
	filter(activity_id == 2673634672) %>%
	glimpse()

act_data %>%
	count(heartrate_opt_out)

act_data %>%
	count(sport_type, gear_id, gear_name)

act_data %>%
	filter(type == "Ride") %>%
	select(year, month, day,  start_date,  start_date_local, timezone, type, gear_id, commute) %>%
	arrange(year, month, day) %>%
	view()

### get specific activities - results in nested list with 7K fields
act_2673634672 <- get_activity(2673634672, stoken)
act_2673634672_dat1 <- compile_activity(act_2673634672)
glimpse(act_2673634672_dat1) %>%
	view()

act_2673634672[["calories"]]

act_10444040043 <- get_activity(10444040043, stoken)


