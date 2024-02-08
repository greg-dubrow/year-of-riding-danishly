# #TidyTuesday | January 2 | Bring your Own Data (BYOD)
# My running data from 2020, 2021, 2023, 2023 using Strava API

# load libraries ----------------------------------------------------------
library(tidyverse)
library(rStrava)
library(jsonlite)
library(httr)
library(showtext)
library(lubridate)
# library(ggchicklet)

# add font ----------------------------------------------------------------
font_add_google(name = "Work Sans", family = "Work Sans")
font <- "Work Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)



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
				 activity_day = lubridate::day(start_date_local),
				 activity_wday = wday(activity_date_p, label = TRUE, abbr = FALSE),
				 activity_hour = lubridate::hour(activity_date),
				 activity_min = lubridate::minute(activity_date),
				 activity_hmt = paste0(activity_hour, ":", activity_min),
				 activity_hm = hm(activity_hmt),
				 moving_time_hms = hms::hms(moving_time),
				 elapsed_time_hms = hms::hms(elapsed_time)) %>%
	select(activity_id = id, activity_date:activity_wday, activity_hm, activity_hour, activity_min, timezone,
				 sport_type, commute, gear_name, gear_id, distance_km = distance, moving_time_hms, moving_time,
				 elapsed_time_hms, elapsed_time, average_speed, max_speed, average_watts, kilojoules,
				 elevation_high = elev_high, elevation_low = elev_low, elevation_gain = total_elevation_gain, location_country,
				 lat_start = start_latlng1, lng_start = start_latlng2, lat_end = end_latlng1, lng_end = end_latlng2)

glimpse(act_data)

act_data_csv <- readRDS("data/strava_activities_from_csv.rds")
glimpse(act_data_csv)

act_data_csv %>%
	count(average_elapsed_speed)

act_data_csv_ext <- act_data_csv %>%
	select(activity_id, average_grade, max_grade, average_elapsed_speed, elevation_loss)

strava_activities_final <- act_data %>%
	merge(act_data_csv_ext) %>%
	select(activity_id:max_speed, average_elapsed_speed, elevation_gain, elevation_loss, elevation_high, elevation_low,
				 average_grade, max_grade, location_country:lng_end, average_watts, kilojoules)

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


