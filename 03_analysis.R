## more tailored analysis
## data produced on 01_data import via csv and 02_data import via API

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gt)
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
glimpse(strava_data)

strava_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_month >= 10) %>%
	view()

## summary table
sumtable <- strava_data %>%
	filter(activity_year == 2023) %>%
	summarise(rides = n(),
						km_total = sum(distance_km),
						time_total_s = sum(moving_time),
						elev_total = sum(elevation_gain),
						cal_total = sum(calories),
						kiloj_total = sum(kilojoules),
						km_avg = round(mean(distance_km), 2),
						km_med = round(median(distance_km),2),
						# km_25th = quantile(distance_km, 0.25),
						# km_75th = quantile(distance_km, 0.75),
						km_min = round(min(distance_km),2),
						km_max = round(max(distance_km),2),
						time_avg_s = mean(moving_time),
						time_med_s = median(moving_time),
						time_min_s = min(moving_time),
						time_max_s = max(moving_time),
						elev_avg = round(mean(elevation_gain),2),
						elev_med = round(median(elevation_gain),2),
						elev_min = round(min(elevation_gain),2),
						elev_max = round(max(elevation_gain),2),
						cal_avg = round(mean(calories),2),
						cal_med = round(median(calories),2),
						cal_min = round(min(calories),2),
						cal_max = round(max(calories),2),
						kiloj_avg = round(mean(kilojoules),2),
						kiloj_med = round(median(kilojoules),2),
						kiloj_min = round(min(kilojoules),2),
						kiloj_max = round(max(kilojoules),2)) %>%
	mutate(time_total1 = hms::hms(time_total_s)) %>%
	mutate(time_total2 = seconds_to_period(time_total_s)) %>%
	mutate(time_avg = seconds_to_period(time_avg_s)) %>%
	mutate(time_avg = round(time_avg, 0)) %>%
	mutate(time_med = seconds_to_period(time_med_s)) %>%
	mutate(time_med = round(time_med, 0)) %>%
	mutate(time_min = seconds_to_period(time_min_s)) %>%
	mutate(time_min = round(time_min, 0)) %>%
	mutate(time_max = seconds_to_period(time_max_s)) %>%
	mutate(time_max = round(time_max, 0))

glimpse(sumtable)

sumtable %>%
	select(rides, km_total, elev_total, time_total1, time_total2, cal_total, kiloj_total) %>%
	gt() %>%
	fmt_number(columns = c(km_total, elev_total, cal_total, kiloj_total), decimals = 0) %>%
	cols_label(rides = "Total Rides", km_total = "Total Kilometers",
						 elev_total = md("Total Elevation *(meters)*"),
						 time_total1 = md("Total Time *(hours/min/sec)*"),
						 time_total2 = md("Total Time *(days/hours/min/sec)*"),
						 cal_total = "Total Calories", kiloj_total = "Total Kilojoules") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_style(
		style = cell_text(align = "center"),
		locations = cells_column_labels(
			columns = c(rides, km_total, elev_total, time_total1, time_total2, cal_total, kiloj_total))) %>%
	tab_header(title = md("My Year of Riding Danishly<br>*Ride Totals*"))

sumtable %>%
	select(km_avg, km_med, km_min, km_max) %>%
	gt() %>%
	cols_label(km_avg = "Average", km_med = "Median",
						 km_min = "Shortest", km_max = "Longest") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_header(title = md("*Ride Statisitcs - Distance (in km)*"))

sumtable %>%
	select(time_avg, time_med, time_min, time_max) %>%
	gt() %>%
	cols_label(time_avg = "Average", time_med = "Median",
						 time_min = "Shortest", time_max = "Longest") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_header(title = md("*Ride Statistics - Time*"))

sumtable %>%
	select(elev_avg, elev_med, elev_min, elev_max) %>%
	gt() %>%
	cols_label(elev_avg = "Average", elev_med = "Median",
						 elev_min = "Shortest", elev_max = "Longest") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_header(title = md("*Ride Statistics - Elevation (meters)*"))

sumtable %>%
	select(cal_avg, cal_min, cal_max, kiloj_avg, kiloj_min, kiloj_max) %>%
	gt() %>%
	cols_label(cal_avg = "Average", cal_min = "Shortest", cal_max = "Longest",
						 kiloj_avg = "Average", kiloj_min = "Shortest", kiloj_max = "Longest") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_spanner(label = "Calories", columns = c(cal_avg, cal_min, cal_max)) %>%
	tab_spanner(label = "Kilojoules", columns = c(kiloj_avg, kiloj_min, kiloj_max)) %>%
	tab_header(title = md("*Ride Statistics - Energy*"))


## bar charts for month, day of the week
strava_data %>%
	filter(activity_year == 2023) %>%
	mutate(ride_type = case_when(
		commute == "TRUE" ~ "Commute/Studieskolen",
		activity_name %in% c("To Studieskolen", "From Studieskolen",
												 "To Studieskolen KVUC", "From Studieskolen KVUC")
		~ "Commute/Studieskolen",
		gear_name == "Univega" ~ "Workout",
		TRUE ~ "Other")) %>%
	count(ride_type, activity_name) %>%
	view()
	group_by(activity_month_t, ride_type) %>%
	summarise(ride_type_n = n()) %>%
	mutate(ride_type_pct = ride_type_n / sum(ride_type_n)) %>%
	ungroup() %>%
	group_by(activity_month_t) %>%
	mutate(rides_by_month = sum(ride_type_n)) %>%
	ungroup() %>%
	view()

	#	group_by(commute) %>%
	ggplot(aes(activity_month_t, fill = commute)) +
	geom_bar() +
	#geom_bar(fill = "#C8102E") +
	geom_text(stat = "count", aes(label=after_stat(count)),
						color = "white", size = 5) +
	labs(x = "", y = "") +
	theme_minimal() +
	theme(panel.grid = element_blank())


## clock for time

## scappterplots

# using cedric scherer post to create scatters of all possible combos
# https://www.cedricscherer.com/2023/07/05/efficiency-and-consistency-automate-subset-graphics-with-ggplot2-and-purrr/

## set up variables of interest
actnames <- c("distance_km", "elapsed_time", "moving_time", "max_speed", "average_speed", "elevation_gain", "elevation_loss",
							"elevation_low", "elevation_high", "average_grade", "max_grade", "average_watts", "calories", "kilojoules")
## ... and create all possible combinations
actnames_set <- tidyr::expand_grid(actnames, actnames)

## function for plot (copied directly from cedric)
plot_scatter_lm <- function(data, var1, var2, pointsize = 2, transparency = .5, color = "") {

	## check if inputs are valid
	if (!exists(substitute(data))) stop("data needs to be a data frame.")
	if (!is.data.frame(data)) stop("data needs to be a data frame.")
	if (!is.numeric(pull(data[var1]))) stop("Column var1 needs to be of type numeric, passed as string.")
	if (!is.numeric(pull(data[var2]))) stop("Column var2 needs to be of type numeric, passed as string.")
	if (!is.numeric(pointsize)) stop("pointsize needs to be of type numeric.")
	if (!is.numeric(transparency)) stop("transparency needs to be of type numeric.")
	if (color != "") { if (!color %in% names(data)) stop("Column color needs to be a column of data, passed as string.") }

	g <-
		ggplot(data, aes(x = !!sym(var1), y = !!sym(var2))) +
		geom_point(aes(color = !!sym(color)), size = pointsize, alpha = transparency) +
		geom_smooth(aes(color = !!sym(color), color = after_scale(prismatic::clr_darken(color, .3))),
								method = "lm", se = FALSE) +
		theme_minimal() +
		theme(panel.grid.minor = element_blank(),
					legend.position = "top")

	if (color != "") {
		if (is.numeric(pull(data[color]))) {
			g <- g + scale_color_viridis_c(direction = -1, end = .85) +
				guides(color = guide_colorbar(
					barwidth = unit(12, "lines"), barheight = unit(.6, "lines"), title.position = "top"
				))
		} else {
			g <- g + scale_color_brewer(palette = "Set2")
		}
	}

	return(g)
}

# "distance_km", "elapsed_time", "moving_time", "max_speed", "average_speed", "elevation_gain", "elevation_loss",
# "elevation_low", "elevation_high", "average_grade", "max_grade", "average_watts", "calories", "kilojoules"

# with distance as Y - elapsed_time, moving_time, average_speed, watts, calories, kilojoules
# with moving_time as Y average_speed, elevation_gain, average_grade, watts, calories, kilojoules
# average speed as Y "average_grade", "max_grade", "average_watts", "calories", "kilojoules"
# watts as y - kilojoules, calories

strava_activities_rides <- strava_activities_final %>%
	filter(sport_type == "Ride")

# produces each plot as its own image
map2(
	c("elapsed_time", "average_speed","average_watts", "calories", "kilojoules"),
	c("distance_km", "average_grade", "distance_km","distance_km","distance_km"),
	~plot_scatter_lm(
		data = strava_activities_rides, var1 = .x, var2 = .y,
		color = "gear_name", pointsize = 3.5
	)
)

# same as above but wraps plots...in qmd put this in r code chunk echo=FALSE, fig.width=15, fig.height=5
patchwork::wrap_plots(map2(c("elapsed_time", "average_speed","average_watts", "calories", "kilojoules"),
													 c("distance_km", "average_grade", "distance_km","distance_km","distance_km"),
													 ~plot_scatter_lm(data = strava_activities_rides, var1 = .x, var2 = .y,
													 								 #color = "gear_name",
													 								 pointsize = 3.5) +
													 	theme(plot.margin = margin(rep(15, 4)))))
# in qmd file add this below
# <figcaption>text here </figcaption>

pmap(
	actnames_set, ~plot_scatter_lm(
		data = strava_data_filter,
		var1 = .x, var2 = .y
	)
)
