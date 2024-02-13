## more tailored analysis
## data produced on 01_data import via csv and 02_data import via API

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gt) # tables!
library(ggtext) # ggplot text helpers
# EDA tools
library(DataExplorer)
library(explore)
library(skimr)
library(modelsummary)

# some custom functions
source("~/Data/r/basic functions.R")

# sets theme as default for all plots
theme_set(theme_light)

## ggplot helpers - load if necessary
library(patchwork) # to stitch together plots
library(ggtext) # helper functions for ggplot text
library(ggrepel) # helper functions for ggplot text

# load data
strava_data <- readRDS("data/strava_activities_final.rds") %>%
	mutate(activity_hour2 = activity_hour + 1)
glimpse(strava_data)

strava_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_month == 9) %>%
	view()

## summary table
sumtable <- strava_data %>%
	filter(activity_year == 2023) %>%
	summarise(rides = n(),
						km_total = sum(moving_time),
						time_total_s = sum(moving_time),
						elev_total = sum(elevation_gain),
						cal_total = sum(calories),
						kiloj_total = sum(kilojoules),
						km_avg = round(mean(moving_time), 2),
						km_med = round(median(moving_time),2),
						# km_25th = quantile(moving_time, 0.25),
						# km_75th = quantile(moving_time, 0.75),
						km_min = round(min(moving_time),2),
						km_max = round(max(moving_time),2),
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
saveRDS(sumtable, file = "data/sumtable.rds")

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
	tab_header(title = md("*Ride Statistics - Distance (in km)*"))

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
						 elev_min = "Lowest", elev_max = "Highest") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_header(title = md("*Ride Statistics - Elevation (meters)*"))

sumtable %>%
	select(cal_avg, cal_min, cal_max, kiloj_avg, kiloj_min, kiloj_max) %>%
	gt() %>%
	cols_label(cal_avg = "Average", cal_min = "Least", cal_max = "Most",
						 kiloj_avg = "Average", kiloj_min = "Least", kiloj_max = "Most") %>%
	cols_align(align = "center", columns = everything()) %>%
	tab_spanner(label = "Calories Burned", columns = c(cal_avg, cal_min, cal_max)) %>%
	tab_spanner(label = "Kilojoules Burned", columns = c(kiloj_avg, kiloj_min, kiloj_max)) %>%
	tab_header(title = md("*Ride Statistics - Energy*"))


## bar charts for month, day of the week
 # by month & type
glimpse(strava_data)
rides_mth_type <- strava_data %>%
	filter(activity_year == 2023) %>%
	group_by(activity_month_t, ride_type) %>%
	summarise(ride_type_n = n()) %>%
	mutate(ride_type_pct = ride_type_n / sum(ride_type_n)) %>%
	ungroup() %>%
	group_by(activity_month_t) %>%
	mutate(rides_by_month = sum(ride_type_n)) %>%
	ungroup() %>%
	mutate(activity_month_abbv = plyr::mapvalues(activity_month_t, from = month.name, to = month.abb))
glimpse(rides_mth_type)
saveRDS(rides_mth_type, file = "data/rides_mth_type.rds")

## all rides
# by month
rides_mth_type %>%
	distinct(activity_month_t, .keep_all = TRUE) %>%
	select(activity_month_abbv, rides_by_month) %>%
	ggplot(aes(activity_month_abbv, rides_by_month)) +
	geom_col(fill = "#C8102E") +
	geom_text(aes(label= rides_by_month),
						color = "white", size = 5, vjust = 1.5) +
	labs(x = "", y = "", title = "Spring & Summer Weather = More Rides",
			 subtitle = glue::glue("*Average Rides / Month = {round(mean(rides_mth_type$rides_by_month, 3))}*")) +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5),
				axis.text.y = element_blank())

# by type
rides_mth_type %>%
	select(ride_type, ride_type_n) %>%
	group_by(ride_type) %>%
	mutate(rides_by_type = sum(ride_type_n)) %>%
	ungroup() %>%
	select(-ride_type_n) %>%
	distinct(rides_by_type, .keep_all = TRUE) %>%
	mutate(ride_type_pct = rides_by_type / sum(rides_by_type)) %>%
	{. ->> tmp} %>%
	ggplot(aes(ride_type, ride_type_pct)) +
	geom_col(fill = "#C8102E") +
	scale_x_discrete(labels = paste0(tmp$ride_type, "<br>Total Rides = ", tmp$rides_by_type, "")) +
	geom_text(data = subset(tmp, ride_type != "Workout"),
		aes(label= scales::percent(round(ride_type_pct, 2))),
						color = "white", size = 5, vjust = 1.5) +
	geom_text(data = subset(tmp, ride_type == "Workout"),
						aes(label= scales::percent(round(ride_type_pct, 2))),
						color = "#C8102E", size = 5, vjust = -.5) +
	labs(x = "", y = "", title = "Lots of Riding to Work or Danish Class") +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
				axis.text.y = element_blank(), axis.text.x = element_markdown())
	rm(tmp)

#	group_by(commute) %>%
rides_mth_type %>%
	ggplot(aes(activity_month_t, ride_type_pct, fill = ride_type)) +
	geom_bar(stat = "identity") +
	geom_text(data = subset(rides_mth_type, ride_type != "Workout"),
						aes(label = scales::percent(round(ride_type_pct, 2))),
						position = position_stack(vjust = 0.5),
						color= "white", vjust = 1, size = 5) +
	labs(x = "", y = "", title = "Most Rides Each Month Were Commutes to/from Work or Danish Class") +
	scale_fill_manual(values = c("#0072B2", "#E69F00", "#CC79A7"),
										labels = c("Commute/<br>Studieskolen", "Other", "Workout")) +
	theme_minimal()+
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
				legend.text = element_markdown(),
				legend.key.width = unit(1.5, 'cm'), legend.title = element_blank(),
				axis.text.y = element_blank(), plot.title = element_text(hjust = 0.5),
		panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom"))

# day of the week and type
strava_data %>%
	filter(activity_year == 2023) %>%
	group_by(activity_wday) %>%
	summarise(rides_by_wday = n()) %>%
	mutate(rides_wday_pct = rides_by_wday / sum(rides_by_wday)) %>%
	mutate(rides_day_avg = round(mean(rides_by_wday), 0)) %>%
	ungroup() %>%
	mutate(total_rides = sum(rides_by_wday)) %>%
	{. ->> tmp} %>%
	ggplot(aes(activity_wday, rides_by_wday)) +
	geom_col(fill = "#C8102E") +
	scale_x_discrete(labels = paste0(tmp$activity_wday, "<br>Total Rides = ", tmp$rides_by_wday, "")) +
	geom_text(aes(label = scales::percent(round(rides_wday_pct, 2))),
						color = "white", size = 5, vjust = 1.5) +
	labs(x = "", y = "", title = "More Rides on Weekdays, Especially Tues -> Thurs",
			 subtitle = glue::glue("*Total Rides = {tmp$total_rides} <br> Average Rides / Day of the Week = {tmp$rides_day_avg}*")) +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5),
				axis.text.x = element_markdown(),
				axis.text.y = element_blank())
rm(tmp)


strava_data %>%
	filter(activity_year == 2023) %>%
	group_by(activity_wday, ride_type) 	%>%
	summarise(ride_type_n = n()) %>%
	mutate(ride_type_pct = ride_type_n / sum(ride_type_n)) %>%
	ungroup() %>%
	ggplot(aes(activity_wday, ride_type_pct, fill = ride_type)) +
	geom_bar(stat = "identity") +
	geom_text(aes(label = scales::percent(round(ride_type_pct, 2))),
						position = position_stack(vjust = 0.5),
						color= "white", size = 5) +
	labs(x = "", y = "", title = "Weekdays Were for Getting to/from Work & Danish Class",
			 subtitle = "Weekends for Errands and Workouts") +
	scale_fill_manual(values = c("#0072B2", "#E69F00", "#CC79A7"),
										labels = c("Commute/<br>Studieskolen", "Other", "Workout")) +
	theme_minimal() +
	theme(legend.position = "bottom", legend.spacing.x = unit(0, 'cm'),
				legend.text = element_markdown(),
				legend.key.width = unit(1.5, 'cm'), legend.title = element_blank(),
				axis.text.y = element_blank(),
				plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_text(hjust = 0.5, size = 14),
				panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
	guides(fill = guide_legend(label.position = "bottom"))

## clock for time
# from https://rstudio-pubs-static.s3.amazonaws.com/3369_998f8b2d788e4a0384ae565c4280aa47.html

strava_data %>%
	filter(activity_year == 2023) %>%
	count(ride_type, activity_hour) %>%
	{. ->> tmp} %>%
	ggplot(aes(activity_hour, y = n, fill = ride_type)) +
	geom_bar(stat = "identity") +
	scale_x_continuous(limits = c(0, 24), breaks = seq(0, 24)) +
	geom_text(data = subset(tmp, ride_type == "Commute/Studieskolen" & n > 20),
		aes(label= n), color = "white", size = 4) +
	coord_polar(start = 0) +
	theme_minimal() +
	scale_fill_manual(values = c("#0072B2", "#E69F00", "#CC79A7"),
										labels = c("Commute/<br>Studieskolen", "Other", "Workout")) +
	labs(x = "", y = "",
			 title = "Most Rides During Morning and Evening Commuting Hours",
			 subtitle = "*Numbers Correspond to Hour of Day on a 24 hr clock*") +
	theme(legend.text = element_markdown(),
				axis.text.y = element_blank(),
				legend.title = element_blank(),
				plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5, size = 10))
rm(tmp)

glimpse(strava_data)
strava_data %>%
	filter(activity_year == 2023) %>%
	count(activity_min)

activity_am <- strava_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_hour < 12) %>%
	group_by(activity_min) %>%
	summarise(activity_min_n = n()) %>%
	mutate(ampm = "AM")

activity_pm <- strava_data %>%
	filter(activity_year == 2023) %>%
	filter(activity_hour >= 12) %>%
	group_by(activity_min) %>%
	summarise(activity_min_n = n()) %>%
	mutate(ampm = "PM")

activty_ampm <- activity_am %>%
	rbind(activity_pm)
glimpse(activty_ampm)
saveRDS(activty_ampm, file = "data/activty_ampm.rds")


activty_ampm %>%
	ggplot(aes(activity_min, y = activity_min_n, fill = ampm)) +
	geom_col(position = position_stack(reverse = TRUE)) +
	scale_x_continuous(limits = c(-1, 60), breaks = seq(0, 59), labels = seq(0, 59)) +
	geom_text(data = subset(activty_ampm, activity_min_n > 5),
						aes(label= activity_min_n), color = "white", size = 4, position = position_nudge(y = -1)) +
	coord_polar(start = 0) +
	theme_minimal() +
	scale_fill_manual(values = c("#E57A77", "#7CA1CC"),
										labels = c("AM", "PM")) +
	labs(x = "", y = "",
			 title = "Most Morning Rides Started Between 12 & 30 Past the Hour <br>
			 Evening Rides More Evenly Spaced Through the Hour",
			 subtitle = "*Numbers Correspond to  Minutes of the Hour*") +
	theme(legend.text = element_markdown(),
				axis.text.y = element_blank(),
				legend.title = element_blank(),
				plot.title = element_markdown(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5, size = 10))


# strava_data %>%
# 	filter(activity_year == 2023) %>%
# 	ggplot(aes(x = activity_hour2, fill = ride_type)) +
# 	geom_histogram(breaks = seq(0, 24), width = 2, colour = "grey") +
# 	# stat_count(data = subset(strava_data, ride_type == "Workout"),
# 	# 	aes(y=..count..,label=..count..),geom="text", vjust = -1) +
# 	coord_polar(start = 0) +
# 	theme_minimal() +
# 	scale_fill_manual(values = c("#0072B2", "#E69F00", "#CC79A7"),
# 										labels = c("Commute/<br>Studieskolen", "Other", "Workout")) +
# 	labs(y = "Count", title = "Most Rides During Morning and Evening Commuting Hours") +
# 	scale_x_continuous("", limits = c(0, 24),
# 										 breaks = seq(0, 24),
# 										 labels = seq(0, 24)) +
# 	theme(legend.text = element_markdown(),
# 				legend.title = element_blank(),
# 				plot.title = element_text(hjust = 0.5),
# 				plot.subtitle = element_text(hjust = 0.5, size = 14))

## scappterplots

# using cedric scherer post to create scatters of all possible combos
# https://www.cedricscherer.com/2023/07/05/efficiency-and-consistency-automate-subset-graphics-with-ggplot2-and-purrr/

## set up variables of interest
actnames <- c("moving_time", "elapsed_time", "moving_time", "max_speed", "average_speed", "elevation_gain", "elevation_loss",
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

# "moving_time", "elapsed_time", "moving_time", "max_speed", "average_speed", "elevation_gain", "elevation_loss",
# "elevation_low", "elevation_high", "average_grade", "max_grade", "average_watts", "calories", "kilojoules"

# with distance as Y - elapsed_time, moving_time, average_speed, watts, calories, kilojoules
# with moving_time as Y average_speed, elevation_gain, average_grade, watts, calories, kilojoules
# average speed as Y elevation_gain, "average_grade",  "max_grade", "average_watts", "calories", "kilojoules"
# watts as y - kilojoules, calories

strava_activities_rides <- strava_data %>%
	filter(activity_year == 2023)

# wraps plots...in qmd put this in r code chunk echo=FALSE, fig.width=15, fig.height=5
patchwork::wrap_plots(map2(c("elapsed_time", "moving_time", "average_speed","average_watts", "calories", "kilojoules"),
													 c("distance_km", "distance_km", "distance_km", "distance_km", "distance_km", "distance_km"),
													 ~plot_scatter_lm(data = strava_activities_rides, var1 = .x, var2 = .y,
													 								 #color = "gear_name",
													 								 pointsize = 3.5) +
													 	theme(plot.margin = margin(rep(15, 4)))))

patchwork::wrap_plots(map2(c("average_speed", "elevation_gain", "average_grade", "average_watts", "calories", "kilojoules"),
													 c("moving_time", "moving_time", "moving_time", "moving_time", "moving_time", "moving_time"),
													 ~plot_scatter_lm(data = strava_activities_rides, var1 = .x, var2 = .y,
													 								 #color = "gear_name",
													 								 pointsize = 3.5) +
													 	theme(plot.margin = margin(rep(15, 4)))))

patchwork::wrap_plots(map2(c("elevation_gain", "average_grade", "max_grade", "average_watts", "calories", "kilojoules"),
													 c("average_speed", "average_speed", "average_speed", "average_speed", "average_speed", "average_speed"),
													 ~plot_scatter_lm(data = strava_activities_rides, var1 = .x, var2 = .y,
													 								 #color = "gear_name",
													 								 pointsize = 3.5) +
													 	theme(plot.margin = margin(rep(15, 4)))))

patchwork::wrap_plots(map2(c("kilojoules"),
													 c("average_watts"),
													 ~plot_scatter_lm(data = strava_activities_rides, var1 = .x, var2 = .y,
													 								 #color = "gear_name",
													 								 pointsize = 3.5) +
													 	theme(plot.margin = margin(rep(15, 4)))))


# in qmd file add this below
# <figcaption>text here </figcaption>




## Regression model with moving time as dependent variable
# "moving_time", "elapsed_time", "moving_time", "max_speed", "average_speed", "elevation_gain", "elevation_loss",
# "elevation_low", "elevation_high", "average_grade", "max_grade", "average_watts", "calories", "kilojoules"

strava_data %>%
	count(ride_type)

strava_models <- strava_data %>%
	filter(activity_year == 2023) %>%
	mutate(bike_type = ifelse(gear_name == "Commute bike", 1, 0)) %>%
	mutate(is_workout = ifelse(ride_type == "Workout", 1, 0))

strava_models %>%
	count(bike_type)


model_time <- lm(data = strava_activities_rides, formula =
							moving_time ~ distance_km +average_speed + elevation_gain + average_grade + average_watts)

model_time$coefficients
model_time_summary <- modelsummary(model_time)

glimpse(strava_activities_rides)

ride_models <- list(
	"time" = lm(moving_time ~ distance_km + average_speed + elevation_gain + average_grade + average_watts,
							data = strava_models),
	"watts" = lm(average_watts ~ moving_time + distance_km +average_speed + elevation_gain + average_grade + kilojoules,
							 data = strava_models),
	"kilojoules" = lm(kilojoules ~ moving_time + average_speed + elevation_gain + average_grade + average_watts,
											data = strava_models))
modelsummary(ride_models, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log")
modelplot(ride_models, coef_omit = "Interc")
car::vif(ride_models$time)
colin_time <- stack(car::vif(ride_models$time))
gt(colin_time)
car::vif(ride_models$watts)
car::vif(ride_models$kilojoules)

model_time = lm(moving_time ~ distance_km + average_speed + elevation_gain + average_grade + average_watts,
								data = strava_models)
modelsummary(model_time)
car::vif(model_time)
model_watts = lm(average_watts ~ distance_km + average_speed + elevation_gain + average_grade + kilojoules,
						 data = strava_models)
car::vif(model_watts)

model_kilojoules = lm(kilojoules ~ moving_time + average_speed + elevation_gain + average_grade + average_watts,
									data = strava_models)
car::vif(model_kilojoules)

## redo with comparison models colinearity removed
ride_models <- list(
	"time" = lm(moving_time ~ distance_km + average_speed + elevation_gain + average_grade + average_watts,
							data = strava_models),
	"watts" = lm(average_watts ~ moving_time + distance_km +average_speed + elevation_gain + average_grade + kilojoules,
							 data = strava_models),
	"watts2" = lm(average_watts ~ distance_km +average_speed + elevation_gain + average_grade + kilojoules,
								data = strava_models),
	"kilojoules" = lm(kilojoules ~ moving_time + distance_km +average_speed + elevation_gain + average_grade + average_watts,
										data = strava_models),
	"kilojoules2" = lm(kilojoules ~ moving_time + average_speed + elevation_gain + average_grade + average_watts,
										 data = strava_models))
modelsummary(ride_models, stars = TRUE, gof_omit = "IC|Adj|F|RMSE|Log")

# Plot actual v predicted
# create dataframes with actual and predicted values
ride_models_time <- data.frame(Predicted = predict(ride_models$time),
															 Observed = strava_models$moving_time)
ride_models_watts <- data.frame(Predicted = predict(ride_models$watts2),
															 Observed = strava_models$average_watts)
ride_models_joules <- data.frame(Predicted = predict(ride_models$kilojoules2),
																Observed = strava_models$kilojoules)


# plot predicted values and actual values
ggplot(ride_models_time, aes(x = Predicted, y = Observed)) +
	geom_point() +
	geom_smooth() +
#	geom_line(aes(y = Predicted), linetype = 2, color = "blue") +
	labs(title = "Predicted vs Observed - Time Model") +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5),
				axis.text.x = element_markdown(),
				axis.text.y = element_blank())

	ggplot(ride_models_watts, aes(x = Predicted, y = Observed)) +
	geom_point() +
	geom_smooth() +
	#	geom_line(aes(y = Predicted), linetype = 2, color = "blue") +
	labs(title = "Predicted vs Observed - Watts Model") +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5),
				axis.text.x = element_markdown(),
				axis.text.y = element_blank())

ggplot(ride_models_joules, aes(x = Predicted, y = Observed)) +
	geom_point() +
	geom_smooth() +
	#	geom_line(aes(y = Predicted), linetype = 2, color = "blue") +
	labs(title = "Predicted vs Observed - Kilojoules Model") +
	theme_minimal() +
	theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_markdown(hjust = 0.5),
				axis.text.x = element_markdown(),
				axis.text.y = element_blank())

###### code not needed
# pmap(
# 	actnames_set, ~plot_scatter_lm(
# 		data = strava_data_filter,
# 		var1 = .x, var2 = .y
# 	)
# )
#
# # produces each plot as its own image
# map2(
# 	c("elapsed_time", "average_speed","average_watts", "calories", "kilojoules"),
# 	c("moving_time", "average_grade", "moving_time","moving_time","moving_time"),
# 	~plot_scatter_lm(
# 		data = strava_activities_rides, var1 = .x, var2 = .y,
# 		color = "gear_name", pointsize = 3.5
# 	)
# )
