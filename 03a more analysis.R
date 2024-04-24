## plotting rides by unique days in tiled grid for the year

library(tidyverse) # to do tidyverse things
library(tidylog) # to get a log of what's happening to the data
library(janitor) # tools for data cleaning
library(gt) # tables!
library(ggtext) # ggplot text helpers
# some custom functions
source("~/Data/r/basic functions.R")



## unique days
# using code from https://github.com/curatedmess/TidyTuesday/blob/main/2024/01022024/runs_2024.R

# get data ready
strava_data <- readRDS("data/strava_activities_final.rds") %>%
	mutate(activity_hour2 = activity_hour + 1)
glimpse(strava_data)


ridedates1 <- strava_data %>%
	filter(activity_year == 2023) %>%
	group_by(activity_date_p) %>%
	mutate(rides_day = n()) %>%
	ungroup() %>%
	select(distance_km, year = activity_year, month = activity_month_t, day = activity_wday, date = activity_date_p, rides_day)
glimpse(ridedates1)

# create date scaffold ----------------------------------------------------
scaffold_df <- data.frame(date = date(seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = 1)))


# create df with scaffold to show all days --------------------------------
ridedates <- full_join(ridedates1, scaffold_df) %>%
	mutate(distance_km = ifelse(is.na(distance_km), 0, distance_km)) %>%
	mutate(rides_day = ifelse(is.na(rides_day), 0, rides_day)) %>%
	group_by(date) %>%
	mutate(distance_day = sum(distance_km)) %>%
	distinct(date, .keep_all = TRUE) %>%
	ungroup() %>%
	mutate(color = case_when(distance_day == 0 ~ "#171c22",
													 distance_day > 0 & distance_day <= 4.5 ~ "#0E4429",
													 distance_day > 4.5 & distance_day <= 10 ~ "#006D32",
													 distance_day > 10 & distance_day <= 20 ~ "#26A642",
													 distance_day > 20 ~ "#39D354")) %>%
	select(date, distance_day, rides_day, color)

glimpse(ridedates)

# for grid
start_day <- as.Date("2023-01-01")
end_day <- as.Date("2023-12-31")

df_grid <- tibble(date = seq(start_day, end_day, by = "1 day")) %>%
	mutate(year = year(date),
				 month_abb = month(date, label = TRUE, abbr = TRUE),
				 day = wday(date, label = TRUE),
				 first_day_of_year = floor_date(date, "year"),
				 week_of_year = as.integer((date - first_day_of_year + wday(first_day_of_year) - 1) / 7) + 1) %>%
	left_join(ridedates) %>%
	arrange(date) %>%
	mutate(num = row_number()) %>%
	mutate(day = as.character(day)) %>%
	mutate(day = factor(day, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))) %>%
	mutate(distance_year = round(sum(distance_day), 0)) %>%
	mutate(rides_year = sum(rides_day)) %>%
	mutate(ride_day = ifelse(rides_day > 0, 1, 0)) %>%
	mutate(ride_days_unique = sum(ride_day)) %>%
	mutate(pct_days_ridden = round((ride_days_unique / 365) * 100, 1)) %>%
	select(-ride_day)

glimpse(df_grid)

df_labels <- df_grid %>%
	group_by(month_abb) %>%
	arrange(date) %>%
	filter(week_of_year == 1 | day == "Sun") %>%
	slice(1)

glimpse(df_labels)


# legend color objects ----------------------------------------------------
df_legend <- data.frame(y = c(-1, -1, -1, -1, -1),
												x = c(44, 45, 46, 47, 48),
												color = c("#171c22", "#0E4429", "#006D32", "#26A642", "#39D354"))

df_legend_labels <- data.frame(y = c(-1, -1),
															 x = c(43, 49),
															 label = c("Fewer km (black = 0)", "More km"),
															 hjust = c(1, 0))

df_legend <- data.frame(y = c(-1, -1, -1, -1, -1),
												x = c(44, 45, 46, 47, 48),
												color = c("#171c22", "#0E4429", "#006D32", "#26A642", "#39D354"))


ggplot() +
	statebins:::geom_rtile(data = df_grid,
												 aes(y = fct_rev(day), x = week_of_year, fill = color), radius = unit(1.75, "pt"),
												 color = "white", size = 1) +
	statebins:::geom_rtile(data = df_legend,
												 aes(y = y, x = x, fill = color),
												 radius = unit(1.75, "pt"), color = "#0d1117", size = 1) +
	# geom_text(data = df_grid,
	# 					aes(x = -2, y = 10,
	# 							label = paste0(rides_year, " rides this year on ", ride_days_unique,
	# 														 " unique days, or ", pct_days_ridden, "% of all days")),
	# 					#family = font,
	# 					hjust = 0, color = "#848d97", size = 4) +
	geom_text(data = df_labels, aes(x = week_of_year, y = 8, label = month_abb),
						#family = font,
						hjust = 0.3, color = "#848d97", size = 3, check_overlap = TRUE) +
	geom_text(data = df_grid, aes(x = -1.9, y = day, label = day),
						#family = font,
						color = "#848d97", size = 3, hjust = 0, check_overlap = TRUE) +
	geom_text(data = df_legend_labels, aes(x, y, label = label, hjust = hjust),
						#family = font,
						color = "#848d97", size = 3) +
	geom_text(data = df_grid,
						aes(x = 0, y = -1, label = paste0("Total kilometers ridden = ", scales::comma(distance_year))),
						#family = font,
						color = "#848d97", size = 4, hjust = 0) +
	scale_y_discrete(breaks = c("Mon", "Wed", "Fri")) +
	expand_limits(y = c(-2, 12)) +
	scale_x_continuous(expand = c(-2, NA)) +
	scale_fill_identity() +
#	facet_wrap(~factor(year, levels = c(2023, 2022, 2021, 2020)), ncol = 1) +
	labs(title = glue::glue("{df_grid$rides_year}", " rides this year on {df_grid$ride_days_unique} unique days, or {df_grid$pct_days_ridden} % of all days"),
			 subtitle = "Black square = no ride that day",
			 caption = "Strava data via rstrava app & Strava API") +
	coord_equal() +
	theme_void() +
	theme(plot.title = element_text(size = 18, vjust = -10, color = "#848d97"),
				plot.title.position = "plot",
				plot.subtitle = element_text(size = 14, vjust = -16, color = "#848d97", margin = margin(t = 8, b = 10)),
				plot.caption.position = "plot",
				plot.caption = element_text(size = 9, color = "#848d97", vjust = 5,
																		hjust = .9, margin = margin(t = 25)),
				legend.position = "none",
				plot.margin = unit(c(0.5, 1, 0.5, 1), "cm"),
				plot.background = element_rect(color = NA, fill = "#FFFFFF"))
