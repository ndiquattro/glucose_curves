library(tidyverse)
library(lubridate)
library(googlesheets)

# Gather Data ---------------------------------------------------------------------------------

curve_sheet <- gs_key("1K4j47nkzR_DrpAS0V3eArqsbt3Ro9Ads3K51h5Yr2co")

all_curves <-
  map_df(seq(1, nrow(curve_sheet$ws)), ~gs_read(curve_sheet, .)) %>%
  mutate(
    time = as_datetime(time),
    day = as_date(time),
    day_norm = `date<-`(time, as.Date("1970-01-01")),
  ) %>%
  group_by(day) %>%
  mutate(
    time_shot = time_length(time[event == 'insulin'] %--% time, "hour")
  ) %>%
  ungroup()

# Plot Functions ------------------------------------------------------------------------------

title_stamp <- stamp("January 1, 2018 - Pacino's Glucose Curve", quiet = TRUE)

curve_plot <- function(curve_df) {
  # Set Data
  glucose <- curve_df %>% filter(!is.na(glucose))
  events <- curve_df %>% filter(is.na(glucose))

  # Set limits
  time_limits <- c(curve_df$time[1], curve_df$time[1])
  hour(time_limits[1]) <- 6
  hour(time_limits[2]) <- 18

  # Plot
  glucose %>%
    ggplot(aes(time, glucose)) +
      geom_rect(
        ymin = 100, ymax = 250, xmin = time_limits[1], xmax = time_limits[2],
        fill = "green", alpha = .1) +
      geom_line() +
      geom_point() +
      geom_vline(aes(xintercept = time, color = event), events) +
      scale_x_datetime(
        date_labels = "%I:%M %p",
        date_breaks = "2 hour",
        limits = time_limits
      ) +
      expand_limits(y = 0) +
      labs(
        x = "Time", y = "Blood Glucose (mg/dl)",
        color = "Events",
        title = title_stamp(glucose$time[1]),
        subtitle = str_glue("Insulin Dose: {unique(ws$insulin_dose)} - Food Amount: {unique(ws$food_grams)}g")
      ) +
      theme(legend.position = "bottom")
}

# Make Curve Plots ----------------------------------------------------------------------------

curve_plot(all_curves %>% filter(day == "2018-12-08"))

all_curves %>%
  filter(!is.na(glucose)) %>%
  ggplot(aes(day_norm, glucose, color = factor(day))) +
  geom_rect(
    ymin = 100, ymax = 250, xmin = make_datetime(hour = 6), xmax = make_datetime(hour = 24),
    fill = "green", inherit.aes = FALSE, alpha = .1) +
    geom_line() +
    geom_point() +
    scale_x_datetime(date_labels = "%I:%M %p", date_breaks = "2 hour") +
    scale_color_discrete(labels = function(x) strftime(x, "%b %d")) +
    expand_limits(y = 0) +
    labs(
      x = "Time",
      y = "Blood Glucose (mg/dl)",
      color = "Day of Curve",
      title = "All Curves"
    )


# Table Format --------------------------------------------------------------------------------

all_curves %>%
  filter(day == max(day)) %>%
  select(time, glucose, event) %>%
  mutate(time = strftime(time, "%I:%M %p", tz = "UTC"))

