# This creates some charts of from your Strava runs R and ggplot. 
#
# Inspired by https://www.reddit.com/r/Strava/comments/yc7qqy/visualising_12_months_of_running_with_strava/
# and https://towardsdatascience.com/using-r-to-analyse-my-strava-data-fc57188b4c51
#
# You will need to download an export of your Strava activities in CSV format. See instructions:
# https://support.strava.com/hc/en-us/articles/216918437-Exporting-your-Data-and-Bulk-Export
#
# Run it from the command-line:
# $ R < analysis.R --no-save

install.packages("hms", dependencies = TRUE, repos = "http://cran.us.r-project.org")
install.packages("ggplot2", dependencies = TRUE, repos = "http://cran.us.r-project.org")

# Load the csv file
activities <- read.csv("~/downloads/activities.csv")

# Do some minimal clean up on the data to make things easier.

# Rename some columns
colnames(activities)[2] <- "ActivityDate"
colnames(activities)[4] <- "ActivityType"
colnames(activities)[7] <- "Distance"

# Create a date only column and date-time column
activities$ActDate <- strptime(as.character(activities$ActivityDate), "%B %d, %Y")
activities$ActDateAndTime <- strptime(as.character(activities$ActivityDate), "%B %d, %Y, %H:%M:%S")
# Lump the dates into the first day of the week
activities$ActFirstDayOfWeek <- cut.Date(as.Date(activities$ActDateAndTime), "week")
# Distance in Miles
activities$DistanceMiles <- activities$Distance / 1.609
# Pace
activities$SecondsPerMile <- activities$`Moving.Time` / activities$DistanceMiles
# Elevation in Feet
activities$ElevationGainFeet <- activities$`Elevation.Gain` * 3.281

# Filter down to just runs (and after i "started" running in August)
runs <- activities[activities$ActivityType == 'Run', ]

# I had a few random runs before this and dropping them made the visualizations better
runs <- runs[as.Date(runs$ActDate) >= as.Date("2019-08-01"), ]

library(ggplot2) # for ggplot
library(dplyr) # for %>%

# Data frame of per week sums
weekly_runs <- runs %>%
  group_by(ActFirstDayOfWeek) %>%
  summarize(Distance = sum(Distance), ElevationGainFeet = sum(ElevationGainFeet))

# high resolution rendering
png("~/downloads/distance.png", units="in", width=6, height=4, res=300)
# Plot both distance per run and week, together
ggplot() + 
  geom_step(data = weekly_runs, 
    mapping = aes(x = as.Date(ActFirstDayOfWeek), y = Distance, color = "coral1"), 
    linetype = "F1", 
    alpha = .75,
    linewidth = 0.8) + 
  geom_col(data = runs, 
    mapping = aes(x = as.Date(ActDate), y = Distance, color="cyan1"), 
    alpha = .25,
    linewidth = 0.1) + 
  labs(
    title = "Distance", 
    subtitle = "Distance per run and per week, km",
    x = element_blank(), 
    y = element_blank()) +
  theme_light() +
  theme(
    plot.title = element_text(colour = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = c("right", "bottom"),
    legend.key = element_blank(), 
    legend.key.width = unit(4, "pt"),
    legend.text = element_text(colour = "goldenrod4"),
    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(colour = "goldenrod4", size = 10),
    axis.ticks = element_blank(), # remove tick marks 
    panel.grid.major.x = element_blank(), # remove vertical grid lines
    panel.border = element_blank() # remove border around graph
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # removes the padding under the x-axis
  scale_color_identity(
    breaks = c("cyan1", "coral1"),
    labels = c("per run", "per week"),
    guide = guide_legend(override.aes = list(alpha = 0.5, linetype = c(1, 1), size = 5, fill = "white")))
dev.off()

# Pace, in minutes per mile along with a smoothed trend line
png("~/downloads/pace.png", units="in", width=6, height=4, res=300)
ggplot(data = runs, mapping = aes(x = as.Date(ActDate), y = SecondsPerMile)) +
  geom_point(stat = "identity", show.legend = FALSE, color = "darkgoldenrod1") +
  geom_smooth(method = "gam", show.legend = FALSE, se = FALSE, color = "darkgoldenrod3") +
  scale_y_time(labels = function(x) strftime(x, "%M:%S")) +
  labs(
    title = "Pace", 
    subtitle = "Average pace per run, minutes per mile",
    x = element_blank(), 
    y = element_blank()) +
  theme_light() +
  theme(
    plot.title = element_text(colour = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),
    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(colour = "goldenrod4", size = 10),
    axis.ticks = element_blank(), # remove tick marks 
    panel.grid.major.x = element_blank(), # remove vertical grid lines
    panel.border = element_blank() # remove border around graph
  ) + 
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_color_identity(breaks = c("darkgoldenrod1", "darkgoldenrod3"))
dev.off()

# Average Heart Rate
png("~/downloads/heartrate.png", units="in", width=6, height=4, res=300)
ggplot(data = runs, mapping = aes(x = as.Date(ActDate), y = `Average.Heart.Rate`)) +
  geom_point(stat = "identity", show.legend = FALSE, color = "darkgoldenrod1") +
  geom_smooth(method = "gam", show.legend = FALSE, se = FALSE, color = "darkgoldenrod3") +
  labs(
    title = "Heart rate", 
    subtitle = "Average heart rate per run, beats per minute",
    x = element_blank(), 
    y = element_blank()) +
  theme_light() +
  theme(
    plot.title = element_text(colour = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),
    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(colour = "goldenrod4", size = 10),
    axis.ticks = element_blank(), # remove tick marks 
    panel.grid.major.x = element_blank(), # remove vertical grid lines
    panel.border = element_blank() # remove border around graph
  ) + 
  ylim(130, NA) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_color_identity(breaks = c("darkgoldenrod1", "darkgoldenrod3"))
dev.off()

# Plot elevation gain per run and week, together
png("~/downloads/elevation.png", units="in", width=6, height=4, res=300)
ggplot() + 
  geom_step(data = weekly_runs, 
    mapping = aes(x = as.Date(ActFirstDayOfWeek), y = ElevationGainFeet, color = "coral1"), 
    linetype = "F1", 
    alpha = .75,
    linewidth = 0.8) + 
  geom_col(data = runs, 
    mapping = aes(x = as.Date(ActDate), y = ElevationGainFeet, color="cyan1"), 
    alpha = .25,
    linewidth = 0.1) + 
  labs(
    title = "Elevation", 
    subtitle = "Elevation gain per run and per week, feet",
    x = element_blank(), 
    y = element_blank()) +
  theme_light() +
  theme(
    plot.title = element_text(colour = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = c("right", "bottom"),
    legend.key = element_blank(), 
    legend.key.width = unit(4, "pt"),
    legend.text = element_text(colour = "goldenrod4"),
    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(colour = "goldenrod4", size = 10),
    axis.ticks = element_blank(), # remove tick marks 
    panel.grid.major.x = element_blank(), # remove vertical grid lines
    panel.border = element_blank() # remove border around graph
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + # removes the padding under the x-axis
  scale_color_identity(
    breaks = c("cyan1", "coral1"),
    labels = c("per run", "per week"),
    guide = guide_legend(override.aes = list(alpha = 0.5, linetype = c(1, 1), size = 5, fill = "white")))
dev.off()

# Plot cumulative distance
png("~/downloads/cumulative.png", units="in", width=6, height=4, res=300)
ggplot(data = runs, aes(x = as.Date(ActDate), y = cumsum(Distance))) +
  geom_line() +
  labs(
    title = "Cumulative Distance", 
    subtitle = "Cumulative Distance run, km",
    x = element_blank(), 
    y = element_blank()) +
  theme_light() +
  theme(
    plot.title = element_text(color = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(color = "goldenrod4", face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(color = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(color = "goldenrod4", size = 10),
    axis.ticks = element_blank(), # remove tick marks 
    panel.grid.major.x = element_blank(), # remove vertical grid lines
    panel.border = element_blank() # remove border around graph
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # removes the padding under the x-axis
dev.off()

