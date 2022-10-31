# Load the csv file into activities <- read_csv or something of "Documents/sports/Strava Export 20221024/activities.csv"
#
# ggplot intro:
# https://ggplot2.tidyverse.org/reference/ggtheme.html
#
# cheatsheet:
# https://raw.githubusercontent.com/rstudio/cheatsheets/main/data-visualization-2.1.pdf
# ggplot2 themes:
# https://ggplot2.tidyverse.org/reference/ggtheme.html
# color names:
# https://www.datanovia.com/en/blog/awesome-list-of-657-r-color-names/
# 

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
activities$SecondsPerMile <- activities$`Moving Time` / activities$DistanceMiles 
# Elevation in Feet
activities$ElevationGainFeet <- activities$`Elevation Gain` * 3.281


# Filter down to just runs (and after i "started" running in August)
runs <- activities[activities$ActivityType == 'Run', ]
runs <- runs[as.Date(runs$ActDate) >= as.Date("2019-08-01"), ]

# Data frame of per week sums
weekly_runs <- runs %>%
  group_by(ActFirstDayOfWeek) %>%
  summarize(Distance = sum(Distance), ElevationGainFeet = sum(ElevationGainFeet))


# Histogram by distance
hist(runs$Distance, main = "Histogram of Distances", xlab = 'Distance (km)')

# for ggplot
library(ggplot2) 
# for %>%
library(dplyr)

# Scatter Plot 
ggplot(data=runs, aes(x = as.Date(ActDate), y = Distance)) +
    geom_point() +
    theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank())+ 
    theme(plot.title = element_text(hjust = 0.5))+
    labs(title = "Activities",
         colour = "ActivityType",
         y = "Distance (km)",
         x = "Date" + theme_bw(base_size = 15))

# Bar plot of distance by month
barplot(t(rowsum(runs$Distance,
                 format(runs$ActDate, "%Y-%m"))), 
        las=2, col = "darkolivegreen3",
        cex.lab=0.75, cex.axis=0.75, cex.names = 0.75,
                xlab="Month",
                ylab= "Distance (KM)"
                )


# Boxplot of distance
boxplot(runs$Distance,
            data=runs,
            xlab="Activity Type",
            ylab="Distance (km)",
            cex.lab=0.75,
            cex.axis=0.75,
            col="lightsalmon",
            border="lightsalmon3"
    )


# Distance per run 
# make the bars wider??
ggplot(data = runs, mapping = aes(x = as.Date(ActDate), y = Distance)) +
  geom_col() +
  theme_light() +
  labs(title = "Distance (km)", x = element_blank(), y = element_blank()) +
  theme(
    axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(colour = "grey20", size = 12),
    strip.text = element_text(face = "italic"),
    text = element_text(size = 16)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL)

# Then distance per week
weekly_runs <- runs %>%
  group_by(ActFirstDayOfWeek) %>%
  summarize(weekly_distance = sum(Distance))
ggplot(data = weekly_runs, mapping = aes(x = as.Date(ActFirstDayOfWeek), y = Distance)) +  geom_step(size=0.1, color = "red")  


# Plot both distance per run and week, together
# todo: add labels for timing of plantar fasciatis and travel
ggplot() +   geom_step(data = weekly_runs, 
    mapping = aes(x = as.Date(ActFirstDayOfWeek), y = Distance, color = "coral1"),     linetype = "F1", 
    alpha = .75,    size = 0.8) +   geom_col(data = runs, 
    mapping = aes(x = as.Date(ActDate), y = Distance, color="cyan1"), 
    alpha = .25,    size = 0.1) +   labs(
    title = "Distance", 
    subtitle = "Distance per run and per week, km",
    x = element_blank(), 
    y = element_blank()) +  theme_light() +  theme(
    plot.title = element_text(colour = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = c("right", "bottom"),
    legend.key = element_blank(), 
    legend.key.width = unit(4, "pt"),
    legend.text = element_text(colour = "goldenrod4"),    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),    axis.text.y = element_text(colour = "goldenrod4", size = 10),
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


# Pace, in minutes per mile along with a smoothed trend line
ggplot(data = runs, mapping = aes(x = as.Date(ActDate), y = SecondsPerMile)) +  geom_point(stat = "identity", show.legend = FALSE, color = "darkgoldenrod1") +  geom_smooth(method = "gam", show.legend = FALSE, se = FALSE, color = "darkgoldenrod3") +  scale_y_time(labels = function(x) strftime(x, "%M:%S")) +  labs(    title = "Pace",     subtitle = "Average pace per run, minutes per mile",    x = element_blank(),     y = element_blank()) +  theme_light() +  theme(    plot.title = element_text(colour = "goldenrod4", face = "bold"),    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),    axis.text.y = element_text(colour = "goldenrod4", size = 10),    axis.ticks = element_blank(), # remove tick marks     panel.grid.major.x = element_blank(), # remove vertical grid lines    panel.border = element_blank() # remove border around graph  ) +   scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_color_identity(breaks = c("darkgoldenrod1", "darkgoldenrod3"))


# Average Heart Rate
ggplot(data = runs, mapping = aes(x = as.Date(ActDate), y = `Average Heart Rate`)) +  geom_point(stat = "identity", show.legend = FALSE, color = "darkgoldenrod1") +  geom_smooth(method = "gam", show.legend = FALSE, se = FALSE, color = "darkgoldenrod3") +  labs(    title = "Heart rate",     subtitle = "Average heart rate per run, beats per minute",    x = element_blank(),     y = element_blank()) +  theme_light() +  theme(    plot.title = element_text(colour = "goldenrod4", face = "bold"),    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),    axis.text.y = element_text(colour = "goldenrod4", size = 10),    axis.ticks = element_blank(), # remove tick marks     panel.grid.major.x = element_blank(), # remove vertical grid lines    panel.border = element_blank() # remove border around graph  ) + 
  ylim(130, NA) +  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_color_identity(breaks = c("darkgoldenrod1", "darkgoldenrod3"))



# Plot elevation gain per run and week, together
# todo: add labels for timing of plantar fasciatis and travel
ggplot() +   geom_step(data = weekly_runs, 
    mapping = aes(x = as.Date(ActFirstDayOfWeek), y = ElevationGainFeet, color = "coral1"),     linetype = "F1", 
    alpha = .75,    size = 0.8) +   geom_col(data = runs, 
    mapping = aes(x = as.Date(ActDate), y = ElevationGainFeet, color="cyan1"), 
    alpha = .25,    size = 0.1) +   labs(
    title = "Elevation", 
    subtitle = "Elevation gain per run and per week, feet",
    x = element_blank(), 
    y = element_blank()) +  theme_light() +  theme(
    plot.title = element_text(colour = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(colour = "goldenrod4", face = "bold"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = c("right", "bottom"),
    legend.key = element_blank(), 
    legend.key.width = unit(4, "pt"),
    legend.text = element_text(colour = "goldenrod4"),    axis.text.x = element_text(colour = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),    axis.text.y = element_text(colour = "goldenrod4", size = 10),
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

# Plot cumulative distance
ggplot(data = runs, aes(x = as.Date(ActDate), y = cumsum(Distance))) +
  geom_line() +
  labs(
    title = "Cumulative Distance", 
    subtitle = "Cumulative Distance run, km",
    x = element_blank(), 
    y = element_blank()) +  theme_light() +  theme(
    plot.title = element_text(color = "goldenrod4", face = "bold"),
    plot.subtitle = element_text(color = "goldenrod4", face = "bold"),
    legend.title = element_blank(),
    axis.text.x = element_text(color = "goldenrod4", size = 10, angle = 90, hjust = 1, vjust = 0.5),    axis.text.y = element_text(color = "goldenrod4", size = 10),
    axis.ticks = element_blank(), # remove tick marks 
    panel.grid.major.x = element_blank(), # remove vertical grid lines
    panel.border = element_blank() # remove border around graph
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", minor_breaks = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) # removes the padding under the x-axis


# high resolution rendering
tiff("/Users/kevin/Documents/Sports/Strava Export 20221024/visualizations/test.tiff", units="in", width=6, height=4, res=300)
# ggplot code goes here.
dev.off()

