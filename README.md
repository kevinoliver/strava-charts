This creates some charts from your Strava runs using R and ggplot. 

It was inspired by [a Reddit post](https://www.reddit.com/r/Strava/comments/yc7qqy/visualising_12_months_of_running_with_strava/) (with better visuals) and [someone else who learned some R by way of their Strava data](https://towardsdatascience.com/using-r-to-analyse-my-strava-data-fc57188b4c51).

Here's what a couple years of my data looks like.

![distance](images/distance.png)

![elevation](images/elevation.png)

![pace](images/pace.png)

![heart rate](images/heartrate.png)

## Usage

You need to have [R installed](https://cran.r-project.org).

Then, [download an export of your Strava activities](https://support.strava.com/hc/en-us/articles/216918437-Exporting-your-Data-and-Bulk-Export) in CSV format. 

Move the `activities.csv` file to your `~/downloads` directory.

Run the script to generate the charts into your `~/downloads` folder:
```
$ R < analysis.R --no-save
```
