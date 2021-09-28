PM 566 Lab 5
================
Chris Hanson
9/24/2021

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
rm(list = ls())
```

``` r
if (!file.exists("met_all.gz"))
  download.file(
    url = "https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz",
    destfile = "met_all.gz",
    method   = "libcurl",
    timeout  = 60
    )
met <- data.table::fread("met_all.gz")

stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

#removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

#merge the two datasets
met <- merge(
  x     = met,      
  y     = stations, 
    # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE,
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  )
```

## Question 1: What is the median station in terms of temperature, wind speed, and atmospheric pressure? Look for the three weather stations that best represent continental US using the quantile() function. Do these three coincide?

First, generate a representative version of each station. We will use
the averages (or median).

``` r
#getting the average temp, wind speed, and atmospheric pressure of each station in met (by USAFID)
station_averages <- met[,.(
  temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
), by = USAFID]
```

Now, we need to calculate the median per variable.

``` r
#using the station averages data set, find the median value using the quantile function.
medians <- station_averages[,.(
  temp_50 = quantile(temp, probs = 0.5, na.rm = TRUE),
  wind.sp_50 = quantile(wind.sp, probs = 0.5, na.rm = TRUE),
  atm.press_50 = quantile(atm.press, probs = 0.5, na.rm = TRUE)
)]

medians
```

    ##     temp_50 wind.sp_50 atm.press_50
    ## 1: 23.68406   2.461838     1014.691

Now we can find the stations that are the closest to the median
locations.

``` r
#calculate which station is closest to the median temp value
station_averages[, temp_dist := abs(temp - medians$temp_50)]
median_temp_station <- station_averages[order(temp_dist)][1]
median_temp_station
```

    ##    USAFID     temp  wind.sp atm.press   temp_dist
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

``` r
#calculate which station is closest to the median wind speed value
station_averages[, wind.sp_dist := abs(wind.sp - medians$wind.sp_50)]
median_wind.sp_station <- station_averages[order(wind.sp_dist)][1]
median_wind.sp_station
```

    ##    USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist
    ## 1: 720929 17.43278 2.461838       NaN  6.251284            0

``` r
#calculate which station is closest to the median atmospheric pressure value
station_averages[, atm.press_dist := abs(atm.press - medians$atm.press_50)]
median_atm.press_station <- station_averages[order(atm.press_dist)][1]
median_atm.press_station
```

    ##    USAFID     temp  wind.sp atm.press temp_dist wind.sp_dist atm.press_dist
    ## 1: 722238 26.13978 1.472656  1014.691  2.455719    0.9891817   0.0005376377

The station that is closest to the temp median is 720458. The station
that is closest to the wind speed median is 720929. The station that is
closest to the atmospheric pressure median is 722238.

## Question 2. What is the most representative (median) station per state? This time, instead of looking at one variable at a time, look at the euclidean distance. If multiple stations show in the median, select the one located at the lowest latitude.

``` r
#We don't have state information in this station_averages data set, so we'll merge it in from met.
station_averages <- 
  merge(x = station_averages, y=stations, by.x = "USAFID", by.y = "USAF", all.x = TRUE, all.y = FALSE)
```

``` r
#Calculating median values of weather data per state
station_averages[, temp_50 := quantile(temp, probs = 0.5, na.rm = TRUE), by = STATE]
station_averages[, wind.sp_50 := quantile(wind.sp, probs = 0.5, na.rm = TRUE), by = STATE]
station_averages[, atm.press_50 := quantile(atm.press, probs = 0.5, na.rm = TRUE), by = STATE]
```

Now, we compute the euclidean distance:
$\\sqrt{\\sum\_i(x\_i - y\_i)^2}$

``` r
#calculating the distance of each weather value to the state median value, then using this to find the euclidean distance to the value best representing the median of all 3. However, we're not including atmospheric distance because there are so many NA's.
station_averages[, eudist := sqrt(
  (temp - temp_50)^2 + (wind.sp - wind.sp_50)^2
)]
```

``` r
#calculate the station closest to the true median of both wind speed and temp per state
state_ordered <- station_averages[
  with(station_averages, order(STATE, eudist))
]
state_median_stations <- station_averages[ , .SD[which.min(eudist)], by = STATE]
```

The stations that are closest to the shared median of wind speed and
temperature in each state are:

``` r
knitr::kable(state_median_stations[, .(USAFID, STATE)])
```

| USAFID | STATE |
|-------:|:------|
| 722970 | CA    |
| 722598 | TX    |
| 725395 | MI    |
| 723107 | SC    |
| 722076 | IL    |
| 720479 | MO    |
| 722054 | AR    |
| 720202 | OR    |
| 720254 | WA    |
| 722197 | GA    |
| 726553 | MN    |
| 722286 | AL    |
| 724386 | IN    |
| 720864 | NC    |
| 724006 | VA    |
| 725464 | IA    |
| 725204 | PA    |
| 725565 | NE    |
| 725867 | ID    |
| 726413 | WI    |
| 720328 | WV    |
| 722218 | MD    |
| 722745 | AZ    |
| 720625 | OK    |
| 726654 | WY    |
| 722041 | LA    |
| 720448 | KY    |
| 722011 | FL    |
| 724699 | CO    |
| 724295 | OH    |
| 724090 | NJ    |
| 723658 | NM    |
| 724550 | KS    |
| 720911 | ND    |
| 726115 | VT    |
| 722358 | MS    |
| 725087 | CT    |
| 724885 | NV    |
| 725750 | UT    |
| 726590 | SD    |
| 720974 | TN    |
| 724988 | NY    |
| 725079 | RI    |
| 725088 | MA    |
| 724180 | DE    |
| 726116 | NH    |
| 726077 | ME    |
| 726798 | MT    |

## Question 3. For each state, identify what is the station that is closest to the mid-point of the state. Combining these with the stations you identified in the previous question, use leaflet() to visualize all \~100 points in the same figure, applying different colors for those identified in this question.

``` r
#Use leaflet() to visualize each state's midpoint station along with the stations from question 2.
```

## Question 4.

Using the quantile() function, generate a summary table that shows the
number of states, average temperature, wind-speed, and atmospheric
pressure by the variable “average temperature level,” which you’ll need
to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

low: temp &lt; 20 Mid: temp &gt;= 20 and temp &lt; 25 High: temp &gt;=
25

``` r
#calculating the mean temperature of each state using by = STATE
met[, state_temp := mean(temp, na.rm = TRUE), by = STATE]

#categorizing the temperature of each state into low, mid, or high
met[, temp_cat := fifelse(
  state_temp < 20, "low-temp",
    fifelse(state_temp < 25, "mid-temp", "high-temp"))
    ]
```

Calculate the number of NA entries:

``` r
#making a table of the counts of each value, to ensure the count of NA is 0
table(met$temp_cat, useNA = "always")
```

    ## 
    ## high-temp  low-temp  mid-temp      <NA> 
    ##    811126    430794   1135423         0

Compute the following:

Number of entries (records), Number of stations, Number of states
included, and Mean temperature, wind-speed, and atmospheric pressure.
All by the levels described before.

``` r
#creating a new matrix "tab" that has counts of the number of each low, mid, and high temp READING as "N_entries" and also counts the number of each STATION as N_stations.
tab <- met[, .(
  N_entries = .N,
  N_stations = length(unique(USAFID)),
  mean_temp = mean(temp, na.rm = TRUE),
  wind.sp = mean(wind.sp, na.rm = TRUE),
  atm.press = mean(atm.press, na.rm = TRUE)
),
    by = temp_cat]

knitr::kable(tab)
```

| temp\_cat | N\_entries | N\_stations | mean\_temp |  wind.sp | atm.press |
|:----------|-----------:|------------:|-----------:|---------:|----------:|
| mid-temp  |    1135423 |         781 |   22.39909 | 2.352712 |  1014.383 |
| high-temp |     811126 |         555 |   27.75066 | 2.514644 |  1013.738 |
| low-temp  |     430794 |         259 |   18.96446 | 2.637410 |  1014.366 |
