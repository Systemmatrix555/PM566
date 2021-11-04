PM 566 Lab 5
================
Chris Hanson
9/24/2021

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
  atm.press = mean(atm.press, na.rm = TRUE),
  lon = mean(lon, na.rm = TRUE),
  lat = mean(lat, na.rm = TRUE)
), by = USAFID]
```

Now, we need to calculate the median per variable, nation-wide.

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

    ##    USAFID     temp  wind.sp atm.press     lon    lat   temp_dist
    ## 1: 720458 23.68173 1.209682       NaN -82.637 37.751 0.002328907

``` r
#calculate which station is closest to the median wind speed value
station_averages[, wind.sp_dist := abs(wind.sp - medians$wind.sp_50)]
median_wind.sp_station <- station_averages[order(wind.sp_dist)][1]
median_wind.sp_station
```

    ##    USAFID     temp  wind.sp atm.press     lon    lat temp_dist wind.sp_dist
    ## 1: 720929 17.43278 2.461838       NaN -91.981 45.506  6.251284            0

``` r
#calculate which station is closest to the median atmospheric pressure value
station_averages[, atm.press_dist := abs(atm.press - medians$atm.press_50)]
median_atm.press_station <- station_averages[order(atm.press_dist)][1]
median_atm.press_station
```

    ##    USAFID     temp  wind.sp atm.press       lon     lat temp_dist wind.sp_dist
    ## 1: 722238 26.13978 1.472656  1014.691 -85.66667 31.3499  2.455719    0.9891817
    ##    atm.press_dist
    ## 1:   0.0005376377

The station that is closest to the national temp median is 720458. The
station that is closest to the national wind speed median is 720929. The
station that is closest to the national atmospheric pressure median is
722238. These do not coincide.

## Question 2. What is the most representative (median) station per state? This time, instead of looking at one variable at a time, look at the euclidean distance.

``` r
#We don't have state information in this station_averages data set, so we'll merge it in from met.
station_averages <- 
  merge(x = station_averages, y=stations, by.x = "USAFID", by.y = "USAF", all.x = TRUE, all.y = FALSE)
```

``` r
#Calculating median values of our data per state
station_averages[, temp_50 := quantile(temp, probs = 0.5, na.rm = TRUE), by = STATE]
station_averages[, wind.sp_50 := quantile(wind.sp, probs = 0.5, na.rm = TRUE), by = STATE]
station_averages[, atm.press_50 := quantile(atm.press, probs = 0.5, na.rm = TRUE), by = STATE]
station_averages[, lat_50 := quantile(lat, probs = 0.5, na.rm = TRUE), by = STATE]
station_averages[, lon_50 := quantile(lon, probs = 0.5, na.rm = TRUE), by = STATE]
```

Now, we compute the euclidean distance:
$\\sqrt{\\sum\_i(x\_i - y\_i)^2}$

``` r
#calculating the distance of each weather value to the state median value, then using this to find the euclidean distance to the value best representing the median of all 3. However, we're not including atmospheric distance because there are so many NA's.
station_averages[, eudist := sqrt(
  (temp - temp_50)^2 + (wind.sp - wind.sp_50)^2
)]

#and euclidean distance for lat/lon
station_averages[, eulatlon := sqrt(
  (lon - lon_50)^2 + (lat - lat_50)^2
)]
```

``` r
#calculate the station closest to the true median of both wind speed and temp per state
state_ordered <- station_averages[
  with(station_averages, order(STATE, eudist))
]
state_median_stations <- station_averages[ , .SD[which.min(eudist)], by = STATE]
state_med_stations <- state_median_stations[, .(USAFID, STATE)]

#calculate the station closest to the true middle of the state
state_middle_stations <- station_averages[ , .SD[which.min(eulatlon)], by = STATE]
state_mid_stations <- state_middle_stations[, .(USAFID, STATE)]
```

The stations that are closest to the shared median of wind speed and
temperature in each state are:

``` r
#knitr::kable(state_med_stations)
```

The stations that are closest to the center of each state are:

``` r
#knitr::kable(state_mid_stations)
```

## Question 3. For each state, identify what is the station that is closest to the mid-point of the state. Combining these with the stations you identified in the previous question, use leaflet() to visualize all \~100 points in the same figure, applying different colors for those identified in this question.

``` r
#Use leaflet() to visualize each state's midpoint station along with the stations from question 2.
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  addCircles(data = state_middle_stations, lat = ~lat, lng = ~lon, opacity = 1, fillOpacity = 1, radius = 400, color = 'Red') %>%
  addCircles(data = state_median_stations, lat = ~lat, lng = ~lon, opacity = 1, fillOpacity = 1, radius = 400)
```

<div id="htmlwidget-a8939e3e25b32463e6fa" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-a8939e3e25b32463e6fa">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addProviderTiles","args":["CartoDB.Positron",null,null,{"errorTileUrl":"","noWrap":false,"detectRetina":false}]},{"method":"addCircles","args":[[36.3189948519949,31.0831530984204,43.322,34.283,40.4827108433735,38.7040028195489,35.6,42.3777333333333,47.1038340767172,32.6332458296752,44.859130600572,32.383,40.711,35.5820807424594,37.4,41.691,40.217,40.9615540935673,43.500262987013,44.3590049261084,39,39.3322308300395,33.4668795483061,35.4169039487727,43.0643970117396,30.558,37.578,28.29,39.05,40.28,40.033,35.0029964747356,38.0676310679612,48.39,44.5340018796992,32.3202025316456,41.5099990825688,39.6009961832061,40.219,44.3810077071291,36.009,41.7010332434861,41.597,41.876,39.1329054054054,43.5672086956522,44.533,45.8054722474977],[-119.628,-97.6831530984204,-84.688,-80.567,-88.9483614457831,-93.1829934210526,-92.45,-122.870865740741,-122.286834076717,-83.5997190517998,-94.382130600572,-86.3506071794872,-86.375,-79.101,-77.517,-93.566,-76.851,-98.3142660818713,-114.299737012987,-89.8370098522168,-80.274,-76.4166703557312,-111.732899623588,-97.3831921024546,-108.456947705443,-92.099,-84.77,-81.437,-105.510043030031,-83.115,-74.3501562130177,-105.662009400705,-97.275,-100.024,-72.614,-90.0789738924051,-72.8280009174312,-116.005020356234,-111.723,-100.285004816956,-86.52,-74.795,-71.412,-71.021,-75.4669684684685,-71.4325130434783,-69.6672303618711,-108.540024567789],400,null,null,{"interactive":true,"className":"","stroke":true,"color":"Red","weight":5,"opacity":1,"fill":true,"fillColor":"Red","fillOpacity":1},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]},{"method":"addCircles","args":[[33.8126445959104,32.969,42.2669724409449,33.587,40.1999540383757,36.9,35.135,45.417,46.6827613941019,33.3550061728395,44.969,33.212,40.412,35.937,36.666,41.674,40.7682093695778,41.4337808219178,42.5420088495575,43.4172000899281,39,38.5332464285714,32.1669504405286,36.75,44.381,29.445,37.578,28.29,39.90066615266,39.84,40.033,36.744,39.0502172096909,46.9419577656676,43.344,31.1829822852081,41.7360101010101,39.417,41.196,45.443765323993,35.178,42.571,41.5329991281604,42.584,39.6740047984645,43.626,44.45,45.6980046136102],[-118.146523855891,-96.836,-84.466968503937,-80.209,-87.5998161535029,-94.017,-90.234,-123.817,-122.983,-84.5670154320988,-95.71,-87.616,-86.937,-77.547,-76.321,-93.022,-80.3981859456333,-97.3496356164383,-113.766053097345,-88.1327999100719,-80.274,-76.0328767857143,-110.883,-97.35,-106.721002247191,-90.261,-84.77,-81.437,-105.116074016962,-83.84,-74.3501562130177,-108.229,-96.7668696741855,-98.018,-72.5179990974729,-90.4710035429584,-72.6509797979798,-118.715772727273,-112.01100124533,-98.413442206655,-86.066,-77.713,-71.2829991281604,-70.917996007984,-75.6060009596929,-72.3049961389961,-68.3667746192893,-110.440038062284],400,null,null,{"interactive":true,"className":"","stroke":true,"color":"#03F","weight":5,"opacity":1,"fill":true,"fillColor":"#03F","fillOpacity":1},null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null,null]}],"limits":{"lat":[28.29,48.39],"lng":[-123.817,-68.3667746192893]}},"evals":[],"jsHooks":[]}</script>

## Question 4.

Using the quantile() function, generate a summary table that shows the
number of states, average temperature, wind-speed, and atmospheric
pressure by the variable “average temperature level,” which you’ll need
to create.

Start by computing the states’ average temperature. Use that measurement
to classify them according to the following criteria:

Low: temp &lt; 20 Mid: temp &gt;= 20 and temp &lt; 25 High: temp &gt;=
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
included, and mean temperature, wind-speed, and atmospheric pressure.
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
