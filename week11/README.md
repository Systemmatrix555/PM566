PM 566 Week 11 Lab
================
Chris Hanson
11/4/2021

# Setup

``` r
library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)

dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

# Exercise 1

**Retrieve the actor ID, first name and last name for all actors using
the actor table. Sort by last name and then by first name.**

``` r
dbGetQuery(con,
"SELECT actor_id, first_name, last_name
FROM actor
ORDER BY last_name, first_name
LIMIT 10
")
```

    ##    actor_id first_name last_name
    ## 1        58  CHRISTIAN    AKROYD
    ## 2       182     DEBBIE    AKROYD
    ## 3        92    KIRSTEN    AKROYD
    ## 4       118       CUBA     ALLEN
    ## 5       145        KIM     ALLEN
    ## 6       194      MERYL     ALLEN
    ## 7        76   ANGELINA   ASTAIRE
    ## 8       112    RUSSELL    BACALL
    ## 9       190     AUDREY    BAILEY
    ## 10       67    JESSICA    BAILEY

# Exercise 2

**Retrieve the actor ID, first name, and last name for actors whose last
name equals ‘WILLIAMS’ or ‘DAVIS’.**

``` r
dbGetQuery(con,
"SELECT actor_id, first_name, last_name
FROM actor
WHERE last_name IN ('WILLIAMS', 'DAVIS')
LIMIT 10
")
```

    ##   actor_id first_name last_name
    ## 1        4   JENNIFER     DAVIS
    ## 2       72       SEAN  WILLIAMS
    ## 3      101      SUSAN     DAVIS
    ## 4      110      SUSAN     DAVIS
    ## 5      137     MORGAN  WILLIAMS
    ## 6      172    GROUCHO  WILLIAMS

# Exercise 3

**Write a query against the rental table that returns the IDs of the
customers who rented a film on July 5, 2005 (use the rental.rental\_date
column, and use the date() function to ignore the time component).
Include a single row for each distinct customer ID.**

``` r
dbGetQuery(con,
"SELECT DISTINCT customer_id, rental_date
FROM rental
WHERE date(rental_date) == '2005-07-05'
ORDER BY customer_id
LIMIT 10
")
```

    ##    customer_id         rental_date
    ## 1            8 2005-07-05 23:01:21
    ## 2           37 2005-07-05 22:56:33
    ## 3           60 2005-07-05 22:57:34
    ## 4          111 2005-07-05 23:25:54
    ## 5          114 2005-07-05 23:23:11
    ## 6          138 2005-07-05 23:13:07
    ## 7          142 2005-07-05 23:44:37
    ## 8          169 2005-07-05 23:46:19
    ## 9          242 2005-07-05 22:51:44
    ## 10         295 2005-07-05 23:59:15

# Exercise 4

## Exercise 4.1

**Construct a query that retrieves all rows from the payment table where
the amount is either 1.99, 7.99, or 9.99.**

``` r
dbGetQuery(con,
"SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)
ORDER BY payment_id
LIMIT 10
")
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16050         269        2         7   1.99 2007-01-24 21:40:19.996577
    ## 2       16056         270        1       193   1.99 2007-01-26 05:10:14.996577
    ## 3       16081         282        2        48   1.99 2007-01-25 04:49:12.996577
    ## 4       16103         294        1       595   1.99 2007-01-28 12:28:20.996577
    ## 5       16133         307        1       614   1.99 2007-01-28 14:01:54.996577
    ## 6       16158         316        1      1065   1.99 2007-01-31 07:23:22.996577
    ## 7       16160         318        1       224   9.99 2007-01-26 08:46:53.996577
    ## 8       16161         319        1        15   9.99 2007-01-24 23:07:48.996577
    ## 9       16180         330        2       967   7.99 2007-01-30 17:40:32.996577
    ## 10      16206         351        1      1137   1.99 2007-01-31 17:48:40.996577

## Exercise 4.2

**Construct a query that retrieves all rows from the payment table where
the amount is greater then 5.**

``` r
dbGetQuery(con,
"SELECT *
FROM payment
WHERE amount > 5
ORDER BY payment_id
LIMIT 10
")
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16052         269        2       678   6.99 2007-01-28 21:44:14.996577
    ## 2       16058         271        1      1096   8.99 2007-01-31 11:59:15.996577
    ## 3       16060         272        1       405   6.99 2007-01-27 12:01:05.996577
    ## 4       16061         272        1      1041   6.99 2007-01-31 04:14:49.996577
    ## 5       16068         274        1       394   5.99 2007-01-27 09:54:37.996577
    ## 6       16073         276        1       860  10.99 2007-01-30 01:13:42.996577
    ## 7       16074         277        2       308   6.99 2007-01-26 20:30:05.996577
    ## 8       16082         282        2       282   6.99 2007-01-26 17:24:52.996577
    ## 9       16086         284        1      1145   6.99 2007-01-31 18:42:11.996577
    ## 10      16087         286        2        81   6.99 2007-01-25 10:43:45.996577

# Exercise 5

**Retrieve all payment IDs and their amount from the customers whose
last name is ‘DAVIS’.**

``` r
dbGetQuery(con,
"SELECT payment_id, amount, customer.last_name
FROM payment
INNER JOIN customer ON payment.customer_id = customer.customer_id
WHERE customer.last_name == 'DAVIS'
")
```

    ##   payment_id amount last_name
    ## 1      16685   4.99     DAVIS
    ## 2      16686   2.99     DAVIS
    ## 3      16687   0.99     DAVIS

# Exercise 6

## Excercise 6.1

**Use COUNT() to count the number of rows in rental.**

``` r
dbGetQuery(con,
"SELECT COUNT(*)
FROM rental
LIMIT 10
")
```

    ##   COUNT(*)
    ## 1    16044

## Excercise 6.2

**Use COUNT() and GROUP BY to count the number of rentals for each
customer\_id.**

``` r
dbGetQuery(con,
"SELECT COUNT(*) as '# rentals', customer_id
FROM rental
GROUP BY customer_id
LIMIT 10
")
```

    ##    # rentals customer_id
    ## 1         32           1
    ## 2         27           2
    ## 3         26           3
    ## 4         22           4
    ## 5         38           5
    ## 6         28           6
    ## 7         33           7
    ## 8         24           8
    ## 9         23           9
    ## 10        25          10

## Excercise 6.3

**Repeat the previous query and sort by the count in descending order.**

``` r
dbGetQuery(con,
"SELECT COUNT(*) as '# rentals', customer_id
FROM rental
GROUP BY customer_id
ORDER BY `# rentals` DESC
LIMIT 10
")
```

    ##    # rentals customer_id
    ## 1         46         148
    ## 2         45         526
    ## 3         42         236
    ## 4         42         144
    ## 5         41          75
    ## 6         40         469
    ## 7         40         197
    ## 8         39         468
    ## 9         39         178
    ## 10        39         137

## Excercise 6.4

**Repeat the previous query but use HAVING to only keep the groups with
40 or more.**

``` r
dbGetQuery(con,
"SELECT COUNT(*) as '# rentals', customer_id
FROM rental
GROUP BY customer_id
HAVING `# rentals` >= 40
ORDER BY `# rentals` DESC
LIMIT 10
")
```

    ##   # rentals customer_id
    ## 1        46         148
    ## 2        45         526
    ## 3        42         236
    ## 4        42         144
    ## 5        41          75
    ## 6        40         469
    ## 7        40         197

# Exercise 7

**Calculate a number of summary statistics for the payment table using
MAX, MIN, AVG and SUM**

## Excercise 7.1

**Modify the above query to do those calculations for each
customer\_id.**

``` r
dbGetQuery(con,
"SELECT
  customer_id,
  MAX(amount) as max,
  MIN(amount) as min,
  AVG(amount) as avg,
  SUM(amount) as sum
FROM payment
GROUP BY customer_id
LIMIT 10
")
```

    ##    customer_id  max  min      avg   sum
    ## 1            1 2.99 0.99 1.990000  3.98
    ## 2            2 4.99 4.99 4.990000  4.99
    ## 3            3 2.99 1.99 2.490000  4.98
    ## 4            5 6.99 0.99 3.323333  9.97
    ## 5            6 4.99 0.99 2.990000  8.97
    ## 6            7 5.99 0.99 4.190000 20.95
    ## 7            8 6.99 6.99 6.990000  6.99
    ## 8            9 4.99 0.99 3.656667 10.97
    ## 9           10 4.99 4.99 4.990000  4.99
    ## 10          11 6.99 6.99 6.990000  6.99

## Excercise 7.2

**Modify the above query to only keep the customer\_ids that have more
then 5 payments.**

``` r
dbGetQuery(con,
"SELECT
  customer_id,
  MAX(amount) as max,
  MIN(amount) as min,
  AVG(amount) as avg,
  SUM(amount) as sum
FROM payment
GROUP BY customer_id
HAVING count(customer_id) > 5
LIMIT 10
")
```

    ##    customer_id  max  min      avg   sum
    ## 1           19 9.99 0.99 4.490000 26.94
    ## 2           53 9.99 0.99 4.490000 26.94
    ## 3          109 7.99 0.99 3.990000 27.93
    ## 4          161 5.99 0.99 2.990000 17.94
    ## 5          197 3.99 0.99 2.615000 20.92
    ## 6          207 6.99 0.99 2.990000 17.94
    ## 7          239 7.99 2.99 5.656667 33.94
    ## 8          245 8.99 0.99 4.823333 28.94
    ## 9          251 4.99 1.99 3.323333 19.94
    ## 10         269 6.99 0.99 3.156667 18.94

# Cleanup

**Run the following chunk to disconnect from the connection.**

``` r
# clean up
dbDisconnect(con)
```
