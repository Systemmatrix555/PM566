PM 566 HW 4
================
Chris Hanson
11/19/2021

``` r
library(dplyr)
```

# HPC

## Problem 1: Make sure your code is nice

**Rewrite the following R functions to make them faster.**

*Original:*

This function adds up all values of each row.

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}
```

*Rewrite:*

``` r
fun1alt <- function(mat) {
  ans <- rowSums(mat)
  ans
}
```

*Original:*

This function creates a matrix of the same size as the original, but
each value is the cumulative of each row across the row.

``` r
# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}
```

*Rewrite:*

``` r
fun2alt <- function(mat) {
  ans <- t(apply(mat, 1, cumsum))
  ans
}
```

``` r
# Make a matrix to test and compare the original and rewritten functions
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test the first original function against the rewritten function
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median       uq       max neval
    ##     fun1(dat) 4.217778 5.360262 4.379355 5.448498 5.724501 0.2949912   100
    ##  fun1alt(dat) 1.000000 1.000000 1.000000 1.000000 1.000000 1.0000000   100

``` r
# Test the second original function against the rewritten function
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean  median       uq      max neval
    ##     fun2(dat) 5.814949 4.305575 3.831038 4.29666 4.134261 1.301973   100
    ##  fun2alt(dat) 1.000000 1.000000 1.000000 1.00000 1.000000 1.000000   100

## Problem 2: Make things run faster with parallel computing

**The following function allows simulating PI (3.14)**

(runif generates uniform random numbers between 0 and 1. we generate 2
of these, square them, and add them together. if this is less than 1, we
get a 1. if not, we get a 0. we take the mean of 2000 of these and
multiply by 4, and this approximates pi, much to my disbelief)

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

**In order to get accurate estimates, we can run this function multiple
times, with the following code:**

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##    1.42    0.00    1.43

**Rewrite the previous code using parLapply() to make it run faster.
Make sure you set the seed using clusterSetRNGStream():**

``` r
library(parallel)
system.time({
  
  cl <- makePSOCKcluster(5L)
  
  clusterSetRNGStream(cl, 1231)
  
  clusterExport(cl, c("sim_pi"), envir = environment())
  
  ans <- unlist(parLapply(cl = cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  
  stopCluster(cl)
  
  ans
})
```

    ## [1] 3.141584

    ##    user  system elapsed 
    ##    0.00    0.02    0.70

# SQL

**Set up a temporary database by running the following chunk:**

``` r
library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

## Question 1: How many movies are there available in each rating category?

``` sql
SELECT rating as 'Rating', COUNT(*) AS '# of films'
FROM film
GROUP BY rating
```

| Rating | \# of films |
|:-------|------------:|
| G      |         180 |
| NC-17  |         210 |
| PG     |         194 |
| PG-13  |         223 |
| R      |         195 |

5 records

## Question 2: What is the average replacement cost and rental rate for each rating category?

``` sql
SELECT f.rating AS 'Rating', AVG(f.replacement_cost) AS 'Avg Replacement Cost', AVG(f.rental_rate) AS 'Avg Rental Rate'
FROM film AS f
GROUP BY f.rating
```

| Rating | Avg Replacement Cost | Avg Rental Rate |
|:-------|---------------------:|----------------:|
| G      |             20.12333 |        2.912222 |
| NC-17  |             20.13762 |        2.970952 |
| PG     |             18.95907 |        3.051856 |
| PG-13  |             20.40256 |        3.034843 |
| R      |             20.23103 |        2.938718 |

5 records

## Question 3: Find how many films there are within each category ID:

``` sql
SELECT fc.category_id as 'Category ID', COUNT(*) AS 'Count'
FROM film AS f
INNER JOIN film_category AS fc
ON f.film_id = fc.film_id
GROUP BY fc.category_id
```

| Category ID | Count |
|:------------|------:|
| 1           |    64 |
| 2           |    66 |
| 3           |    60 |
| 4           |    57 |
| 5           |    58 |
| 6           |    68 |
| 7           |    62 |
| 8           |    69 |
| 9           |    73 |
| 10          |    61 |

Displaying records 1 - 10

## Question 4: Incorporate the ‘category’ table into the previous answer to find the name of the most popular cagetory:

``` sql
SELECT COUNT(*) as 'Count', a.category_id as 'Category ID', a.name AS 'Category Name'
FROM 
(SELECT *
FROM film_category AS fc
LEFT JOIN category as c
ON fc.category_id = c.category_id) AS a
LEFT JOIN film as f
ON a.film_id = f.film_id
GROUP BY `Category Name`
ORDER BY `Count` DESC
```

| Count | Category ID | Category Name |
|------:|------------:|:--------------|
|    74 |          15 | Sports        |
|    73 |           9 | Foreign       |
|    69 |           8 | Family        |
|    68 |           6 | Documentary   |
|    66 |           2 | Animation     |
|    64 |           1 | Action        |
|    63 |          13 | New           |
|    62 |           7 | Drama         |
|    61 |          14 | Sci-Fi        |
|    61 |          10 | Games         |

Displaying records 1 - 10

The most popular category is Sports.

### Cleanup

**Run the following chunk to disconnect from the connection.**

``` r
# clean up
dbDisconnect(con)
```
