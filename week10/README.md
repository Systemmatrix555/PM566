PM 566 Week 10 Lab
================
Chris Hanson
10/29/2021

# Question 1

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  # return(x)
  x
}
#ans <- fun1(50000, 10)
```

This code is not vectorized, it’s in a for loop. This does not allow us
to use parallel computing.

``` r
fun1alt <- function(n = 100, k = 4, lambda = 4) {
  x <- matrix(rpois(n * k, lambda), nrow = n, ncol = k)
  x
}
```

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun1(n = 1000),
  fun1alt(n = 1000), unit="relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median      uq      max neval
    ##     fun1(n = 1000) 24.42263 24.25228 26.85344 23.87344 27.9207 14.48597   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.0000  1.00000   100

# Question 2

``` r
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}
```

``` r
fun2alt <- function(x) {
  # Position of the max value per row of x
  idx <- max.col(t(x))
  
  x[cbind(idx, 1:ncol(x))]
}
```

``` r
all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr     min       lq     mean   median       uq      max neval
    ##     fun2(x) 743.667 768.0215 874.0261 800.7300 850.5425 2603.459   100
    ##  fun2alt(x)  93.251  99.6880 132.4324 108.1465 118.0215 2145.126   100
