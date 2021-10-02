pm566week6lab
================
Chris Hanson
10/1/2021

``` r
library(tidyverse)
library(tidytext)
```

``` r
fn  <- "mtsamples.csv"

if (!file.exists(fn))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/00_mtsamples/mtsamples.csv", destfile = fn)

mtsamples <-  read.csv(fn)
mtsamples <- as_tibble(mtsamples)
```

## Question 1. How are specialities distributed?

``` r
specialties <- mtsamples %>%
  count(medical_specialty)

specialties %>%
  arrange(desc(n)) %>%
  top_n(n, 15) %>%
  knitr::kable()
```

| medical\_specialty            |    n |
|:------------------------------|-----:|
| Surgery                       | 1103 |
| Consult - History and Phy.    |  516 |
| Cardiovascular / Pulmonary    |  372 |
| Orthopedic                    |  355 |
| Radiology                     |  273 |
| General Medicine              |  259 |
| Gastroenterology              |  230 |
| Neurology                     |  223 |
| SOAP / Chart / Progress Notes |  166 |
| Obstetrics / Gynecology       |  160 |
| Urology                       |  158 |
| Discharge Summary             |  108 |
| ENT - Otolaryngology          |   98 |
| Neurosurgery                  |   94 |
| Hematology - Oncology         |   90 |
| Ophthalmology                 |   83 |
| Nephrology                    |   81 |
| Emergency Room Reports        |   75 |
| Pediatrics - Neonatal         |   70 |
| Pain Management               |   62 |
| Psychiatry / Psychology       |   53 |
| Office Notes                  |   51 |
| Podiatry                      |   47 |
| Dermatology                   |   29 |
| Cosmetic / Plastic Surgery    |   27 |
| Dentistry                     |   27 |
| Letters                       |   23 |
| Physical Medicine - Rehab     |   21 |
| Sleep Medicine                |   20 |
| Endocrinology                 |   19 |
| Bariatrics                    |   18 |
| IME-QME-Work Comp etc.        |   16 |
| Chiropractic                  |   14 |
| Diets and Nutritions          |   10 |
| Rheumatology                  |   10 |
| Speech - Language             |    9 |
| Autopsy                       |    8 |
| Lab Medicine - Pathology      |    8 |
| Allergy / Immunology          |    7 |
| Hospice - Palliative Care     |    6 |

There are 40 specialties. Let’s take a look at the distribution:

``` r
ggplot(mtsamples, aes(x = medical_specialty)) +
  geom_histogram(stat = "count") +
  coord_flip()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot(specialties, aes(x = n, y = fct_reorder(medical_specialty, n))) +
  geom_col()
```

![](README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

These are not evenly (uniformly) distributed.

## Question 2.

``` r
mtsamples %>%
  unnest_tokens(output = word, input = transcription) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/token-transcript-1.png)<!-- -->

Let’s remove all these stop words.

## Question 3.

``` r
mtsamples %>%
  unnest_tokens(output = word, input = transcription) %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl(pattern = "^[0-9]+$", x = word)) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(word, n))) +
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/token-transcript-wo-stop-1.png)<!-- -->

## Question 4. Tokenize into bi-grams and tri-grams.

``` r
mtsamples %>%
  unnest_ngrams(output = bigram, input = transcription, n = 2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(bigram, n))) +
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Tri-grams:

``` r
mtsamples %>%
  unnest_ngrams(output = trigram, input = transcription, n = 3) %>%
  count(trigram, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(x = n, y = fct_reorder(trigram, n))) +
    geom_col()
```

    ## Selecting by n

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Some fun phrases have appeared.

## Question 5.

``` r
# Don't pick "history"

bigrams <- mtsamples %>%
  unnest_ngrams(output = bigram, input = transcription, n = 2) %>%
  separate(bigram, into = c("w1", "w2"), sep = " ") %>%
  filter((w1 == "operation") | (w2 == "operation"))
  
bigrams %>%  
  filter(w1 == "operation") %>%
  select(w1, w2) %>%
  count(w2, sort = TRUE)
```

    ## # A tibble: 94 x 2
    ##    w2            n
    ##    <chr>     <int>
    ##  1 the         174
    ##  2 performed   159
    ##  3 1            46
    ##  4 in           25
    ##  5 right        22
    ##  6 was          22
    ##  7 after        20
    ##  8 bilateral    18
    ##  9 this         18
    ## 10 and          16
    ## # ... with 84 more rows

``` r
bigrams %>%  
  filter(w2 == "operation") %>%
  select(w1, w2) %>%
  count(w1, sort = TRUE)
```

    ## # A tibble: 129 x 2
    ##    w1           n
    ##    <chr>    <int>
    ##  1 of         226
    ##  2 the        129
    ##  3 for         73
    ##  4 this        13
    ##  5 re          12
    ##  6 norwood      9
    ##  7 same         9
    ##  8 cancer       7
    ##  9 mass         7
    ## 10 aneurysm     6
    ## # ... with 119 more rows

Let’s filter out stop words and numbers.

## Question 6.

``` r
bigrams %>%
  filter(w1 == "operation") %>%
  filter(!(w2 %in% stop_words$word) & !grepl("^[0-9]+$", w2)) %>%
  count(w2, sort = TRUE)
```

    ## # A tibble: 63 x 2
    ##    w2                      n
    ##    <chr>               <int>
    ##  1 performed             159
    ##  2 bilateral              18
    ##  3 left                   15
    ##  4 excision               13
    ##  5 procedure               9
    ##  6 cystoscopy              8
    ##  7 expected                8
    ##  8 loss                    8
    ##  9 phacoemulsification     8
    ## 10 endoscopic              6
    ## # ... with 53 more rows
