PM 566 HW 3
================
Chris Hanson
11/5/2021

``` r
library(httr)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(forcats)
```

## 1. APIs

**Using the NCBI API, look for papers that show up under the term
“sars-cov-2 trial vaccine.” Look for the data in the PubMed database,
and then retrieve the details of the paper (as shown in lab 7),
including the PubMed IDs.**

``` r
# Accessing the PubMed database through the NCBI API and searching for "sars-cov-2 trial  vaccine".  
query_ids <- GET(
  url = "https://eutils.ncbi.nlm.nih.gov/",
  path = "entrez/eutils/esearch.fcgi",
  query = list(db = "pubmed",
               term = "sars-cov-2 trial vaccine",
               retmax = 10000)
)

ids <- content(query_ids)

ids <- as.character(ids)

ids <- str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]

ids <- str_remove_all(ids, "<Id>|</Id>")
```

I found 1044 papers.

**Using the list of Pubmed IDs retrieved, download each paper’s details
using the query parameter rettype = abstract. Keep just the first 250.**

``` r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/",
  path = "entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",
    id = I(paste(ids[1:250], collapse=",")),
    rettype = "abstract"
    )
)

# Extracting the content of the results of GET into an XML object:
publications <- httr::content(publications)

# Turning this XML object into characters:
publications_txt <- as.character(publications)
```

**Create a dataset containing the following:**

**PubMed ID number**

**Title of the paper**

**Name of the journal where it was published**

**Publication date**

**Abstract of the paper**

``` r
pub_char_list <- xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)

abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+.")
abstracts <- str_replace_all(abstracts, "[[:space:]]+", " ")

titles <- str_extract(pub_char_list, "<ArticleTitle>[[:print:][:space:]]+</ArticleTitle>")
titles <- str_remove_all(titles, "</?[[:alnum:]- =\"]+>")

journals <- str_extract(pub_char_list, "<Title>[[:print:][:space:]]+</Title>")
journals <- str_remove_all(journals, "</?[[:alnum:]- =\"]+>")

#dates <- str_extract(pub_char_list, "<PubDate>[[:print:][:space:]]+</PubDate>")
dates <- str_extract(pub_char_list, "<DateRevised>[[:print:][:space:]]+</DateRevised>")
dates <- str_remove_all(dates, "</?[[:alnum:]- =\"]+>")
dates <- str_replace_all(dates, "[[:space:]]+", " ")
dates <- trimws(dates, which = c("both"))
dates <- format(as.Date(dates, "%Y %m %d"), "%m/%d/%Y")

database <- data.frame(
  PubMedId = ids[1:250],
  Title    = titles,
  Journal  = journals,
  Date     = dates,
  Abstract = abstracts
)

knitr::kable(database[1:5,], caption = "Covid19 Vaccine Trial")
```

| PubMedId | Title                                                                                                                             | Journal                                  | Date       | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
|:---------|:----------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------|:-----------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 34729549 | Adverse events of active and placebo groups in SARS-CoV-2 vaccine randomized trials: A systematic review.                         | The Lancet regional health. Europe       | 11/03/2021 | For safety assessment in clinical trials, adverse events (AEs) are reported for the drug under evaluation and compared with AEs in the placebo group. Little is known about the nature of the AEs associated with clinical trials of SARS-CoV-2 vaccines and the extent to which these can be traced to nocebo effects, where negative treatment-related expectations favor their occurrence. In our systematic review, we compared the rates of solicited AEs in the active and placebo groups of SARS-CoV-2 vaccines approved by the Western pharmaceutical regulatory agencies.We implemented a search strategy to identify trial-III studies of SARS-CoV-2 vaccines through the PubMed database. We adopted the PRISMA Statement to perform the study selection and the data collection and identified three trial: two mRNA-based (37590 participants) and one adenovirus type (6736 participants). Relative risks showed that the occurrence of AEs reported in the vaccine groups was higher compared with the placebo groups. The most frequently AEs in both groups were fatigue, headache, local pain, as injection site reactions, and myalgia. In particular, for first doses in placebo recipients, fatigue was reported in 29% and 27% in BNT162b2 and mRNA-1273 groups, respectively, and in 21% of Ad26.COV2.S participants. Headache was reported in 27% in both mRNA groups and in 24% of Ad26.COV2.S recipients. Myalgia was reported in 10% and 14% in mRNA groups (BNT162b2 and mRNA-1273, respectively) and in 13% of Ad26.COV2.S participants. Local pain was reported in 12% and 17% in mRNA groups (BNT162b2 and mRNA-1273, respectively), and in 17% of Ad26.COV2.S recipients. These AEs are more common in the younger population and in the first dose of placebo recipients of the mRNA vaccines. Our results are in agreement with the expectancy theory of nocebo effects and suggest that the AEs associated with COVID-19 vaccines may be related to the nocebo effect. Fondazione CRT - Cassa di Risparmio di Torino, IT (grant number 66346, “GAIA-MENTE” 2019). © 2021 The Authors.                                                           |
| 34726743 | Analysis of the Effectiveness of the Ad26.COV2.S Adenoviral Vector Vaccine for Preventing COVID-19.                               | JAMA network open                        | 11/03/2021 | Continuous assessment of the effectiveness and safety of the US Food and Drug Administration-authorized SARS-CoV-2 vaccines is critical to amplify transparency, build public trust, and ultimately improve overall health outcomes. To evaluate the effectiveness of the Johnson & Johnson Ad26.COV2.S vaccine for preventing SARS-CoV-2 infection. Setting, and Participants" NlmCategory=“UNASSIGNED”&gt;This comparative effectiveness research study used large-scale longitudinal curation of electronic health records from the multistate Mayo Clinic Health System (Minnesota, Arizona, Florida, Wisconsin, and Iowa) to identify vaccinated and unvaccinated adults between February 27 and July 22, 2021. The unvaccinated cohort was matched on a propensity score derived from age, sex, zip code, race, ethnicity, and previous number of SARS-CoV-2 polymerase chain reaction tests. The final study cohort consisted of 8889 patients in the vaccinated group and 88 898 unvaccinated matched patients. Single dose of the Ad26.COV2.S vaccine. The incidence rate ratio of SARS-CoV-2 infection in the vaccinated vs unvaccinated control cohorts, measured by SARS-CoV-2 polymerase chain reaction testing. The study was composed of 8889 vaccinated patients (4491 men \[50.5%\]; mean \[SD\] age, 52.4 \[16.9\] years) and 88 898 unvaccinated patients (44 748 men \[50.3%\]; mean \[SD\] age, 51.7 \[16.7\] years). The incidence rate ratio of SARS-CoV-2 infection in the vaccinated vs unvaccinated control cohorts was 0.26 (95% CI, 0.20-0.34) (60 of 8889 vaccinated patients vs 2236 of 88 898 unvaccinated individuals), which corresponds to an effectiveness of 73.6% (95% CI, 65.9%-79.9%) and a 3.73-fold reduction in SARS-CoV-2 infections. This study’s findings are consistent with the clinical trial-reported efficacy of Ad26.COV2.S and the first retrospective analysis, suggesting that the vaccine is effective at reducing SARS-CoV-2 infection, even with the spread of variants such as Alpha or Delta that were not present in the original studies, and reaffirm the urgent need to continue mass vaccination efforts globally. |
| 34715931 | Lessons from Israel’s COVID-19 Green Pass program.                                                                                | Israel journal of health policy research | 11/03/2021 | As of the beginning of March 2021, Israeli law requires the presentation of a Green Pass as a precondition for entering certain businesses and public spheres. Entitlement for a Green Pass is granted to Israelis who have been vaccinated with two doses of COVID-19 vaccine, who have recovered from COVID-19, or who are participating in a clinical trial for vaccine development in Israel. The Green Pass is essential for retaining immune individuals’ freedom of movement and for promoting the public interest in reopening the economic, educational, and cultural spheres of activity. Nonetheless, and as the Green Pass imposes restrictions on the movement of individuals who had not been vaccinated or who had not recovered, it is not consonant with solidarity and trust building. Implementing the Green Pass provision while advancing its effectiveness on the one hand, and safeguarding equality, proportionality, and fairness on the other hand may imbue this measure with ethical legitimacy despite involving a potential breach of trust and solidarity. © 2021. The Author(s).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 34713912 | Vaccine development and technology for SARS-CoV-2: current insights.                                                              | Journal of medical virology              | 10/29/2021 | SARS-CoV-2 is associated to a severe respiratory disease in China, that rapidly spread across continents. Since the beginning of the pandemic, available data suggested the asymptomatic transmission and patients were treated with specific drugs with efficacy and safety data not always satisfactory. The aim of this review is to describe the vaccines developed by three companies, Pfizer-BioNTech, Moderna and University of Oxford/AstraZeneca, in terms of both technological and pharmaceutical formulation, safety, efficacy and immunogenicity. A critical analysis of phase 1, 2 and 3 clinical trial results available was conducted, comparing the three vaccine candidates, underlining their similarities and differences. All candidates showed consistent efficacy and tolerability; although some differences can be noted, such as their technological formulation, temperature storage, which will be related to logistics and costs. Further studies will be necessary to evaluate long-term effects and to assess the vaccine safety and efficacy in the general population. This article is protected by copyright. All rights reserved. This article is protected by copyright. All rights reserved.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 34711598 | BCG vaccination to reduce the impact of COVID-19 in healthcare workers: Protocol for a randomised controlled trial (BRACE trial). | BMJ open                                 | 11/03/2021 | BCG vaccination modulates immune responses to unrelated pathogens. This off-target effect could reduce the impact of emerging pathogens. As a readily available, inexpensive intervention that has a well-established safety profile, BCG is a good candidate for protecting healthcare workers (HCWs) and other vulnerable groups against COVID-19. This international multicentre phase III randomised controlled trial aims to determine if BCG vaccination reduces the incidence of symptomatic and severe COVID-19 at 6 months (co-primary outcomes) compared with no BCG vaccination. We plan to randomise 10 078 HCWs from Australia, The Netherlands, Spain, the UK and Brazil in a 1:1 ratio to BCG vaccination or no BCG (control group). The participants will be followed for 1 year with questionnaires and collection of blood samples. For any episode of illness, clinical details will be collected daily, and the participant will be tested for SARS-CoV-2 infection. The secondary objectives are to determine if BCG vaccination reduces the rate, incidence, and severity of any febrile or respiratory illness (including SARS-CoV-2), as well as work absenteeism. The safety of BCG vaccination in HCWs will also be evaluated. Immunological analyses will assess changes in the immune system following vaccination, and identify factors associated with susceptibility to or protection against SARS-CoV-2 and other infections. Ethical and governance approval will be obtained from participating sites. Results will be published in peer-reviewed open-access journals. The final cleaned and locked database will be deposited in a data sharing repository archiving system. ClinicalTrials.gov NCT04327206. © Author(s) (or their employer(s)) 2021. Re-use permitted under CC BY. Published by BMJ.                                                                                                                                                                                                                                                                                                                                          |

Covid19 Vaccine Trial

## 2. Text Mining

**A dataset from the course data repository contains 3241 abstracts from
articles across 5 search terms. Analyse these abstracts to find
interesting insights.**

**Tokenize the abstracts and count the number of each token.**

``` r
# Importing the dataset 
mining_abstracts <- read.csv("pubmed.csv")
mining_abstracts <- as_tibble(mining_abstracts)

# Discovering the 5 topics of the abstracts provided
terms <- mining_abstracts %>%
  count(term) %>%
  arrange(desc(n))

# Tokenize the abstracts and count the number of each token--------------------

# All abstracts, stop words not removed
mining_abstracts %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  knitr::kable()
```

| word  |     n |
|:------|------:|
| the   | 28126 |
| of    | 24759 |
| and   | 19993 |
| in    | 14653 |
| to    | 10920 |
| a     |  8246 |
| with  |  8038 |
| covid |  7267 |
| 19    |  7080 |
| is    |  5649 |

``` r
# All abstracts, stop words and numbers removed
tokens <- mining_abstracts %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word, sort = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl(pattern = "^[0-9]+$", x = word))
  
tokens %>%  
  top_n(10) %>%
  knitr::kable()
```

| word         |    n |
|:-------------|-----:|
| covid        | 7267 |
| patients     | 4673 |
| cancer       | 3999 |
| prostate     | 3832 |
| disease      | 2574 |
| pre          | 2158 |
| eclampsia    | 2005 |
| preeclampsia | 1863 |
| treatment    | 1841 |
| clinical     | 1682 |

``` r
# Tokenizing per search term----------------------------------------------------

tokens2 <- mining_abstracts %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word, term, sort = TRUE) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!grepl(pattern = "^[0-9]+$", x = word))

# Covid abstracts
covid <- tokens2 %>%
  filter(term == "covid") %>%
  top_n(5)
covid %>%
  knitr::kable()
```

| word        | term  |    n |
|:------------|:------|-----:|
| covid       | covid | 7267 |
| patients    | covid | 2292 |
| disease     | covid |  943 |
| pandemic    | covid |  800 |
| coronavirus | covid |  647 |
| health      | covid |  647 |

``` r
# Cystic fibrosis abstracts
cystic <- tokens2 %>%
  filter(term == "cystic fibrosis") %>%
  top_n(5) 
cystic %>%
  knitr::kable()
```

| word     | term            |   n |
|:---------|:----------------|----:|
| fibrosis | cystic fibrosis | 867 |
| cystic   | cystic fibrosis | 862 |
| cf       | cystic fibrosis | 625 |
| patients | cystic fibrosis | 586 |
| disease  | cystic fibrosis | 400 |

``` r
# Meningitis abstracts
menin <- tokens2 %>%
  filter(term == "meningitis") %>%
  top_n(5) 
menin %>%
  knitr::kable()
```

| word       | term       |   n |
|:-----------|:-----------|----:|
| patients   | meningitis | 446 |
| meningitis | meningitis | 429 |
| meningeal  | meningitis | 219 |
| csf        | meningitis | 206 |
| clinical   | meningitis | 187 |

``` r
# Preeclampsia abstracts
preec <- tokens2 %>%
  filter(term == "preeclampsia") %>%
  top_n(5) 
preec %>%
  knitr::kable()
```

| word         | term         |    n |
|:-------------|:-------------|-----:|
| pre          | preeclampsia | 2031 |
| eclampsia    | preeclampsia | 2005 |
| preeclampsia | preeclampsia | 1863 |
| women        | preeclampsia | 1196 |
| pregnancy    | preeclampsia |  969 |

``` r
# Prostate cancer abstracts
prostate <- tokens2 %>%
  filter(term == "prostate cancer") %>%
  top_n(5) 

prostate %>%
  knitr::kable()
```

| word      | term            |    n |
|:----------|:----------------|-----:|
| cancer    | prostate cancer | 3840 |
| prostate  | prostate cancer | 3832 |
| patients  | prostate cancer |  934 |
| treatment | prostate cancer |  926 |
| disease   | prostate cancer |  652 |

The most frequently used words across all these abstracts are not very
interesting unless stop words are removed. When they are removed, it
becomes clear that this is a list of abstracts of medical papers, from a
variety of subjects: COVID, cancer, preeclampsia. When filtering these
abstracts by search term before tokenizing, the most frequent words are
to be expected: “cystic,” “fibrosis,” “disease,” for cystic fibrosis
papers, etc.

**Tokenize the abstracts into bigrams. Find the 10 most common bigrams
and visualize them with ggplot2.**

``` r
mining_abstracts %>%
  unnest_ngrams(output = bigram, input = abstract, n = 2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(10) %>%
  ggplot(aes(x = n, y = fct_reorder(bigram, n))) +
  geom_col() +
  labs(Title = "Bigram frequency from all abstracts", y = "Bigram", x = "Count")
```

![](README_files/figure-gfm/bigrams-1.png)<!-- -->

**Calculate the TF-IDF value for each word-search term combination (the
search term is the “document”). What are the 5 tokens from each search
term with the highest TF-IDF value? How are the results different from
the answers from question 1?**

(TF-IDF is term frequency x inverse document frequency, it’s a measure
of how unique a term is to a single document)

``` r
tfidf <- mining_abstracts %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word, term) %>%
  bind_tf_idf(word, term, n) %>%
  arrange(desc(tf_idf))

covid2 <- tfidf %>%
  filter(term == "covid") %>%
  top_n(5) 
covid2 %>%
  knitr::kable()
```

| word        | term  |    n |        tf |      idf |   tf\_idf |
|:------------|:------|-----:|----------:|---------:|----------:|
| covid       | covid | 7267 | 0.0370348 | 1.609438 | 0.0596052 |
| pandemic    | covid |  800 | 0.0040770 | 1.609438 | 0.0065617 |
| coronavirus | covid |  647 | 0.0032973 | 1.609438 | 0.0053068 |
| sars        | covid |  371 | 0.0018907 | 1.609438 | 0.0030430 |
| cov         | covid |  333 | 0.0016971 | 1.609438 | 0.0027313 |

``` r
cystic2 <- tfidf %>%
  filter(term == "cystic fibrosis") %>%
  top_n(5) 
cystic2 %>%
  knitr::kable()
```

| word     | term            |   n |        tf |       idf |   tf\_idf |
|:---------|:----------------|----:|----------:|----------:|----------:|
| cf       | cystic fibrosis | 625 | 0.0127164 | 0.9162907 | 0.0116520 |
| fibrosis | cystic fibrosis | 867 | 0.0176402 | 0.5108256 | 0.0090111 |
| cystic   | cystic fibrosis | 862 | 0.0175385 | 0.5108256 | 0.0089591 |
| cftr     | cystic fibrosis |  86 | 0.0017498 | 1.6094379 | 0.0028162 |
| sweat    | cystic fibrosis |  83 | 0.0016887 | 1.6094379 | 0.0027179 |

``` r
menin2 <- tfidf %>%
  filter(term == "meningitis") %>%
  top_n(5) 
menin2 %>%
  knitr::kable()
```

| word            | term       |   n |        tf |       idf |   tf\_idf |
|:----------------|:-----------|----:|----------:|----------:|----------:|
| meningitis      | meningitis | 429 | 0.0091891 | 1.6094379 | 0.0147892 |
| meningeal       | meningitis | 219 | 0.0046909 | 1.6094379 | 0.0075497 |
| pachymeningitis | meningitis | 149 | 0.0031915 | 1.6094379 | 0.0051366 |
| csf             | meningitis | 206 | 0.0044125 | 0.9162907 | 0.0040431 |
| meninges        | meningitis | 106 | 0.0022705 | 1.6094379 | 0.0036542 |

``` r
preec2 <- tfidf %>%
  filter(term == "preeclampsia") %>%
  top_n(5) 
preec2 %>%
  knitr::kable()
```

| word         | term         |    n |        tf |       idf |   tf\_idf |
|:-------------|:-------------|-----:|----------:|----------:|----------:|
| eclampsia    | preeclampsia | 2005 | 0.0142711 | 1.6094379 | 0.0229684 |
| preeclampsia | preeclampsia | 1863 | 0.0132604 | 1.6094379 | 0.0213417 |
| pregnancy    | preeclampsia |  969 | 0.0068971 | 0.5108256 | 0.0035232 |
| maternal     | preeclampsia |  797 | 0.0056728 | 0.5108256 | 0.0028978 |
| gestational  | preeclampsia |  191 | 0.0013595 | 1.6094379 | 0.0021880 |

``` r
prostate2 <- tfidf %>%
  filter(term == "prostate cancer") %>%
  top_n(5) 
prostate2 %>%
  knitr::kable()
```

| word          | term            |    n |        tf |      idf |   tf\_idf |
|:--------------|:----------------|-----:|----------:|---------:|----------:|
| prostate      | prostate cancer | 3832 | 0.0311808 | 1.609438 | 0.0501836 |
| androgen      | prostate cancer |  305 | 0.0024818 | 1.609438 | 0.0039943 |
| psa           | prostate cancer |  282 | 0.0022946 | 1.609438 | 0.0036931 |
| prostatectomy | prostate cancer |  215 | 0.0017494 | 1.609438 | 0.0028156 |
| castration    | prostate cancer |  148 | 0.0012043 | 1.609438 | 0.0019382 |

For the COVID abstracts, the top used words and the top TF-IDF shared 3
of 5 words.

For the cystic fibrosis abstracts, the top used words and the top TF-IDF
shared 3 of 5 words.

For the meningitis abstracts, the top used words and the top TF-IDF
shared 3 of 5 words.

For the preeclampsia abstracts, the top used words and the top TF-IDF
shared 3 of 5 words.

For the prostate cancer abstracts, the top used words and the top TF-IDF
shared 1 of 5 words.

It’s to be expected that there is not full intersection between the top
words and the top TF-IDF words. Each subject is likely to use common
words such as disease and patient, giving those terms a high TF but a
low IDF.
