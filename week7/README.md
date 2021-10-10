PM566 Lab 7
================
Chris Hanson
10/8/2021

Lab description: Using the NCBI (National Center for Biotechnology
Information) API to make queries and extract information using XML and
regular expressions.

## Question 1: How many results are provided on PubMed when searching sars-cov-2?

``` r
# Downloading the website using XML
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2")

# Finding the counts
#The XPath was found by right clicking the area of the website of which we wanted to see the html code and clicking "inspect," then right clicking on that code and copying full Xpath. We then search the html code for this Xpath.
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/span")

# "counts" is currently an {html_node}. Turning it into text:
counts <- as.character(counts)
#"counts" is currently: "<span class=\"value\">114,846</span>"

# Extracting the data using regex.
#stringr is a tidyverse regex package.
#[0-9] is any number in the range 0-9, + means one or more matches, and we also include a comma.
stringr::str_extract(counts, "[0-9,]+") #This is our final answer
```

    ## [1] "114,846"

## Question 2: Academic publications on COVID19 and Hawaii

``` r
library(httr)
#communicating with APIs
#httr is an r package for working with html. GET is a common httr function, it GETS a url.

query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/",
  path  = "entrez/eutils/esearch.fcgi",
  query = list(db = "pubmed", 
               term = "covid19 hawaii", 
               retmax = 1000
               )
)

# Extracting the content of the response of GET
ids <- httr::content(query_ids)
#This returns an XML object. 
```

## Question 3: Get details about the articles

The IDs are wrapped around text in the following way: <Id>… id number …
</Id>. The ID number can be extracted using a regex:

``` r
ids_list <- xml2::as_list(ids)
  
  # Turn the result of Q2 into a character vector
ids <- as.character(ids)

# Find all the ids using a stringr regex.
# We are looking for multiple digits within <Id> brackets.
ids <- stringr::str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]

# Remove all the leading and trailing <Id> </Id>.
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
```

We now have a list of all of the IDs. Next we need to get the abstracts
of all of the papers associated with these ID numbers. We will do so
using httr::GET

``` r
#Now instead of term: covid19 hawaii, we query with the ID numbers of the papers we just gathered, and the rettype "abstract."

#"I()" is inhibit interpretation/conversion of objects. Without doing so, R would convert the comma in our ID numbers into a "%2C". We also collapse the , which apparently removes it.
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",
    id = I(paste(ids, collapse=",")),
    retmax = 1000,
    rettype = "abstract"
    )
)

# Extracting the content of the results of GET into an XML object:
publications <- httr::content(publications)

#Turning this XML object into characters:
publications_txt <- as.character(publications)
```

Question 4: Distribution of universities, schools, and departments

Use stringr on the publications\_txt created above to capture all of the
terms that look like “University of …” or “… Institute of …”.

``` r
library(stringr)

institution <- str_extract_all(
  str_to_lower(publications_txt),
  "university\\s+of\\s+(southern|new|the)?\\s*[[:alpha:]-]+|\\s+institute\\s+of\\s+(southern|new|the)?\\s*[[:alpha:]-]+"
  ) 

# "\s" is whitespace character. when you need to use a "\", you have to double it. ? means 0 or 1 match, * is zero or more matches. [[:alpha:]] is upper and lower case letters, - allows it to have a dash inside, + is one or more matches.

institution <- unlist(institution)
table(institution) #table builds a contingency table of the counts
```

    ## institution
    ##              institute of allergy               institute of allied 
    ##                                 1                                 1 
    ##           institute of biomedical          institute of biomedicine 
    ##                                 1                                 1 
    ##         institute of chiropractic            institute of education 
    ##                                 1                                 1 
    ##                institute of emory        institute of environmental 
    ##                                 2                                 3 
    ##         institute of epidemiology              institute of harvard 
    ##                                 1                                 2 
    ##               institute of health                institute of liver 
    ##                                 5                                 2 
    ##               institute of marine             institute of medicine 
    ##                                 1                                 3 
    ##          institute of montpellier           institute of new jersey 
    ##                                 1                                 1 
    ##          institute of new zealand         institute of oceanography 
    ##                                 4                                 1 
    ##         institute of pharmacology           institute of psychiatry 
    ##                                 2                                 1 
    ##               institute of public       institute of rehabilitation 
    ##                                 1                                 3 
    ##            institute of rheumatic            institute of singapore 
    ##                                 2                                 1 
    ##       institute of southern italy           institute of technology 
    ##                                 2                                 4 
    ##             institute of tropical             university of alberta 
    ##                                13                                 2 
    ##             university of applied             university of arizona 
    ##                                 1                                 5 
    ##            university of arkansas               university of basel 
    ##                                 1                                 8 
    ##               university of benin            university of botswana 
    ##                                 1                                 1 
    ##            university of bradford             university of bristol 
    ##                                 1                                 4 
    ##             university of british             university of calgary 
    ##                                 4                                 1 
    ##          university of california             university of chicago 
    ##                                65                                11 
    ##          university of cincinnati            university of colorado 
    ##                                 9                                 5 
    ##         university of connecticut          university of copenhagen 
    ##                                 1                                 1 
    ##             university of córdoba           university of education 
    ##                                 1                                 1 
    ##              university of exeter             university of florida 
    ##                                 1                                 5 
    ##             university of granada               university of haifa 
    ##                                 2                                 1 
    ##               university of hawai              university of hawaii 
    ##                                92                               180 
    ##        university of hawaii-manoa              university of health 
    ##                                 2                                 8 
    ##                university of hong            university of honolulu 
    ##                                 1                                 3 
    ##            university of illinois                university of iowa 
    ##                                 1                                 4 
    ##           university of jerusalem                university of juiz 
    ##                                 1                                 4 
    ##              university of kansas            university of kentucky 
    ##                                 2                                 1 
    ##            university of lausanne               university of leeds 
    ##                                 1                                 2 
    ##          university of louisville              university of malaya 
    ##                                 1                                 2 
    ##            university of maryland            university of medicine 
    ##                                 9                                 3 
    ##           university of melbourne               university of miami 
    ##                                 1                                 2 
    ##            university of michigan           university of minnesota 
    ##                                 8                                 4 
    ##              university of murcia            university of nebraska 
    ##                                 1                                 5 
    ##              university of nevada         university of new england 
    ##                                 1                                 1 
    ##           university of new south            university of new york 
    ##                                 3                                 3 
    ## university of new york-university               university of north 
    ##                                 1                                 2 
    ##             university of ontario                university of oslo 
    ##                                 1                                 6 
    ##              university of ottawa              university of oxford 
    ##                                 1                                 9 
    ##               university of paris        university of pennsylvania 
    ##                                 1                                47 
    ##          university of pittsburgh               university of porto 
    ##                                13                                 2 
    ##              university of puerto                 university of rio 
    ##                                 2                                 1 
    ##           university of rochester                 university of sao 
    ##                                 4                                 2 
    ##             university of science           university of singapore 
    ##                                13                                 1 
    ##               university of south university of southern california 
    ##                                 4                                21 
    ##    university of southern denmark              university of sydney 
    ##                                 1                                 1 
    ##          university of technology               university of texas 
    ##                                 3                                 7 
    ##          university of the health     university of the philippines 
    ##                                16                                 1 
    ##             university of toronto              university of toulon 
    ##                                 5                                 1 
    ##            university of tübingen                university of utah 
    ##                                 3                                 4 
    ##          university of washington           university of wisconsin 
    ##                                 6                                 3

Now I’ll repeat the exercise on “School of” or “Department of”:

``` r
department <- str_extract_all(
  str_to_lower(publications_txt),
  "department\\s+of\\s*[[:alpha:]-]+|school\\s+of\\s*[[:alpha:]-]+"
  ) 

department <- unlist(department)
table(department)
```

    ## department
    ##              department of ageing             department of anatomy 
    ##                                 1                                 2 
    ##          department of anesthesia       department of anesthesilogy 
    ##                                 2                                 1 
    ##      department of anesthesiology             department of applied 
    ##                                 6                                 3 
    ##        department of biochemistry             department of biology 
    ##                                 1                                11 
    ##         department of biosciences       department of biostatistics 
    ##                                 1                                15 
    ##              department of botany          department of cardiology 
    ##                                 1                                 1 
    ##      department of cardiovascular                department of cell 
    ##                                 1                                 4 
    ##           department of chemistry               department of civil 
    ##                                 2                                12 
    ##            department of clinical            department of commerce 
    ##                                10                                 1 
    ##       department of communication       department of communicology 
    ##                                 2                                 2 
    ##           department of community       department of computational 
    ##                                 3                                 1 
    ##            department of critical             department of defense 
    ##                                 4                                 1 
    ##         department of dermatology           department of economics 
    ##                                22                                 3 
    ##           department of education           department of emergency 
    ##                                 7                                 5 
    ##       department of environmental        department of epidemiology 
    ##                                 6                                18 
    ##        department of experimental              department of family 
    ##                                 1                                 6 
    ##             department of general             department of genetic 
    ##                                 3                                 1 
    ##           department of geography              department of health 
    ##                                 5                                50 
    ##          department of hematology          department of immunology 
    ##                                 3                                 1 
    ##          department of infectious         department of information 
    ##                                22                                 2 
    ##           department of intensive            department of internal 
    ##                                 3                                55 
    ##       department of international         department of kinesiology 
    ##                                 1                                 2 
    ##          department of laboratory         department of mathematics 
    ##                                 3                                 7 
    ##          department of mechanical             department of medical 
    ##                                 5                                 7 
    ##            department of medicine        department of microbiology 
    ##                               110                                 3 
    ##              department of native          department of nephrology 
    ##                                 2                                 5 
    ##        department of neurological           department of neurology 
    ##                                12                                 2 
    ##        department of neurosurgery             department of nursing 
    ##                                 1                                 1 
    ##           department of nutrition                  department of ob 
    ##                                 7                                 5 
    ##          department of obstetrics        department of occupational 
    ##                                18                                 2 
    ##          department of orthopedic department of otolaryngology-head 
    ##                                 5                                 4 
    ##          department of paediatric           department of pathology 
    ##                                 1                                11 
    ##           department of pediatric          department of pediatrics 
    ##                                 2                                26 
    ##      department of pharmaceutical        department of pharmacology 
    ##                                 1                                 2 
    ##            department of pharmacy            department of physical 
    ##                                 1                                 5 
    ##          department of physiology       department of physiotherapy 
    ##                                10                                 1 
    ##          department of population          department of preventive 
    ##                                 6                                13 
    ##          department of psychiatry          department of psychology 
    ##                                27                                 7 
    ##              department of public           department of pulmonary 
    ##                                10                                 1 
    ##        department of quantitative      department of rehabilitation 
    ##                                 8                                 6 
    ##            department of research        department of rheumatology 
    ##                                 1                                 7 
    ##             department of smoking              department of social 
    ##                                 8                                 1 
    ##           department of sociology              department of sports 
    ##                                 4                                 1 
    ##          department of statistics             department of surgery 
    ##                                 2                                13 
    ##             department of traffic       department of translational 
    ##                                 1                                 1 
    ##            department of tropical                department of twin 
    ##                                31                                 4 
    ##             department of urology            department of veterans 
    ##                                 1                                 8 
    ##          department of veterinary              school of biomedical 
    ##                                 2                                 3 
    ##                   school of brown               school of education 
    ##                                 2                                 2 
    ##              school of electronic            school of epidemiology 
    ##                                 1                                 6 
    ##                  school of health              school of immunology 
    ##                                 1                                 1 
    ##                    school of life                school of medicine 
    ##                                 1                               343 
    ##                 school of natural                 school of nursing 
    ##                                 1                                23 
    ##                   school of ocean                school of pharmacy 
    ##                                 1                                 1 
    ##                school of physical           school of physiotherapy 
    ##                                 6                                 1 
    ##              school of population                  school of public 
    ##                                 2                                64 
    ##                  school of social          school of transportation 
    ##                                11                                 1

Question 5: Form a database which includes the title and the abstract of
the papers.

``` r
#Using xml_children() first will keep one element per id.
pub_char_list <- xml2::xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)

#Extracting the abstract for each one of the elements.

#print matches all printable characters like alphabets, numbers, and blank spaces. space is tab, newline, and space. 
abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
#We now remove all the html tags 
abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+.")
#Removing all the extra space on the line, replacing them with a single space.
abstracts <- str_replace_all(abstracts, "[[:space:]]+", " ")

#Extracting the title for each one of the elements in the XML object, just like we did for abstracts.
titles <- str_extract(pub_char_list, "<ArticleTitle>[[:print:][:space:]]+</ArticleTitle>")
titles <- str_remove_all(titles, "</?[[:alnum:]- =\"]+>")

database <- data.frame(
  PubMedId = ids,
  title    = titles,
  Abstract = abstracts
)
knitr::kable(database[1:5,], caption = "Covid19 and Hawaii")
```

| PubMedId | title                                                                                                              | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|:---------|:-------------------------------------------------------------------------------------------------------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 34621978 | GenoRisk: A polygenic risk score for Alzheimer’s disease.                                                          | Recent clinical trials are considering inclusion of more than just apolipoprotein E (APOE) e4 genotype as a way of reducing variability in analysis of outcomes. Case-control data were used to compare the capacity of age, sex, and 58 Alzheimer’s disease (AD)-associated single nucleotide polymorphisms (SNPs) to predict AD status using several statistical models. Model performance was assessed with Brier scores and tenfold cross-validation. Genotype and sex × age estimates from the best performing model were combined with age and intercept estimates from the general population to develop a personalized genetic risk score, termed age, and sex-adjusted GenoRisk. The elastic net model that included age, age x sex interaction, allelic APOE terms, and 29 additional SNPs performed the best. This model explained an additional 19% of the heritable risk compared to APOE genotype alone and achieved an area under the curve of 0.747. GenoRisk could improve the risk assessment of individuals identified for prevention studies. © 2021 The Authors. Alzheimer’s & Dementia: Diagnosis, Assessment & Disease Monitoring published by Wiley Periodicals, LLC on behalf of Alzheimer’s Association.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 34562997 | Variables Associated with Coronavirus Disease 2019 Vaccine Hesitancy Amongst Patients with Neurological Disorders. | Given that the success of vaccines against coronavirus disease 2019 (COVID-19) relies on herd immunity, identifying patients at risk for vaccine hesitancy is imperative-particularly for those at high risk for severe COVID-19 (i.e., minorities and patients with neurological disorders). Among patients from a large neuroscience institute in Hawaii, vaccine hesitancy was investigated in relation to over 30 sociodemographic variables and medical comorbidities, via a telephone quality improvement survey conducted between 23 January 2021 and 13 February 2021. Vaccine willingness (n = 363) was 81.3%. Univariate analysis identified that the odds of vaccine acceptance reduced for patients who do not regard COVID-19 as a severe illness, are of younger age, have a lower Charlson Comorbidity Index, use illicit drugs, or carry Medicaid insurance. Multivariable logistic regression identified the best predictors of vaccine hesitancy to be: social media use to obtain COVID-19 information, concerns regarding vaccine safety, self-perception of a preexisting medical condition contraindicated with vaccination, not having received the annual influenza vaccine, having some high school education only, being a current smoker, and not having a prior cerebrovascular accident. Unique amongst males, a conservative political view strongly predicted vaccine hesitancy. Specifically for Asians, a higher body mass index, while for Native Hawaiians and other Pacific Islanders (NHPI), a positive depression screen, both reduced the odds of vaccine acceptance. Upon identifying the variables associated with vaccine hesitancy amongst patients with neurological disorders, our clinic is now able to efficiently provide ancillary COVID-19 education to sub-populations at risk for vaccine hesitancy. While our results may be limited to the sub-population of patients with neurological disorders, the findings nonetheless provide valuable insight to understanding vaccine hesitancy.                                                                                                                                                                                                                                                    |
| 34559481 | Astronomical Use of Nitrous Oxide Associated With Stress From the COVID-19 Pandemic and Lockdown.                  | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| 34545941 | Cancer statistics for the US Hispanic/Latino population, 2021.                                                     | The Hispanic/Latino population is the second largest racial/ethnic group in the continental United States and Hawaii, accounting for 18% (60.6 million) of the total population. An additional 3 million Hispanic Americans live in Puerto Rico. Every 3 years, the American Cancer Society reports on cancer occurrence, risk factors, and screening for Hispanic individuals in the United States using the most recent population-based data. An estimated 176,600 new cancer cases and 46,500 cancer deaths will occur among Hispanic individuals in the continental United States and Hawaii in 2021. Compared to non-Hispanic Whites (NHWs), Hispanic men and women had 25%-30% lower incidence (2014-2018) and mortality (2015-2019) rates for all cancers combined and lower rates for the most common cancers, although this gap is diminishing. For example, the colorectal cancer (CRC) incidence rate ratio for Hispanic compared with NHW individuals narrowed from 0.75 (95% CI, 0.73-0.78) in 1995 to 0.91 (95% CI, 0.89-0.93) in 2018, reflecting delayed declines in CRC rates among Hispanic individuals in part because of slower uptake of screening. In contrast, Hispanic individuals have higher rates of infection-related cancers, including approximately two-fold higher incidence of liver and stomach cancer. Cervical cancer incidence is 32% higher among Hispanic women in the continental US and Hawaii and 78% higher among women in Puerto Rico compared to NHW women, yet is largely preventable through screening. Less access to care may be similarly reflected in the low prevalence of localized-stage breast cancer among Hispanic women, 59% versus 67% among NHW women. Evidence-based strategies for decreasing the cancer burden among the Hispanic population include the use of culturally appropriate lay health advisors and patient navigators and targeted, community-based intervention programs to facilitate access to screening and promote healthy behaviors. In addition, the impact of the COVID-19 pandemic on cancer trends and disparities in the Hispanic population should be closely monitored. © 2021 The Authors. CA: A Cancer Journal for Clinicians published by Wiley Periodicals LLC on behalf of American Cancer Society. |
| 34536350 | Addendum needed on COVID-19 travel study.                                                                          | NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |

Covid19 and Hawaii
