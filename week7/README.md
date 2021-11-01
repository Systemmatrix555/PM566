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

    ## [1] "119,937"

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
    ##        institute of biostructures         institute of chiropractic 
    ##                                 1                                 1 
    ##            institute of education                institute of emory 
    ##                                 1                                 2 
    ##        institute of environmental         institute of epidemiology 
    ##                                 3                                 1 
    ##               institute of global              institute of harvard 
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
    ##             institute of tropical         institute of tuberculosis 
    ##                                13                                 2 
    ##             university of alberta             university of applied 
    ##                                 2                                 1 
    ##             university of arizona            university of arkansas 
    ##                                 5                                 1 
    ##               university of basel               university of benin 
    ##                                 8                                 1 
    ##            university of botswana            university of bradford 
    ##                                 1                                 1 
    ##             university of bristol             university of british 
    ##                                 4                                 4 
    ##             university of calgary          university of california 
    ##                                 1                                65 
    ##             university of chicago          university of cincinnati 
    ##                                11                                 9 
    ##            university of colorado         university of connecticut 
    ##                                 5                                 1 
    ##          university of copenhagen             university of córdoba 
    ##                                 1                                 1 
    ##           university of education              university of exeter 
    ##                                 1                                 1 
    ##             university of florida             university of granada 
    ##                                 5                                 2 
    ##               university of haifa               university of hawai 
    ##                                 1                               169 
    ##              university of hawaii        university of hawaii-manoa 
    ##                               180                                 2 
    ##              university of health                university of hong 
    ##                                 8                                 1 
    ##            university of honolulu            university of illinois 
    ##                                 3                                 1 
    ##                university of iowa           university of jerusalem 
    ##                                 4                                 1 
    ##                university of juiz              university of kansas 
    ##                                 4                                 2 
    ##            university of kentucky            university of lausanne 
    ##                                 1                                 1 
    ##               university of leeds          university of louisville 
    ##                                 2                                 1 
    ##              university of malaya            university of maryland 
    ##                                 2                                 9 
    ##             university of medical            university of medicine 
    ##                                 2                                 3 
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
    ##             university of palermo               university of paris 
    ##                                 1                                 1 
    ##        university of pennsylvania          university of pittsburgh 
    ##                                47                                13 
    ##               university of porto              university of puerto 
    ##                                 2                                 2 
    ##                 university of rio           university of rochester 
    ##                                 1                                 4 
    ##                 university of sao             university of science 
    ##                                 2                                13 
    ##           university of singapore               university of south 
    ##                                 1                                 4 
    ## university of southern california    university of southern denmark 
    ##                                21                                 1 
    ##              university of sydney          university of technology 
    ##                                 1                                 3 
    ##               university of texas          university of the health 
    ##                                 7                                16 
    ##     university of the philippines             university of toronto 
    ##                                 1                                 5 
    ##              university of toulon            university of tübingen 
    ##                                 1                                 3 
    ##                university of utah          university of washington 
    ##                                 4                                 6 
    ##           university of wisconsin                university of york 
    ##                                 3                                 1

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
    ##                                 1                                 9 
    ##             department of general             department of genetic 
    ##                                 3                                 1 
    ##           department of geography              department of health 
    ##                                 5                                69 
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
    ##                               112                                 3 
    ##              department of native          department of nephrology 
    ##                                 3                                 5 
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
    ##                                 2                                 5 
    ##          department of physiology       department of physiotherapy 
    ##                                10                                 1 
    ##          department of population          department of preventive 
    ##                                 6                                13 
    ##          department of psychiatry          department of psychology 
    ##                                30                                 7 
    ##              department of public           department of pulmonary 
    ##                                10                                 1 
    ##        department of quantitative        department of radiotherapy 
    ##                                 8                                 1 
    ##      department of rehabilitation            department of research 
    ##                                 6                                 1 
    ##        department of rheumatology             department of smoking 
    ##                                 7                                 8 
    ##              department of social           department of sociology 
    ##                                 4                                 6 
    ##              department of sports          department of statistics 
    ##                                 1                                 2 
    ##             department of surgery             department of traffic 
    ##                                13                                 1 
    ##       department of translational            department of tropical 
    ##                                 1                                31 
    ##                department of twin             department of urology 
    ##                                 4                                 1 
    ##            department of veterans          department of veterinary 
    ##                                 8                                 2 
    ##              school of biomedical                   school of brown 
    ##                                 3                                 2 
    ##               school of education              school of electronic 
    ##                                 2                                 1 
    ##            school of epidemiology                  school of health 
    ##                                 6                                 1 
    ##              school of immunology                     school of law 
    ##                                 1                                 1 
    ##                    school of life                school of medicine 
    ##                                 1                               369 
    ##                 school of natural                 school of nursing 
    ##                                 1                                43 
    ##                   school of ocean                school of pharmacy 
    ##                                 1                                 1 
    ##                school of physical           school of physiotherapy 
    ##                                 6                                 1 
    ##              school of population                  school of public 
    ##                                 2                                64 
    ##                  school of social          school of transportation 
    ##                                31                                 1

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

| PubMedId | title                                                                                                                                        | Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|:---------|:---------------------------------------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 34704071 | Sugar-Sweetened Beverage Fee: A Model to Address Health Disparities in Hawai’i.                                                              | Sugar-sweetened beverage (SSB) consumption is associated with increased risk of obesity, diabetes, and other chronic diseases. SSB consumption is also a health equity issue, as rates of consumption and related chronic diseases vary by race, ethnicity, and income in Hawai’i. The COVID-19 pandemic has highlighted the need for greater investment in public health and the well-being of communities experiencing health disparities because individuals with chronic diseases are more likely to develop complications from the virus. It has also created economic hardships for the people of Hawai’i, especially the state’s most vulnerable populations. Amid this health and economic crisis, an opportunity exists to implement an SSB fee in Hawai’i. An SSB fee would impose a fee on SSB distributors that would be passed on to consumers in the form of price increases that influence purchasing behavior. Jurisdictions with SSB taxes or fees have seen reductions in SSB purchases and consumption and have generated millions of dollars in revenues to support health initiatives and reduce socioeconomic disparities. Models predict that a $0.02 SSB fee in Hawai’i could generate $60.5 million and significantly reduce healthcare costs and chronic diseases. This commentary will present an SSB fee policy as a viable model for Hawai’i to reduce SSB consumption, lower chronic disease risks, and generate needed revenues to support health, reduce inequities, and rebuild the state’s economy. ©Copyright 2021 by University Health Partners of Hawai‘i (UHP Hawai‘i).                                                                                                                                                                                                                                                                                                                                                                    |
| 34704070 | Health Literate Hawai’i: A Blueprint to Empower Health and Wellbeing.                                                                        | Recent studies have identified high rates of chronic disease in Hawai’i’s adults and youth. As the state responds to the COVID-19 pandemic and looks beyond it, the prevention and management of chronic diseases are critical for community health and wellbeing. Low health literacy is more common in rural populations, Filipinos, and Pacific Islanders in Hawai’I, older adults, and many other groups with high rates of chronic disease. Promoting health literacy can reduce chronic disease burdens for individuals, families, and communities. Using the framework of the social-ecological model, which is important for visioning effective chronic disease management and prevention, this article provides a blueprint of layers of influence for building a health literate Hawai’I generally and around chronic disease specifically. The article will close with a call to action informed by the National Action Plan to Improve Health Literacy for stakeholders and providers to address health literacy in the state of Hawai’I in organizations, systems, and policy. These actions should address root causes of disease and help build more equitable health outcomes across the state now and in the future. ©Copyright 2021 by University Health Partners of Hawai‘i (UHP Hawai‘i).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| 34704069 | He ’A’ali’i Ku Makani Mai Au: Developing a Cultural Framework for Advancing COVID-19 Related, Community-informed Health Policies.            | The Native Hawaiian and Pacific Islander community found itself on the front pages of national news when the COVID-19 pandemic struck the United States. By April 2020, the small, frequently overlooked community experienced the highest COVID-19 case rates in 5 states including Hawai’i. In response, Native Hawaiian and Pacific Islander networks across the US were mobilized to address the crisis. In Hawai’i, the Native Hawaiian Pacific Islander COVID-19 Response, Recovery, and Resilience Team was created. Framed by Indigenous Pacific based cultural values, protocols, and practices, the team consists of multiple committees that examine policy; testing, contract tracing, and isolation; communications; social supports and resources; and data and research. Inherent in this work are the shared core values of pono (righteousness, goodness), aloha (love, compassion), laulima (cooperation), and imua (moving forward with strength) as well as an ’ohana/aiga (family)-based, kuleana (responsibility)-centric approach that acknowledges, honors, and values ’ike kupuna (ancestral knowledge). With the burden of not only COVID-19 disparities, but also chronic diseases and socioeconomic disparities that place Native Hawaiian and Pacific Islander communities at increased risk for adverse impacts from COVID-19, an effective response is critical. This article, authored by members of the Team’s Policy Committee, discusses the development of a cultural framework that guides its advocacy efforts. The Policy Committee’s work presents a cultural framework that grounds and guides their efforts for effectively promoting a strong voice in governmental and agency policies which would ultimately contribute to a healthy and thriving Native Hawaiian and Pacific Islander community. ©Copyright 2021 by University Health Partners of Hawai‘i (UHP Hawai‘i).                                                           |
| 34704068 | The Critical Role Hawai’i’s Community Health Workers Are Playing in COVID-19 Response Efforts.                                               | Community health workers play an instrumental role in the health care system and are critical partners in pandemic response. In Hawai’i, community health workers are working to reduce the burden of chronic disease among Pacific Islander, Filipino, and Native Hawaiian populations in partnership with government agencies and health care organizations. This commentary reviews the role community health workers in Hawai’i are playing in assisting with the COVID-19 response. Utilizing their skills and the community’s trust, they are optimally positioned to reach marginalized and vulnerable populations hit hardest by COVID-19; community health workers educate, screen, and provide social service referrals to community members. ©Copyright 2021 by University Health Partners of Hawai‘i (UHP Hawai‘i).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| 34704067 | Addressing Native Hawaiian and Pacific Islander Data Deficiencies Through a Community-based Collaborative Response to the COVID-19 Pandemic. | Early evidence of disproportionate COVID-19 infection and death rates in Native Hawaiian and Pacific Islander communities in the continental US raised concerns for similar disparities in Hawai’i, where these communities make up 25% of the state’s population. Representatives from more than 40 different government, academic, institutional and community-based organizations partnered to form the Hawai’i Native Hawaiian and Pacific Islander COVID-19 Response, Recovery, and Resilience Team. The team consists of 5 committees including the Data & Research Committee. This committee is tasked with examining issues regarding the acquisition, quality, public reporting, and utilization of race/ethnicity-related health data used to inform priorities and guide resource allocation. Problems addressed by this committee include: inconsistency across agencies in the use of race identifiers, defaulting to the Office of Management and Budget standards which aggregated Native Hawaiian and Pacific Islanders, and methods of data collection and reporting by the Department of Health. Outcomes include: 2 forms with race categories that reflect the population of Hawai’i; the reporting of disaggregated data by the Department of Health; and conversations with testing sites, laboratories, and health institutions urging a standardized form for race/ethnicity data collection. The collection and reporting of disaggregated race/ethnicity data is critical to guiding organizations in addressing underlying inequities in chronic disease and social determinants of health that can exacerbate the adverse effects of COVID-19. The Data and Research Committee’s network offers a community-based model for collaborative work that honors culture and ensures Native Hawaiian, Pacific Islander, and other minority populations are recognized and counted. ©Copyright 2021 by University Health Partners of Hawai‘i (UHP Hawai‘i). |

Covid19 and Hawaii
