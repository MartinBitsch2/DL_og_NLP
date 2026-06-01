Tekstanalyse
================

# Load text:

## API-kald fra dansketaler.dk

**Statuskoder** <br> Kan ses under variabal af GET-request -\>
status_code

- 200: forespørgsel succesfuld
- 404: side ikke fundet
- 500: serverfejl

``` r
library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)
danish_stopwords <- readRDS("danish_stopwords.rds")

url = "https://www.dansketaler.dk/api/v1/speeches"

res <- GET(url, query = list(per_page = 50, q = "Anker Jørgensens nytårstale", fields = "title"))
rescontent <- content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)

df <- as.data.frame(resretval[["speeches"]])
df <- unnest(df, cols = date)

AJ <- c()
for(i in 1:8){
AJ <- c(AJ, df[i,"transcription"])}



AJ1 <- tibble(document = paste("Tale", 1:length(AJ)),  text = AJ)
AJ1[,1] <- df[,"iso_date"]
```

## gutenberg

``` r
library(gutenbergr)
gutmeta <- gutenberg_metadata

#The Declaration of Independence of the United States of America
DoI <- gutenberg_download(1)
```
