Tekstanalyse
================

# Load text:

## API-kald fra dansketaler.dk

**Statuskoder**

Kan ses under variabal af GET-request -\> status_code

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

## load PDF’er

``` r
library(pdftools)
sti <- "C:\\Users\\marti\\Documents\\Git\\EK\\2. sem\\Deep Learning og NLP\\Text"

pdf <- list()
pdf[[1]] <- pdf_text(paste0(sti, "/global-food-systems-transformation-2025.pdf")) 
pdf[[2]] <- pdf_text(paste0(sti,"/About_Cats-Nicolae_Sfetcu-CCNS.pdf"))
```

# Analyse

**Laver en lang dataframe med alle PDF’er**

``` r
library(wordcloud)
library(pdftools)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(topicmodels)
library(ggplot2)
library(dplyr)
library(quanteda)

madkatpdf <- data.frame()
for(i in 1:length(pdf)){
temp <- data.frame(doc=rep(i, times=length(pdf[[i]])), text=pdf[[i]])
madkatpdf <- rbind(madkatpdf, temp)
}
```

**Wordcount**

``` r
word_counts <- madkatpdf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%            
  filter(!str_detect(word, "\\d")) %>% 
  count(doc, word, sort = TRUE) 
```

## DTM og LDA

``` r
madkat_dtm <- word_counts %>%
  cast_dtm(doc, word, n)

madkat_lda <- LDA(madkat_dtm, k = 2, control = list(seed = 1234))
madkat_lda
```

## topic modelling (beta)

``` r
aj_topics <- tidy(aj_lda, matrix = "beta") #Hvilke ord der karakteriserer hvert emne
```

**TOP 10 ORD I HVER KATEGORI**

``` r
aj_top_terms <- aj_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

aj_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
```

**Visualisering af ord med størst afstand**

``` r
beta_wide <- aj_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n = 10) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(x = log_ratio, y = term)) +
  geom_col() +
  labs(
    x = "Log2 ratio of beta in topic 2 / topic 1",
    y = NULL # Removes the "term" label from the y-axis for a cleaner look
  ) +
  theme_minimal()
```

## document modelling (gamma)

``` r
aj_documents <- tidy(aj_lda, matrix = "gamma") #hvilke emner fylder i de forskellige dokumenter
```

``` r
beta_wide <- aj_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = gamma) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n = 8) %>%
  ungroup() %>%
  mutate(document = reorder(document, log_ratio)) %>%
  ggplot(aes(x = log_ratio, y = document)) +
  geom_col() +
  labs(
    x = "Log2 ratio of beta in topic 2 / topic 1",
    y = NULL # Removes the "term" label from the y-axis for a cleaner look
  ) +
  theme_minimal()
```

Wordscloud bigrams
