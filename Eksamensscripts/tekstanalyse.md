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
library(tm)

madkatpdf <- data.frame()
for(i in 1:length(pdf)){
temp <- data.frame(doc=rep(i, times=length(pdf[[i]])), text=pdf[[i]])
madkatpdf <- rbind(madkatpdf, temp)
}
```

**Wordcount**

- Tæller antallet af hvert ord i hvert dokument. Dette bruges i DTM
- Udregner hvor stor en procentdel ordet udgør af det totale antal ord
  pr. dokument

``` r
word_counts <- madkatpdf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "\\d")) %>% #fjerner alle ord med tal
  count(doc, word, sort = TRUE) 

wc_procent <- word_counts %>%
  group_by(doc) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup()
```

## DTM, TF-IDF, Zipf’s lov

[Link til kap. 5 i Tidytextmining](https://www.tidytextmining.com/dtm)

**DTM (document-term matrix**

- Matrix hvor hver række repræsenterer et dokument (f.eks. en bog eller
  en artikel)
- hver kolonne repræsenterer et term
- hver værdi typisk indeholder antalle af gange som det term har optrådt
  i dokumentet

Når madkat-dtm kører får man en sparsity på 46%. Dette betyder at 46% af
cellerne har nulværdier. Med to dokumenter betyder det at der er et
“termoverlap” på: $1-\frac{46}{50}=0.08$

``` r
madkat_dtm <- word_counts %>%
  cast_dtm(doc, word, n)

madkat_dtm_viz <- as.matrix(t(madkat_dtm))
```

**TF-IDF (Term frequency - inverse document frequency**

Istedet for blot at kigge på vigtighed af ord i et dokument som
termhyppigheden (TF) - hvor ofte et ord optræder i et dokument, kan vi
tage termernes inverse dokumenthyppighed (IDF) med i ligningen. Dette
gør at mindre brugte ord får en større stemme. Ved at gange TF og IDF
sammen opnås et mere balanceret billede.

TF-IDF kan forstås som hyppigheden af et term justeret for hvor ofte det
optræder.

$$idf(term)=ln\begin{pmatrix}\frac{n_{documents}}{n_{documents\ containing\ term}}\end{pmatrix}$$

``` r
madkat_tf_idf <- word_counts %>%
  bind_tf_idf(word, doc, n) %>%
  arrange(desc(tf_idf))
```

Vi kan derfor se at ord der er unikke for kun et eller få dokumenter får
et boost.

**Zipf’s lov**

[kap. 3.2 i
tidytextmining](https://www.tidytextmining.com/tfidf#zipfs-law)

## topic modelling (beta og LDA)

**LDA (Latent Dirichlet allocation)**

[Link til kap. 6 i
Tidytextmining](https://www.tidytextmining.com/topicmodeling)

LDA er en metode indefor *topic modelling* som behandler hvert dokument
som en blanding af emner, og hvert emne som en blanding af ord. Dette
tillader at dokumenters indhold overlapper istedet for at være sepereret
i grupper.

F.eks. kan en toemnemodel med emnerne politik og underholdning, have ord
som kun tilhører de to kategorier repektivt, f.eks. forhandlinger og
minister til politik og film og skuespiller til underholdning. Der kan
dog så også være ord som indgår i begge emner som f.eks. København og
budget. Vi bruger **LDA** til at bestemme hvordan ordende fordeler sig
indefor emnerne, men også hvilke emner som kendetegner dokumenterne.

Vi bruger Anker Jørgensens taler til dette eksempel

madkat ville se sådan ud:

``` r
madkat_lda <- LDA(madkat_dtm, k = 2, control = list(seed = 1234))
madkat_lda
```

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

# Sentiment i Python med Sentida

``` python
import 
```

Wordscloud bigrams
