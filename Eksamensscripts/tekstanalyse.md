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

## gutenberg, Jane Austen, AssociatedPress

``` r
library(gutenbergr)
gutmeta <- gutenberg_metadata

#The Declaration of Independence of the United States of America
DoI <- gutenberg_download(1)
```

``` r
library(janeaustenr)
ab <- austen_books()
```

Dette er på DTM-form

``` r
library(tm)

data("AssociatedPress", package = "topicmodels")
ap <- AssociatedPress
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

Zipf’s lov er en relation mellem hyppighed af af ord og rangeringen. Den
lyder således:

> *“Hyppigheden af et ords optræden er omvendt proportionel med dets
> rangering”*

Demonstration. Vi bruger Anker Jørgensens taler. Her finder vi først
termhyppigheden, som er antal gange et ord optræder i et dokument
divideret med det totalte antal ord i dokumentet.

Herunder er disse hyppigheder grupperet, hvor det kan observeres at der
i Anker Jørgensens taler er mellem 400 og 500 ord som har den laveste
hyppighed. Det vil være ord som kun optræder 1 gang i hele talen.

``` r
library(dplyr)
library(tidytext)
library(ggplot2)

book_words <- AJ1 %>%
  unnest_tokens(word, text) %>%
  count(document, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(document) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

ggplot(book_words, aes(log(n/total), fill = document)) +
  geom_histogram(show.legend = FALSE, binwidth = 0.1) +
  xlim(NA, 0.2) +
  facet_wrap(~document, ncol = 2, scales = "free_y")
```

Dette er grundlæggende Zipf’s lov. Men det kommer særligt til udtryk ved
denne distribution, hvor det kan obeserveres at alle termhyppigheder
følger hinanden.

``` r
freq_by_rank <- book_words %>% 
  group_by(document) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/total) %>%
  ungroup()

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, color = document)) + 
  geom_line(linewidth = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()
```

<figure>
<img
src="https://raw.githubusercontent.com/MartinBitsch2/DL_og_NLP/refs/heads/main/Eksamensscripts/billeder/000002.png"
alt="Zipf’s lov" />
<figcaption aria-hidden="true">Zipf’s lov</figcaption>
</figure>

Hvis vi fitter en lineær funktion til værdierne $100\gt rank\gt 10$ får
vi en hældning på -1,0774. Afvigelserne i de høje range (ca. 1-5) er dog
meget normale for mange sprog. Det er her de sjælne ord ligger.

``` r
rank_subset <- freq_by_rank %>% 
  filter(rank < 100,
         rank > 10)

lm(log10(term_frequency) ~ log10(rank), data = rank_subset)
```

## topic modelling (LDA, beta, gamma)

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

Vi bruger igen Anker Jørgensens taler til dette eksempel. For at køre
LDA kræves det at dataen er på DTM-form.

Processen er derfor:

> DF med dokumentnr i en kolonne og brødtekst i anden kolonne =\> Lave
> word-count som tæller hvor mange gange hvert ord optræder i hvert
> dokument =\> lave DTM som danner en matrix ud af word-count =\> LDA

``` r
AJ_word_counts <- AJ1 %>%
  unnest_tokens(word, text) %>%
  anti_join(danish_stopwords) %>% 
  filter(!str_detect(word, "\\d")) %>% #fjerner alle ord med tal
  count(document, word, sort = TRUE) 

AJ_dtm <- AJ_word_counts %>%
  cast_dtm(document, word, n)
```

k er her antallet af topics

``` r
AJ_lda <- LDA(AJ_dtm, k = 2, control = list(seed = 1234))
```

**Beta**

Hvilke ord der karakteriserer hvert emne. For hver kombination beregnes
sandsynligheden for at det term bliver genereret for det emne.

``` r
aj_topics <- tidy(AJ_lda, matrix = "beta")
```

Her kigger vi på top 10 termer i hvert emne

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

<figure>
<img
src="https://raw.githubusercontent.com/MartinBitsch2/DL_og_NLP/refs/heads/main/Eksamensscripts/billeder/000011.png"
alt="top 10" />
<figcaption aria-hidden="true">top 10</figcaption>
</figure>

Vi kan også visualisere de ord som har størst afstand i $\beta$ værdi

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

Plottet viser at emnerne er meget adskilt, men dette kan være kunstigt,
fordi det er taler fra den samme person.

**document modelling (gamma)**

Hvilke emner fylder i de forskellige dokumenter.

Værdierne angiver det estimerede forhold af ord fra et dokument som er
genereret fra et givet emne. F.eks. er dette fordelingen i dette data
meget skævt. For alle 8 taler vil over 99,99% af ordene tilhøre et af
emnerne mens de resterende under 0,01% tilhører det andet emne. Derfor
tilbage til pointen om at emnedelingen er kunstig.

``` r
aj_documents <- tidy(AJ_lda, matrix = "gamma")
aj_documents
```

Her vist som plots

``` r
gamma_wide <- aj_documents %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = gamma) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

gamma_wide %>%
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

[Gamma
billede](https://raw.githubusercontent.com/MartinBitsch2/DL_og_NLP/refs/heads/main/Eksamensscripts/billeder/002.png)

# Wordclouds + bigrams (n-grams) + igraph

**Bigrams**

Bruges sammen med unnest_tokens til at kigge på sekvenser af ord. F.eks.
ved at kigge på hvor ofte ord x er efterfulgt af ord y, kan vi bygge en
model som for forholdet mellem dem.

n sættes som det antal ord i træk man vil kigge på. derfor n-grams.
**bigrams** er derfor n=2.

``` r
austen_bigrams <- ab %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% #bigrams i starten er bare hvad kolonnen kommer til at hedde.
  filter(!is.na(bigram))
```

Vi kan herefter seperere sætningerne til videre analyse. Her bruges
*seperate*-funktionen til at skille bigrams ad, så hvert ord får sin
egen kolonne.

I den nye DF fjernes rækker hvor stopord indgår

``` r
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
```

**igraph**

**Wordcloud**

``` r
ab
```
