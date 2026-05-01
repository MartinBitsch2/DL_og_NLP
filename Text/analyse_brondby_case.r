# Pakker
library(pdftools)
library(dplyr)
library(tibble)
library(tidytext)
library(stringr)
library(ggplot2)
library(stopwords)

# 1. Læs PDF til analyse af 2. semester opgave
text <- pdf_text("Eksamenscase med Brøndby.pdf")

original_case <- tibble(text = text) %>%
  mutate(page = row_number())

# 2. Gør teksten til enkelte ord
case_words <- original_case %>%
  unnest_tokens(word, text)

# 3. Fjern danske stopord
stop_words_da <- tibble(word = c(
  stopwords("da"),
  "kan", "skal", "vil", "der", "også", "fx"
))

case_words_clean <- case_words %>%
  anti_join(stop_words_da, by = "word") %>%
  filter(str_detect(word, "^[a-zæøå0-9]+$"))

# 4. Søgeord opdelt i kategorier
football_words <- c(
  "fodbold", "superliga", "brøndby", "spiller", "spillere",
  "hold", "kamp", "kampe", "liga", "sæson", "træner",
  "mål", "scoring", "taktik", "formation", "scouting",
  "talent", "talentudvikling", "transfer", "performance",
  "præstation", "matchanalyse"
)

data_words <- c(
  "xg", "expected", "goals", "data", "datasæt",
  "statistik", "model", "modeller", "algoritme",
  "clustering", "klyngeanalyse", "cluster", "klynger",
  "machine", "learning", "regression", "variabel",
  "variable", "observation", "mønster", "segmentering",
  "analyse", "kvantitativ"
)

science_words <- c(
  "videnskabsteori", "metode", "teori", "empiri",
  "positivisme", "hermeneutik", "socialkonstruktivisme",
  "erkendelse", "fortolkning", "objektivitet",
  "subjektivitet", "validitet", "reliabilitet",
  "generaliserbarhed", "hypotese"
)

ethics_words <- c(
  "etik", "moral", "ansvar", "fairness",
  "bias", "diskrimination", "gennemsigtighed",
  "privatliv", "databeskyttelse", "konsekvensetik",
  "pligtetik", "dydsetik"
)

law_words <- c(
  "jura", "lov", "lovgivning", "regler",
  "gdpr", "persondata", "samtykke", "rettigheder",
  "kontrakt", "compliance", "regulering", "ansvar"
)

# 5. Kategoriser ordene
case_words_categorized <- case_words_clean %>%
  mutate(category = case_when(
    word %in% football_words ~ "Fodbold / Superliga",
    word %in% data_words ~ "xG / clustering",
    word %in% science_words ~ "Videnskabsteori",
    word %in% ethics_words ~ "Etik",
    word %in% law_words ~ "Jura",
    TRUE ~ "Andet"
  ))

# 6. Visualisering 1: Mest brugte ord samlet
case_words_clean %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 20) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "De 20 mest brugte ord i casen",
    x = "Ord",
    y = "Antal"
  ) +
  theme_minimal()

# 7. Visualisering 2: Fordeling mellem faglige kategorier
case_words_categorized %>%
  filter(category != "Andet") %>%
  count(category, sort = TRUE) %>%
  ggplot(aes(x = reorder(category, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Fordeling af centrale temaer i casen",
    x = "Tema",
    y = "Antal ord"
  ) +
  theme_minimal()

# 8. Visualisering 3: Mest brugte ord inden for hver kategori
case_words_categorized %>%
  filter(category != "Andet") %>%
  count(category, word, sort = TRUE) %>%
  group_by(category) %>%
  slice_max(n, n = 8) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ category, scales = "free") +
  labs(
    title = "Mest brugte ord inden for hver faglig kategori",
    x = "Ord",
    y = "Antal"
  ) +
  theme_minimal()

# 9. Visualisering 4: Opgavetype ud fra handlingsord
action_words <- c(
  "analysere", "analyse", "vurdere", "vurdering",
  "diskutere", "diskussion", "redegøre", "forklare",
  "beregne", "undersøge", "sammenligne", "argumentere",
  "perspektivere", "konkludere"
)

case_words_clean %>%
  filter(word %in% action_words) %>%
  count(word, sort = TRUE) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Handlingsord i casen",
    subtitle = "Viser hvilken type opgave casen lægger op til",
    x = "Handlingsord",
    y = "Antal"
  ) +
  theme_minimal()


################################################################################

# Pakker
library(pdftools)
library(dplyr)
library(tibble)
library(tidytext)
library(stringr)
library(ggplot2)
library(stopwords)

# 1. Læs PDF til analyse af gammel hovedopgave
text <- pdf_text("Vækstskabende tiltag.docx.pdf")

opgave <- tibble(text = text) %>%
  mutate(page = row_number())

# 2. Lav teksten om til ord
opgave_words <- opgave %>%
  unnest_tokens(word, text)

# 3. Fjern danske stopord og støj
stop_words_da <- tibble(word = stopwords("da"))

opgave_words_clean <- opgave_words %>%
  anti_join(stop_words_da, by = "word") %>%
  filter(str_detect(word, "^[a-zæøå0-9]+$")) %>%
  filter(nchar(word) > 2)

# 4. Definér søgeord/kategorier til sproganalyse

business_words <- c(
  "vækst", "vækstskabende", "virksomhed", "virksomheden",
  "strategi", "strategisk", "marked", "markedet",
  "konkurrence", "konkurrenter", "kunder", "kunde",
  "målgruppe", "forretning", "indtjening", "omsætning",
  "profit", "rentabilitet", "økonomi", "økonomisk"
)

marketing_words <- c(
  "marketing", "markedsføring", "brand", "branding",
  "positionering", "differentiering", "segmentering",
  "målgruppe", "kunderejse", "kommunikation",
  "kampagne", "kanaler", "sociale", "medier"
)

analysis_words <- c(
  "analyse", "analyserer", "analysere", "vurdere",
  "vurdering", "diskutere", "diskussion", "redegøre",
  "forklare", "undersøge", "sammenligne",
  "argumentere", "konkludere", "perspektivere"
)

method_words <- c(
  "metode", "teori", "model", "empiri",
  "kvalitativ", "kvantitativ", "interview",
  "spørgeskema", "data", "validitet",
  "reliabilitet", "kildekritik", "afgrænsning"
)

implementation_words <- c(
  "tiltag", "implementering", "anbefaling",
  "anbefalinger", "løsning", "løsninger",
  "forslag", "handling", "handlingsplan",
  "udvikling", "forbedring", "potentiale"
)

# 5. Kategorisér ordene
opgave_categorized <- opgave_words_clean %>%
  mutate(category = case_when(
    word %in% business_words ~ "Virksomhed / vækst",
    word %in% marketing_words ~ "Marketing",
    word %in% analysis_words ~ "Analyse / opgavetype",
    word %in% method_words ~ "Metode / teori",
    word %in% implementation_words ~ "Tiltag / anbefalinger",
    TRUE ~ "Andet"
  ))

# 6. Visualisering 1: De mest brugte ord
opgave_words_clean %>%
  count(word, sort = TRUE) %>%
  slice_max(n, n = 25) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "De mest brugte ord i hovedopgaven",
    x = "Ord",
    y = "Antal"
  ) +
  theme_minimal()

# 7. Visualisering 2: Fordeling af temaer
opgave_categorized %>%
  filter(category != "Andet") %>%
  count(category, sort = TRUE) %>%
  ggplot(aes(x = reorder(category, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Tematisk fordeling i hovedopgaven",
    x = "Tema",
    y = "Antal ord"
  ) +
  theme_minimal()

# 8. Visualisering 3: Mest brugte ord pr. kategori
opgave_categorized %>%
  filter(category != "Andet") %>%
  count(category, word, sort = TRUE) %>%
  group_by(category) %>%
  slice_max(n, n = 8) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ category, scales = "free") +
  labs(
    title = "Centrale ord inden for hver kategori",
    x = "Ord",
    y = "Antal"
  ) +
  theme_minimal()

# 9. Visualisering 4: Handlingsord/opgavetype
opgave_words_clean %>%
  filter(word %in% analysis_words) %>%
  count(word, sort = TRUE) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Handlingsord i hovedopgaven",
    subtitle = "Bruges til at vurdere hvilken type opgave det er",
    x = "Handlingsord",
    y = "Antal"
  ) +
  theme_minimal()

################################################################################

library(tidytext)
library(dplyr)
library(ggplot2)

# Hent Bing-leksikonet
bing <- get_sentiments("bing")

# Lav score: positive = +1, negative = -1
bing_score <- bing %>%
  mutate(score = ifelse(sentiment == "positive", 1, -1))

# Samlet sentiment-score for hele leksikonet
bing_total_score <- bing_score %>%
  summarise(
    positive_ord = sum(sentiment == "positive"),
    negative_ord = sum(sentiment == "negative"),
    samlet_score = sum(score)
  )

bing_total_score

bing_score %>%
  count(sentiment) %>%
  ggplot(aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3, size = 5) +
  labs(
    title = "Fordeling af positive og negative ord i Bing-leksikonet",
    x = "Sentiment",
    y = "Antal ord"
  ) +
  theme_minimal()
