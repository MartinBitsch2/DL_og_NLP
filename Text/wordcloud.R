library(wordcloud)
library(pdftools)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
url <- "https://gist.githubusercontent.com/berteltorp/0cf8a0c7afea7f25ed754f24cfc2467b/raw/fa34ef448aff6adbb4b6bab9bda62a8b0f1ee597/stopord.txt"


auto <- pdf_text("C:\\Users\\marti\\Downloads\\Kopi af Afsluttende eksamensprojekt - Procesoptimering af eftermarkedet hos Ejner Hessel, Kastrup.pdf")
autodf <- as.data.frame(auto)
autodf <- autodf %>% unnest_tokens(words,auto,token = "words")



# 2. Hent ordene fra nettet og gem dem i en tibble
# Kolonnenavnet sættes til "words", så det matcher din eksisterende dataframe
danske_stopord <- tibble(words = readLines(url, encoding = "UTF-8"))

# Skriv de specifikke ord ind, du selv vil af med (f.eks. navne, tal eller irrelevante koder)
egne_stopord <- tibble(words = c(
  "martin", "dahlskov", "bitsch", "afsluttende", 
  "eksamensprojekt", "4", "semester", "mab002", 
  "edu.zealand.dk", "holdnummer", "aut", "kø", "f", "s23a", "sem4", "antal", 1:46, "xxx"
))

# Saml dem til én liste
alle_stopord <- bind_rows(danske_stopord, egne_stopord)

# 2. Filtrer data, tæl og tegn wordclouden
autodf %>% # HUSK: Skift 'autodf' ud med det din dataframe faktisk hedder
  anti_join(alle_stopord, by = "words") %>%    # Fjerner alle stopordene
  count(words, name = "freq", sort = TRUE) %>% # Tæller ordene
  filter(freq >= 2) %>%                        # (Valgfrit) Beholder kun ord, der optræder mindst 2 gange
  slice_max(freq, n = 100) %>%                 # Tager top 100 mest brugte ord
  ggplot(aes(label = words, size = freq, color = freq)) +
  geom_text_wordcloud_area() +                 # Tegner selve skyen
  scale_size_area(max_size = 15) +             # Justerer max-størrelsen på de største ord
  scale_color_gradient(low = "darkgray", high = "darkblue") + # Giver farve efter frekvens
  theme_minimal()
