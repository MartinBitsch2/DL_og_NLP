
#DEN NEDERSTE SEKTION:
#HARRY OG HARMONIE I BOG 7





library(tidyverse)
library(ggraph)
library(igraph)
library(pdftools)
library(tidytext)
library(janeaustenr)
hpworksdf <- readRDS("hpworksdf_med_titler.rds")
data("stop_words")


#DATA RETRIEVAL
jsworks <- austen_books()
hpworks <- pdf_text("harrypotter.pdf")
hpworksdf <- as.data.frame(hpworks)

#FEATURE ENGINEERING
all_contents <- hpworksdf %>% filter(str_detect(hpworks, "CONTENTS"))

split_text <- strsplit(all_contents[1,], "\n")[[1]]
clean_text <- trimws(split_text)
books <- clean_text[clean_text != "" & clean_text != "CONTENTS"]

tekst_kolonne <- hpworksdf$hpworks
na_vektor <- rep(NA, length(tekst_kolonne))
start_raekker <- which(str_detect(tekst_kolonne, "CONTENTS\\s+ONE"))
na_vektor[start_raekker] <- books
hpworksdf$Bogtitel <- na_vektor
hpworksdf <- hpworksdf %>%
  fill(Bogtitel, .direction = "down")


###############################################################################
#Wulf Script
###############################################################################

# jane aust
jsss <- jsworks %>% filter(book=="Sense & Sensibility")
# bigrams
jsswords <- jsss %>% unnest_tokens(words,text,token = "words")
jsswordscount <- jsswords %>% count(words, sort = T)
stweng=stop_words
jssbigrams <- jsss %>% unnest_tokens(bigrams,text,token = "ngrams", n=2)
jssbigrams <- jssbigrams %>% separate(bigrams, c("w1","w2"))

# filter on gender He,S
flist=c("He","he","She","she")
jssbigramsGender <- jssbigrams %>% filter(w1 %in% flist)
jssbigramsGenderCount <- jssbigramsGender %>% group_by(w1) %>% count(w2)
jssbigramsGenderCount$w1 <- tolower(jssbigramsGenderCount$w1)

#male

dfm=jssbigramsGenderCount  %>% filter(w1=="he")
dff=jssbigramsGenderCount  %>% filter(w1=="she")
common = inner_join(dfm,dff,by="w2")
commonv=unlist(common[,'w2'])
jssbigramsGenderCountCommon = jssbigramsGenderCount %>% filter(w2 %in% commonv)
plojss=jssbigramsGenderCountCommon %>% filter(n>10)
plojssAll=jssbigramsGenderCount %>% filter(n>10)
ggplot(plojssAll, aes(x=reorder(w2,n),y=n, fill = as.factor(w1)))+
  geom_bar(stat="identity",position = "dodge")+
  coord_flip()


plotgender <- jssbigramsGenderCountCommon %>% 
  filter(n>10) %>% 
  graph_from_data_frame()

plotgender <- jssbigramsGenderCount %>% 
  filter(n>10) %>% 
  graph_from_data_frame()
ggraph(plotgender, layout = "fr")+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name))


##############################################################################
#Harry/Harmonie i bog 7
##############################################################################
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)

# 1. Filtrer på bogen "Harry Potter and the Prisoner of Azkaban"
jsss <- hpworksdf %>% filter(Bogtitel == "Harry Potter and the Deathly Hallows")

# 2. Lav bigrams
jssbigrams <- jsss %>% unnest_tokens(bigrams, hpworks, token = "ngrams", n = 2)
jssbigrams <- jssbigrams %>% separate(bigrams, c("w1", "w2"))

# 3. Filtrer på navne
flist <- c("harry", "hermione") 
jssbigramsGender <- jssbigrams %>% filter(w1 %in% flist)

# --- DET NYE TRIN: FJERN STOPORD ---
# Vi fjerner rækker, hvor w2 findes i listen af stop_words
jssbigrams_uden_stopord <- jssbigramsGender %>%
  anti_join(stop_words, by = c("w2" = "word")) 
# -----------------------------------

# 4. Tæl ordene
jssbigramsGenderCount <- jssbigramsGender %>% 
  group_by(w1) %>% 
  count(w2, sort = TRUE) %>% 
  ungroup()

# 5. Find ord der KUN er brugt af den ene eller den anden karakter
# Først tæller vi, hvor mange karakterer (w1) der bruger hvert ord (w2)
ord_karakter_count <- jssbigramsGenderCount %>%
  group_by(w2) %>%
  summarize(antal_karakterer = n()) %>%
  ungroup()

# Nu beholder vi KUN de ord (w2), hvor antal_karakterer == 1 
# (altså ord der ikke deles af Harry og Hermione)
unikke_ord <- ord_karakter_count %>% filter(antal_karakterer == 1)

# Vi filtrerer vores store datasæt, så vi kun har de unikke ord
plot_data_unik <- jssbigramsGenderCount %>% 
  filter(w2 %in% unikke_ord$w2)

# 6. Gør dataen klar til plot (vælg top 10 for hver karakter)
plot_data_top10 <- plot_data_unik %>%
  # Vi fjerner "s" som før
  filter(w2 != "s") %>% 
  group_by(w1) %>%
  # Tag de 10 mest brugte unikke ord for hver karakter
  top_n(10, n) %>% 
  ungroup() %>%
  # Denne linje er VIGTIG for, at ordene sorteres pænt inden i hvert 'facet'
  mutate(w2 = reorder_within(w2, n, w1)) 

# 7. Lav selve plottet
ggplot(plot_data_top10, aes(x = w2, y = n, fill = w1)) +
  geom_col(show.legend = FALSE) + # geom_col er ofte nemmere end geom_bar(stat="identity")
  # facet_wrap er hemmeligheden til at dele det op i to!
  # scales = "free_y" sikrer, at hver side har sine egne ord på aksen
  facet_wrap(~w1, scales = "free_y", labeller = labeller(w1 = c(harry = "Harry", hermione = "Hermione"))) + 
  coord_flip() +
  scale_x_reordered() + # Rydder op i akse-navnene efter vi brugte reorder_within
  scale_fill_manual(values = c("harry" = "#E67451", "hermione" = "#00A3A3")) + # Sætter farverne så de ligner dit billede
  labs(
    title = "Ord kun brugt efter Harry og Hermione i\nHarry Potter and the Deathly Hallows",
    x = "Ord",
    y = "Antal"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    strip.text = element_text(size = 12), # Gør overskrifterne (Harry / Hermione) større
    panel.grid.major.y = element_blank() # Fjerner vandrette streger for et renere look
  )

