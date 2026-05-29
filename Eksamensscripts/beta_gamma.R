library(httr)
library(jsonlite)
library(readr)
danish_stopwords <- readRDS("danish_stopwords.rds")

url = "https://www.dansketaler.dk/api/v1/speeches"

res <- GET(url, query = list(per_page = 50, q = "Anker Jørgensens nytårstale", fields = "title"))
rescontent <- content(res, as="text")
resretval <- jsonlite::fromJSON(rescontent)

df <- as.data.frame(resretval[["speeches"]])
df <- t(df)
df <- unnest(df, cols = date)

AJ <- c(df[1,"transcription"], df[2,"transcription"], df[3,"transcription"],df[4,"transcription"],df[5,"transcription"],
        df[6,"transcription"],df[7,"transcription"], df[8,"transcription"])



AJ1 <- tibble(
  document = paste("Page", 1:length(AJ)), # Creates ID names like "Page 1"
  text = AJ)
AJ1[,1] <- df[,"iso_date"]

word_counts <- AJ1 %>%
  unnest_tokens(word, text) %>%
  anti_join(danish_stopwords) %>%            
  filter(!str_detect(word, "\\d")) %>% 
  filter(!str_detect(word, "\\.")) %>%  # <-- fjerner ord med punktum
  count(document, word, sort = TRUE)

aj_dtm <- word_counts %>%
  cast_dtm(document, word, n)

aj_lda <- LDA(aj_dtm, k = 2, control = list(seed = 1234))

#BETA
aj_topics <- tidy(aj_lda, matrix = "beta") #Hvilke ord der karakteriserer hvert emne

#GAMMA
aj_documents <- tidy(aj_lda, matrix = "gamma") #hvilke emner fylder i de forskellige dokumenter


#TOP 10 ORD I HVER KATEGORI
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






######################################
#BETA
#####################################
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



######################################
#GAMMA
#####################################
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


