library(wordcloud)
library(pdftools)
library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(topicmodels)
library(ggplot2)
library(dplyr)

madkat <- c(pdf_text("C:\\Users\\marti\\Downloads\\global-food-systems-transformation-2025.pdf"), 
         pdf_text("C:\\Users\\marti\\Downloads\\About_Cats-Nicolae_Sfetcu-CCNS.pdf"))

madkatpdf <- tibble(
  document = paste("Page", 1:length(madkat)), # Creates ID names like "Page 1"
  text = madkat)

word_counts <- madkatpdf %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%            
  filter(!str_detect(word, "\\d")) %>% 
  count(document, word, sort = TRUE)        

madkat_dtm <- word_counts %>%
  cast_dtm(document, word, n)

madkat_lda <- LDA(madkat_dtm, k = 2, control = list(seed = 1234))
madkat_lda


madkat_topics <- tidy(madkat_lda, matrix = "beta")
madkat_topics




madkat_top_terms <- madkat_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

madkat_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

###################################################################################
library(tidyr)

beta_wide <- madkat_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))



beta_wide %>%
  group_by(direction = log_ratio > 0) %>%
  slice_max(abs(log_ratio), n = 30) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(x = log_ratio, y = term)) +
  geom_col() +
  labs(
    x = "Log2 ratio of beta in topic 2 / topic 1",
    y = NULL # Removes the "term" label from the y-axis for a cleaner look
  ) +
  theme_minimal()
