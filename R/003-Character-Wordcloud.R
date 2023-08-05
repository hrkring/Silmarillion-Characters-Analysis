
Text <- Corpus(VectorSource(Silm_Chapters[[22]])) |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, stopwords("english"))

tdm <- TermDocumentMatrix(Text)
matrix <- as.matrix(tdm)
words <- sort(rowSums(matrix), decreasing = TRUE)

wordcloud_data <- data.frame(
  word = names(words),
  freq = words
) |>
  filter(word %in% Silmarillion_Characters$Alt.Character.Name.Plain) |>
  left_join(Silmarillion_Characters, by = c("word" = "Alt.Character.Name.Plain")) |>
  mutate(word = Character.Name) |>
  summarise(freq = sum(freq), .by = word) |>
  arrange(word)

set.seed(1)
wordcloud(
  words = wordcloud_data$word,
  freq = wordcloud_data$freq,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = RColorBrewer::brewer.pal(8, "Dark2")
)
