Text <- Corpus(VectorSource(Silmarillion_Text))

Text_Clean <- Text |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(Text_Clean)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
wordcloud_data <- data.frame(
  word = names(words),
  freq = words
  ) |>
  filter(word %in% Silmarillion_Characters$Alt.Character.Name.Plain) |>
  left_join(Silmarillion_Characters, by = c("word" = "Alt.Character.Name.Plain")) |>
  mutate(word = Character.Name.Plain) |>
  summarise(freq = sum(freq), .by = word) |>
  arrange(word)

wordcloud_data$word <- Silmarillion_Characters |>
  filter(Character.Name.Plain %in% wordcloud_data$word) |>
  distinct(Character.Name) |>
  pull(Character.Name)

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


