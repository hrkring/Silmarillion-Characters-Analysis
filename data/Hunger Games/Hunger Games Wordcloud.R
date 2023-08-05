HG_Characters <- readr::read_csv(here::here("data", "Hunger Games Characters.csv")) |>
  mutate(Character.Name = tolower(Character.Name)) |>
  arrange(Character.Name)

HG_Text <- readr::read_file(here::here("data", "Hunger Games.txt"))

HG_Text <- HG_Text |>
  stringr::str_squish() |>
  stringr::str_replace_all(pattern = "'s", replacement = "")

Text <- Corpus(VectorSource(HG_Text))

Text_Clean <- Text |>
  tm_map(removeNumbers) |>
  tm_map(removePunctuation) |>
  tm_map(stripWhitespace) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removeWords, stopwords("english"))

dtm <- TermDocumentMatrix(Text_Clean)
matrix <- as.matrix(dtm)
words <- sort(rowSums(matrix), decreasing = TRUE)
df <- data.frame(
  word = names(words),
  freq = words
) |>
  #filter(word %in% HG_Characters$Character.Name) |>
  summarise(freq = sum(freq), .by = word) |>
  arrange(word)

# df$word <- HG_Characters |>
#   filter(Character.Name %in% df$word) |>
#   distinct(Character.Name) |>
#   pull(Character.Name)

set.seed(1)
wordcloud(
  words = df$word,
  freq = df$freq,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
