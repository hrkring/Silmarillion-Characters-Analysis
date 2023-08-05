# Build character frequencies by chapter -------------

Characters_By_Chapter <- data.frame(Character = unique(CoH_Characters$Character.Name))
for (i in 1:length(CoH_Chapters)) {
  Text <- Corpus(VectorSource(CoH_Chapters[[i]] |> unlist() |> paste0( collapse = ". ")))

  Text_Clean <- Text |>
    tm_map(removeNumbers) |>
    tm_map(removePunctuation) |>
    tm_map(stripWhitespace) |>
    tm_map(content_transformer(tolower)) |>
    tm_map(removeWords, stopwords("english"))

  tdm <- TermDocumentMatrix(Text_Clean)
  matrix <- as.matrix(tdm)
  words <- sort(rowSums(matrix), decreasing = TRUE)

  Chapter_Characters <- data.frame(
    word = names(words),
    freq = words,
    row.names = names(words)
  ) |>
    filter(word %in% Silmarillion_Characters$Alt.Character.Name.Plain) |>
    left_join(Silmarillion_Characters, by = c("word" = "Alt.Character.Name.Plain")) |>
    mutate(word = Character.Name) |>
    summarise(freq = sum(freq), .by = word) |>
    arrange(word)

  names(Chapter_Characters) <- c("Character", paste0("Chapter", i))

  Characters_By_Chapter <- Characters_By_Chapter |>
    left_join(Chapter_Characters, by = c("Character"))
}

Characters_By_Chapter <- Characters_By_Chapter |>
  mutate(across(everything(), ~ replace_na(., 0))) |>
  pivot_longer(cols = 2:ncol(Characters_By_Chapter), names_to = "Chapter", values_to = "Frequency") |>
  mutate(Importance = (Frequency / sum(Frequency)), .by = c(Chapter))

Characters_By_Chapter$Chapter <- factor(Characters_By_Chapter$Chapter, levels = stringr::str_sort(unique(Characters_By_Chapter$Chapter), numeric = TRUE))

# Slicers

First_Born <- c("Finwë", "Olwë", "Thingol")
Feanor_House <- c("Fëanor", "Maedhros", "Maglor", "Caranthir", "Curufin", "Celegorm", "Amrod", "Amras", "Celebrimbor")
Fingolfin_House <- c("Fingolfin", "Fingon", "Turgon", "Aredhel", "Argon", "Idril")
Finarfin_House <- c("Finarfin", "Finrod", "Galadriel", "Angrod", "Aegnor", "Orodreth", "Finduilas")
Valar_Lords <- c("Manwë", "Tulkas", "Oromë", "Ulmo", "Mandos", "Aulë", "Irmo")
Valar_Queens <- c("Varda", "Estë", "Yavanna", "Nessa", "Nienna", "Vairë", "Vána")
Maiar <- c("Tilion", "Arien", "Ossë", "Uinen", "Melian", "Eönwë")
Enemies <- c("Morgoth", "Sauron", "Ungoliant", "Gothmog", "Ancalagon", "Glaurung")

Hurin_Family <- c("Húrin", "Túrin", "Morwen", "Nienor", "Lalaith")
Sindar <- c("Beleg", "Mablung", "Thingol", "Melian")
CoH_Enemies <- c("Morgoth", "Glaurung", "Gothmog", "Mîm")

Plot_Data <- Characters_By_Chapter |>
  filter(Character %in% CoH_Enemies)

Max_ylim <- Plot_Data |>
  filter(Importance == max(Importance)) |>
  pull(Importance)

ggplot(data = Plot_Data, mapping = aes(x = Chapter, y = Importance, group = Character, color = Character)) +
  theme_bw() +
  stat_smooth(se = FALSE, span = .45) +
  theme(axis.text.x = element_text(angle = 90, size = 6)) +
  coord_cartesian(ylim = c(0, Max_ylim), expand = FALSE) +
  scale_y_continuous(labels = scales::percent_format())
