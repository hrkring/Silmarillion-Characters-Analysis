Silmarillion_Characters <- readr::read_csv(here::here("data", "Silmarillion Characters.csv"))

Silmarillion_Characters <- Silmarillion_Characters |>
  tidyr::separate(col = 2, into = c(1:7) |> as.character(), sep = ", ") |>
  pivot_longer(cols = c(2:8), names_to = "1", values_to = "Alt.Character.Name") |>
  select(-c(`1`)) |>
  drop_na()

Silmarillion_Characters <- Silmarillion_Characters |>
  mutate(Character.Name.Plain = tolower(stringi::stri_trans_general(Character.Name, id = "Latin-ASCII")),
         Alt.Character.Name.Plain = tolower(stringi::stri_trans_general(Alt.Character.Name, id = "Latin-ASCII"))) |>
  arrange(Character.Name.Plain)

Silmarillion_Text <- readr::read_file(here::here("data", "Silmarillion.txt"))

Silm_Text_Chapters <- Silmarillion_Text |>
  stringr::str_squish() |>
  stringr::str_split(pattern = "CHAPTER")

for (i in 1:length(Silm_Text_Chapters[[1]])) {
  Silm_Text_Chapters[[1]][i] <- Silm_Text_Chapters[[1]][i] |>
    stringr::str_squish() |>
    stringr::str_split_1(pattern = "[\\.\\?] ") |>
    as.data.frame()
}

# Beren and Luthien Chapter
Silm_Text_Sentences <- data.frame(Silm_Text_Chapters[[1]][[20]])
