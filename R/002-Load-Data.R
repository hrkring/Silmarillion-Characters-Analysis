
# The Silmarillion

Silmarillion_Characters <- readr::read_csv(here::here("data", "Silmarillion", "Silmarillion Characters.csv")) |>
  tidyr::separate(col = 2, into = c(1:7) |> as.character(), sep = ", ") |>
  pivot_longer(cols = c(2:8), names_to = "1", values_to = "Alt.Character.Name") |>
  select(-c(`1`)) |>
  drop_na() |>
  mutate(Character.Name.Plain = tolower(stringi::stri_trans_general(Character.Name, id = "Latin-ASCII")),
         Alt.Character.Name.Plain = tolower(stringi::stri_trans_general(Alt.Character.Name, id = "Latin-ASCII"))) |>
  arrange(Character.Name.Plain)

saveRDS(Silmarillion_Characters, here::here("data", "Silmarillion_Characters.Rds"))

Silm_Chapter_Names <- readr::read_csv(here::here("data", "Silmarillion", "Silmarillion Chapters.csv")) |>
  pull()

Silm_Chapters <- lapply(Silm_Chapter_Names, function(x) {
  readr::read_file(here::here("data", "Silmarillion", paste0(x, ".txt"))) |>
    stringr::str_split(pattern = "[\\.\\?] ") |>
    as.data.frame()
})


# The Children of Hurin

CoH_Characters <- readr::read_csv(here::here("data", "The Children of Hurin", "CoH Characters.csv")) |>
  distinct() |>
  tidyr::separate(col = 2, into = c(1:7) |> as.character(), sep = ", ") |>
  pivot_longer(cols = c(2:8), names_to = "1", values_to = "Alt.Character.Name") |>
  select(-c(`1`)) |>
  drop_na() |>
  mutate(Character.Name.Plain = tolower(stringi::stri_trans_general(Character.Name, id = "Latin-ASCII")),
         Alt.Character.Name.Plain = tolower(stringi::stri_trans_general(Alt.Character.Name, id = "Latin-ASCII"))) |>
  arrange(Character.Name.Plain)

saveRDS(CoH_Characters, here::here("data", "CoH_Characters.Rds"))

CoH_Chapter_Names <- readr::read_csv(here::here("data", "The Children of Hurin", "CoH Chapters.csv")) |>
  pull()

CoH_Chapters <- lapply(CoH_Chapter_Names, function(x) {
  readr::read_file(here::here("data", "The Children of Hurin", paste0(x, ".txt"))) |>
    stringr::str_split(pattern = "[\\.\\?] ") |>
    as.data.frame()
})
