library(rvest)

Link <- "https://tolkiengateway.net/wiki/Category:Characters_in_The_Silmarillion"

Characters_Page <- read_html(Link)

Characters_Table <- Characters_Page |>
  html_elements(css = ".mw-category")

html_text(Characters_Table) |>
  stringr::str_split("\n") |>
  as.data.frame() |>
  dplyr::rename(Character.Name = 1) |>
  dplyr::mutate(Character.Name = stringr::str_replace(Character.Name, "\\s*\\([^\\)]+\\)", ""),
                Character.Name = stringr::str_replace(Character.Name, "[A-Z]*$", ""))
