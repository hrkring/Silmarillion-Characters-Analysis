library(rvest)

Links <- c("https://tolkiengateway.net/wiki/Category:Characters_in_The_Silmarillion", "https://tolkiengateway.net/w/index.php?title=Category:Characters_in_The_Silmarillion&pagefrom=Tuor#mw-pages")

Characters <- lapply(Links, function(x) {
  read_html(x) |>
    html_elements(css = ".mw-category") |>
    html_text() |>
    stringr::str_split("\n")
}) |> unlist()

Characters <- stringr::str_replace(Characters, "\\s*\\([^\\)]+\\)", "") # Remove parentheses
Characters <- stringr::str_replace(Characters, "[A-Z]*$", "") # Remove index bug
Characters <- setdiff(Characters, c("")) # Remove empty strings
