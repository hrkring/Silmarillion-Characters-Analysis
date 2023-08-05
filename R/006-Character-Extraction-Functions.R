
# Get all indentifiable characters from the sentence
# Input: a single sentence string
# Output: a list of characters
get_characters_from_sentence <- function(sentence) {
  suppressWarnings(
    Clean_Text <- sentence |>
      VectorSource() |>
      Corpus() |>
      tm_map(removeNumbers) |>
      tm_map(removePunctuation) |>
      tm_map(stripWhitespace) |>
      tm_map(content_transformer(tolower))
  )

  Characters <- Clean_Text[[1]][["content"]] |>
    stringr::str_split_1(" ") |>
    intersect(Silmarillion_Characters$Character.Name.Plain)

  return(Characters)
}

# Get every pairwise combination of characters in the list
# Input: vector of characters
# Output: edge list of characters
get_character_combinations <- function(characters) {
  if(length(characters) < 2) {
    return()
  }

  Combinations_Half <- combn(characters, m = 2) |>
    t() |>
    as.data.frame()

  Combinations <- Combinations_Half |>
    rename(V1 = V2, V2 = V1) |>
    bind_rows(Combinations_Half) |>
    rename(source = V2, target = V1)

  return(Combinations)
}

# Get a list of edges for characters throughout the chapter
# Input: dataframe of sentences in a chapter
# Output: list of edges in the chapter
get_character_edges_from_chapter <- function(chapter) {
  Chapter_Edges <- data.frame()
  for (i in 1:nrow(chapter)) {
    Characters <- get_characters_from_sentence(chapter[i, 1])
    Sentence_Edges <- get_character_combinations(Characters)

    Chapter_Edges <- Chapter_Edges |>
      bind_rows(Sentence_Edges)
  }

  return(Chapter_Edges)
}

# Get a list of links and nodes with additional info binded
# Input: edges list for a chapter
# Output: list of links and nodes
get_links_nodes <- function(chapter_edges) {
  # Rename characters for source
  Links_Data <- chapter_edges |>
    dplyr::left_join(Silmarillion_Characters, by = c("source" = "Alt.Character.Name.Plain")) |>
    mutate(source = Character.Name) |>
    select(source, target)

  # Rename characters for target
  Links_Data <- Links_Data |>
    left_join(Silmarillion_Characters, by = c("target" = "Alt.Character.Name.Plain")) |>
    mutate(target = Character.Name) |>
    select(source, target)

  # Create source and target ids and frequencies
  links <- Links_Data |>
    count(source, target, name = "value") |>
    arrange(desc(value))

  # Find Groups
  wc <- cluster_walktrap(graph_from_edgelist(Links_Data |> select(1:2) |> as.matrix()))
  groups <- data.frame(group = membership(wc)) |>
    mutate(group = as.numeric(group))
  groups$name <- rownames(groups)

  # Character name and grouping
  nodes <- links |>
    summarise(size = sum(2*value), .by = source) |>
    rename(name = source) |>
    inner_join(groups, by = c("name"))

  # Generate IDs
  links$source_id <- match(links$source, nodes$name) - 1
  links$target_id <- match(links$target, nodes$name) - 1

  chapter_info <- list(Raw.Links = Links_Data, Links = links, Nodes = nodes)

  return(chapter_info)
}
