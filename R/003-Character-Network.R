
df <- data.frame(Sentence = Silm_Text_Sentences |> pull()) |>
  mutate(Sentence = stringr::str_to_lower(Sentence)) |>
  tidyr::separate(col = 1, into = c(1:160) |> as.character(), sep = " ") |>
  mutate(across(everything(), ~ case_when(
    . %in% Silmarillion_Characters$Alt.Character.Name.Plain ~ .,
    .default = NA
  ))) |>
  select(where(function(x) !all(is.na(x)))) |>
  filter(if_any(everything(), ~ !is.na(.)))

Characters.By.Sentence <- apply(df[-1], 1, function(x) toString(na.omit(unique(x))))

x <- lapply(1:length(Characters.By.Sentence), function(x) strsplit(Characters.By.Sentence[x], ", "))

df <- data.frame()
for (i in 1:length(x)) {
  if (length(x[[i]][[1]]) > 1) {
    y <- combn(x[[i]][[1]], m = 2) |>
      t() |>
      as.data.frame()
    df <- bind_rows(df, y)
  }
}

links <- df |>
  left_join(Silmarillion_Characters, by = c("V1" = "Alt.Character.Name.Plain")) |> # Restylize character names for source
  mutate(V1 = Character.Name) |>
  select(V1, V2) |>
  left_join(Silmarillion_Characters, by = c("V2" = "Alt.Character.Name.Plain")) |> # Restylize character names for target
  mutate(V2 = Character.Name) |>
  select(V1, V2) |>
  filter(V1 != V2) |> # Remove cycles
  rename(source = V1, target = V2) |>
  count(source, target, name = "value") |>
  arrange(desc(value))

# Find Groups
wc <- cluster_walktrap(igraph::graph_from_edgelist(df |> select(1:2) |> as.matrix()))
groups <- membership(wc) |> as.data.frame()
names(groups) <- "group"
groups$name <- rownames(groups)

Source_Size <- links |>
  summarise(size = sum(value), .by = "source") |>
  rename(name = source)

Target_Size <- links |>
  summarise(size = sum(value), .by = "target") |>
  rename(name = target)

nodes <- Source_Size |>
  bind_rows(Target_Size) |>
  summarise(size = sum(size), .by = name) |>
  inner_join(groups, by = c("name"))

# Generate IDs
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

# Force Network
networkD3::forceNetwork(
  Links = links,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  Group = "group",
  fontSize = 18,
  zoom = TRUE,
  Nodesize = "size",
  opacity = 1
)

# Chord Diagram
Adj_Matrix <- get.adjacency(graph.data.frame(df |> select(V1, V2)), sparse=FALSE) # This method produces n by n matrices
networkD3::chordNetwork(
  Adj_Matrix,
  labels = rownames(Adj_Matrix),
)

# Dendrogram
Adj_Matrix2 <- table(df |> select(V1, V2)) # This method produces m by n matrices
hc <- hclust(dist(Adj_Matrix2), method = "complete")
networkD3::dendroNetwork(
  hc,
  height = 1250,
  zoom = TRUE,
  fontSize = 14,
  nodeStroke = "midnightblue"
)
