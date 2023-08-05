Cores <- detectCores() - 1
Cluster <- makeCluster(Cores, type = "FORK")
doParallel::registerDoParallel(Cluster)

df_list <- foreach(i = 1:length(CoH_Chapters)) %dopar%
  get_character_edges_from_chapter(CoH_Chapters[[i]])

saveRDS(df_list, here::here("data", "CoH_Chapter_Data.Rds"))

data <- get_links_nodes(CoH_Chapter_Data[[10]])

# Force Network
networkD3::forceNetwork(
  Links = data$Links,
  Nodes = data$Nodes,
  Source = "source_id",
  Target = "target_id",
  Value = "value",
  NodeID = "name",
  Nodesize = "size",
  Group = "group",
  fontSize = 18,
  zoom = TRUE,
  opacity = 1
)

# Chord Diagram
Adj_Matrix <- get.adjacency(graph.data.frame(data$Raw.Links), sparse=FALSE) # This method produces n by n matrices
networkD3::chordNetwork(Adj_Matrix, labels = rownames(Adj_Matrix))

# Dendrogram
Adj_Matrix2 <- table(data$Raw.Links) # This method produces m by n matrices
hc <- hclust(dist(Adj_Matrix2), method = "complete")
networkD3::dendroNetwork(hc, zoom = TRUE)

