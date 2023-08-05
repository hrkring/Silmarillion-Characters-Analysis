library(shiny)
library(gridlayout)
library(bslib)
library(igraph)
library(networkD3)
library(dplyr)
library(wordcloud)

Silm_Chapter_Data <- readRDS("Silmarillion_Chapter_Data.Rds")
Silm_Characters <- readRDS("Silmarillion_Characters.Rds")

CoH_Chapter_Data <- readRDS("CoH_Chapter_Data.Rds")
CoH_Characters <- readRDS("CoH_Characters.Rds")

CoH_Chapter_List <- c(
  "The Childhood of Túrin" = 1,
  "The Battle of Unnumbered Tears" = 2,
  "The Words of Húrin and Morgoth" = 3,
  "The Departure of Túrin" = 4,
  "Túrin in Doriath" = 5,
  "Túrin among the Outlaws" = 6,
  "Of Mîm the Dwarf" = 7,
  "The Land of Bow and Helm" = 8,
  "The Death of Beleg" = 9,
  "Túrin in Nargothrond" = 10,
  "The Fall of Nargothrond" = 11,
  "The Return of Túrin to Dor-lómin" = 12,
  "The Coming of Túrin into Brethil" = 13,
  "The Journey of Morwen and Niënor to Nargothrond" = 14,
  "Niënor in Brethil" = 15,
  "The Coming of Glaurung" = 16,
  "The Death of Glaurung" = 17,
  "The Death of Túrin" = 18
)

Silm_Chapter_List <- c(
  "Ainulindalë" = 1,
  "Valaquenta" = 2,
  "Of the Beginning of Days" = 3,
  "Of Aulë and Yavanna" = 4,
  "Of the Coming of the Elves and the Captivity of Melkor" = 5,
  "Of Thingol and Melian" = 6,
  "Of Eldamar and the Princes of the Eldalië" = 7,
  "Of Fëanor and the Unchaining of Melkor" = 8,
  "Of the Silmarils and the Unrest of the Noldor" = 9,
  "Of the Darkening of Valinor" = 10,
  "Of the Flight of the Noldor" = 11,
  "Of the Sindar" = 12,
  "Of the Sun and Moon and the Hiding of Valinor" = 13,
  "Of Men" = 14,
  "Of the Return of the Noldor" = 15,
  "Of Beleriand and its Realms" = 16,
  "Of the Noldor in Beleriand" = 17,
  "Of Maeglin" = 18,
  "Of the Coming of Men into the West" = 19,
  "Of the Ruin of Beleriand and the Fall of Fingolfin" = 20,
  "Of Beren and Lúthien" = 21,
  "Of the Fifth Battle: Nirnaeth Arnoediad" = 22,
  "Of Túrin Turambar" = 23,
  "Of the Ruin of Doriath" = 24,
  "Of Tuor and the Fall of Gondolin" = 25,
  "Of the Voyage of Eärendil and the War of Wrath" = 26,
  "Akallabêth" = 27,
  "Of the Rings of Power and the Third Age" = 28
)

function(input, output, session) {
    output$chapter <- renderUI({
      switch(input$book,
             "The Silmarillion" = selectInput('chapter',
                         label = 'Chapter:',
                         choices = Silm_Chapter_List),
             "The Children of Hurin" = selectInput('chapter',
                         label = 'Chapter:',
                         choices = CoH_Chapter_List
                         ))
    })

  observe({
    Chapter_Data <- switch(
      input$book,
      "The Silmarillion" = Silm_Chapter_Data,
      "The Children of Hurin" = CoH_Chapter_Data
    )

    Characters <- switch(
      input$book,
      "The Silmarillion" = Silm_Characters,
      "The Children of Hurin" = CoH_Characters
    )

    Links_Data <- Chapter_Data[[ifelse(!is.null(input$chapter), as.numeric(input$chapter), 1)]] |>
        left_join(Characters, by = c("source" = "Alt.Character.Name.Plain")) |>
        mutate(source = Character.Name) |>
        select(source, target) |>
        left_join(CoH_Characters, by = c("target" = "Alt.Character.Name.Plain")) |>
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
        summarise(size = sum(2 * value), .by = source) |>
        rename(name = source) |>
        inner_join(groups, by = c("name"))

      # Generate IDs
      links$source_id <- match(links$source, nodes$name) - 1
      links$target_id <- match(links$target, nodes$name) - 1

      output$forceNetwork <- renderForceNetwork({
        forceNetwork(
          Links = links,
          Nodes = nodes,
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
      })

      # Adj_Matrix <- get.adjacency(graph.data.frame(Links_Data), sparse = FALSE) # This method produces n by n matrices
      #
      # output$chordDiagram <- renderchordNetwork({
      #   networkD3::chordNetwork(Adj_Matrix, labels = rownames(Adj_Matrix), height = 250, width = 250)
      # })

      Adj_Matrix2 <- table(Links_Data) # This method produces m by n matrices

      output$dendrogram <- renderDendroNetwork({
        hc <- hclust(dist(Adj_Matrix2), method = "complete")
        networkD3::dendroNetwork(hc, zoom = TRUE)
      })

      output$wordCloud <- renderPlot({
          wordcloud(
            words = nodes$name,
            freq = nodes$size,
            min.freq = 1,
            max.words = 200,
            random.order = FALSE,
            rot.per = 0.35,
            colors = RColorBrewer::brewer.pal(8, "Dark2"), scale = c(20, 5)
          )
        },
        res = 10
      )
  })
}
