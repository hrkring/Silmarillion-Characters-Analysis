
library(shiny)
library(gridlayout)
library(bslib)
library(igraph)
library(networkD3)
library(dplyr)
library(wordcloud)

ui = grid_page(
  layout = c(
    "     180px   1fr    1fr   ",
    "60px header  header header",
    "1fr  sidebar plot_c plot_b",
    "1fr  sidebar plot_a plot_b"
  ),
  grid_card_text("header", "Characters in The Silmarillion", is_title = TRUE),
  grid_card(
    "sidebar",
    card_header(markdown("Settings")),
    selectInput(inputId = 'book',
                label = 'Book:',
                choices = c("The Silmarillion", "The Children of Hurin"),
                selected = 1),
    uiOutput("chapter")
  ),
  grid_card(
    "plot_a",
    card_header(markdown("By Relationship")),
    card_body(
      forceNetworkOutput("forceNetwork")
    )
  ),
  grid_card(
    "plot_b",
    card_header(markdown("By Similarity")),
    card_body(
      dendroNetworkOutput("dendrogram")
    )
  ),
  grid_card(
    "plot_c",
    card_header(markdown("By Importance")),
    card_body(
      plotOutput("wordCloud")
    )
  )
)
