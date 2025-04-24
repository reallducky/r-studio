library(readr)
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(tidyverse)
library(shinythemes)
library(shinyWidgets)

# Read the Spotify data
Spotify <- read_csv("Popular_Spotify_Songs.csv")
Spotify_2023 <- Spotify %>% 
  filter(released_year == 2023)

Spotify_2023_plot <- Spotify_2023 %>% 
  select(in_spotify_playlists, in_apple_playlists,in_deezer_playlists, bpm , `danceability_%`, `valence_%`, key, mode)

Spotify_2023_plot2 <- Spotify_2023 %>% 
  select(in_spotify_charts, track_name)


heatmap_data <- table(Spotify_2023_plot2$in_spotify_charts, Spotify_2023_plot2$track_name)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title-panel {
        position: absolute;
        top: 10px;
        right: 10px;
        margin: 0;
        padding: 0;
      }
    "))
  ),
  h1("Spotify Analysis for 2023"),
  fluidRow(column(8, tags$a(href = "https://open.spotify.com/",
                            img(src = "spotify.jpeg", height = "100px", width = "auto")))),
  tags$p(),
  tags$br(),
  div(class = "title-panel", titlePanel("Harry Lu & Chen, Hsu")),
  theme = shinytheme("cosmo"), 
  setBackgroundColor(
    color = c("lightgrey", "lightgreen"),
    gradient = "linear",
    direction = "bottom"
  ),
  tabsetPanel(
    tabPanel("Plots",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Spotify_var", label = "Spotify 2023", choices = names(Spotify_2023_plot)),
                 helpText("Select a variable to display its distribution."),
                 # Additional text below the bottom left side
                 tags$div(
                   br(),
                   h2("About Dataset:"),
                   h3("Dataset contains a comprehensive list of the most famous songs and most streamed songs as listed on Spotify in 2023."),
                   br(),
                   h4("Variable name:"), 
                   h4("in_spotify_playlists, in_apple_playlists, in_deezer_playlists,
                      bpm, danceability_%, valence_%, key, mode"),
                      style = "margin-left: 10px; margin-bottom: 10px;"),
                   br(),
                   h5('Downloaded dataset from Kaggle',
                      style = "margin-bottom: 10px; margin-right: 10px; text-align: right;")
               ),
               mainPanel(tabsetPanel(type = "tab",
                                     h3('Information: '),
                                     p("Histogram privode for in_spotify_playlists, in_apple_playlists,in_deezer_playlists, bpm, danceability_%, and valence_%."),
                                     p("Pie Chart only privode for Key and Mode."),
                                     tabPanel("Plot1", plotOutput("plot1")),
                                     tabPanel("Plot2", plotOutput("plot2"))
               ))
             )
    ),
    tabPanel("map plot",
             sidebarLayout(
               sidebarPanel(
                 tags$div(
                   br(),
                   h2("About Dataset:"),
                   h3("To pick up two variables on a heatmap can help understand the performance of different songs on the Spotify charts. 
                      The colors on the heatmap typically represent the strength of the relationship between the two varialbes."),
                   br(),
                   h4("Variable name:"),
                   h4("track_name, in_spotify_charts"),
                      style = "margin-left: 10px; margin-bottom: 10px;"), 
                   br(),
                   h5('Downloaded dataset from Kaggle',
                     style = "margin-bottom: 10px; margin-right: 10px; text-align: right;")
               ),
               mainPanel(h3("Spotify Tracks Heatmap"),
                         plotlyOutput("heatmap"))
             )
    ),
    tabPanel("Table",
             dataTableOutput(outputId = "dynamic"),
             br(),
             br(),
             h3('Information: Downloaded dataset from Kaggle',
                style = "margin-bottom: 10px; margin-right: 10px; text-align: right;")
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(Spotify_2023_plot, aes(x = .data[[input$Spotify_var]])) +
      geom_histogram(color = "black", fill = "lightblue") +
      ggtitle("Histogram") +
      theme(legend.position = 'none') +
      labs(y = "Count")
  })
  output$plot2 <- renderPlot({
    ggplot(Spotify_2023_plot, aes(x = factor(1), fill = .data[[input$Spotify_var]], label = ..count..)) +
      geom_bar(width = 1) +
      geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5), size = 5) + 
      geom_text(aes(label = .data[[input$Spotify_var]], x = 1.5), 
                stat = "count", position = position_stack(vjust = 0.5), 
                color = "black", size = 5, fontface = "bold", show.legend = FALSE) +
      coord_polar(theta = "y") +
      ggtitle("Pie Chart") +
      theme_minimal() +
      labs(y = "Count")
  })
  
  
  output$heatmap <- renderPlotly({
    p <- plot_ly(
      z = ~heatmap_data,
      type = "heatmap"
    ) 
    
  })
  
  output$dynamic <- renderDataTable({
    Spotify_2023 %>% 
      select(track_name, `artist(s)_name`, streams, in_spotify_playlists,  in_apple_playlists,in_deezer_playlists, bpm, key, mode , `danceability_%`)
  }, options = list(pageLength = 10,
                    searchCols = list(
                      NULL, rep(TRUE, ncol(Spotify_2023) - 1))))
}

shinyApp(ui = ui, server = server)

