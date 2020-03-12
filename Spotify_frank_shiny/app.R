#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(spotifyr)
library(lubridate)
library(tidyverse)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Personalify"),
   
   # Sidebar with a slider input for number of bins 
   actionButton("gen_button", "Generate my analysis"),
   sidebarLayout(
      # Show  plot of the generated distribution
      mainPanel(
         plotOutput("Top"),
         plotOutput("Emotion")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   observeEvent(input$gen_button, {
     output$Top <- renderPlot({
       # generate bins based on input$bins from ui.R
       x    <- faithful[, 2] 
       bins <- seq(min(x), max(x), length.out = input$bins + 1)
       
       # draw the histogram with the specified number of bins
       hist(x, breaks = bins, col = 'darkgray', border = 'white')
       
       
       scopes <- c("user-library-read","streaming","user-top-read","user-read-recently-played","user-read-private")
       get_spotify_authorization_code(scope = scopes)
       top_artists <- get_my_top_artists_or_tracks(type = "artists",limit = 50)
       top_artists <- as_tibble(top_artists %>%
                                  transmute(
                                    genres = genres,
                                    name = name,
                                    popularity = popularity,
                                    type = type,
                                    uri = uri,
                                    followers.total = followers.total
                                  )
       )
       ## User's top artists ##
       ggplot(head(top_artists,n = 10), aes(x=name, y = followers.total,fill = name)) + 
         geom_bar(stat = "identity") +
         geom_text(aes(label = followers.total),angle = 45) +
         ggtitle("YOUR TOP ARTISTS")
       
       
     })
     output$Emotion <- renderPlot({
       scopes <- c("user-library-read","streaming","user-top-read","user-read-recently-played","user-read-private")
       get_spotify_authorization_code(scope = scopes)
       top_tracks <- get_my_top_artists_or_tracks(type = "tracks",limit = 50)
       top_track_info <- get_track_audio_features(top_tracks$id)
       ggplot(data = top_track_info, aes(x = energy, y = valence, color = loudness)) + 
         geom_point(size=3)
     })
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

