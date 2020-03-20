
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(dplyr)
library(spotifyr)
library(formattable)
library(shinydashboard)

r_cat <- GET('https://api.spotify.com/v1/browse/categories',
             add_headers(Authorization = paste("Bearer", get_spotify_access_token())),
             query = list(country = 'US', locale = 'en_US', limit = 50))
json_cat <- content(r_cat, as = 'text')
from_json_cat <- fromJSON(json_cat)
cats <- from_json_cat$categories$items %>% select(name, id)


shinyUI(dashboardPage(
  dashboardHeader(
    title="Spotify Analysis"
  ),
  dashboardSidebar(
    sidebarMenu(
      #first item
      menuItem("Album Artist Analysis", tabName = "aaa", icon = icon("acquisitions-incorporated")),
      menuItem("User Analysis", tabName = "ua", icon = icon("calendar")),
      menuItem("Popular Lists", tabName = "popular", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem( tabName = "aaa",
               fluidRow(
                 box(
                   title= "Artist Album Analysis Input",
                   width="3",
                   height ="360",
                   textInput(inputId="artist",
                             label="Please type an artist name:"),
                   
                   #get sorting method of preference from the user
                   selectInput(inputId= "sort",
                               label="Sort table by:",
                               choices=c("Release Year", "Energy","Valence", "Dance Level"),
                               selected= NULL),
                   
                   #get the arranging method of interest from the user
                   selectInput(inputId= "arrange",
                               label="Arrange table by:",
                               choices=c("Ascending", "Descending"),
                               selected= NULL),
                   
                   #a submit button allows the user to submit the data from above
                   submitButton("Submit")
                 ),
                 box(title= "Artist Album Analysis",
                     height = "360",
                     width = "9",
                     solidHeader = T, 
                     column
                     (width = 12,
                       formattableOutput("albumTable"),
                       style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                     )
                 )
               )
      ),
      tabItem(tabName = "ua",
              fluidRow(
                box(title= "Your Top 10 Artist!",
                    width = "12",
                    solidHeader = T, 
                    column
                    (width = 12,
                      plotOutput("Top"),
                      style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                    )
                ),
                box(title= "Your Popular Top Artist",
                    width = "12",
                    solidHeader = T, 
                    column
                    (width = 12,
                      plotOutput("PopularTop"),
                      style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                    )
                ),
                box(title= "Emotion Summary",
                    width = "12",
                    solidHeader = T, 
                    column
                    (width = 12,
                      plotOutput("Emotion"),
                      style = "height:400px; overflow-y: scroll;overflow-x: scroll;"
                    )
                )
              )
      ),
      tabItem(tabName = "popular",
              fluidRow(
                box(
                  selectInput("cat", "Category", cats$name),
                  sliderInput("num", "Number of Tracks", min = 1, max = 30, value = 10),
                  width = "3",
                  height = "360"
                ),
                box(column(width = 12,
                           tableOutput("mpts"),
                           style = "height:300px; overflow-y: scroll; overflow-x: scroll;"
                ),
                title = "Most Popular Spotify Tracks per Category",
                height = "360",
                width = "9",
                solidHeader = TRUE
                )
              )
      )
    )
  )
))
