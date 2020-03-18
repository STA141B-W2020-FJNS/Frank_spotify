#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(dplyr)
library(spotifyr)
library(formattable)
library(shinydashboard)


# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  dashboardHeader(
    title="Spotify Analysis"
  ),
  dashboardSidebar(
    sidebarMenu(
      #first item
      menuItem("Album Artist Analysis", tabName = "aaa", icon = icon("acquisitions-incorporated")),
      menuItem("User Analysis", tabName = "ua", icon = icon("calendar"))
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
                box(title= "Emotion Summary",
                    width = "12",
                    solidHeader = T, 
                    column
                    (width = 12,
                      plotOutput("Top"),
                      style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                    )
                ),
                box(title= "Artist ranking",
                    width = "12",
                    solidHeader = T, 
                    column
                    (width = 12,
                      plotOutput("Emotion"),
                      style = "height:300px; overflow-y: scroll;overflow-x: scroll;"
                    )
                )
              )
      )
    )
  )
))
