#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
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

time_calculator <- function(a){
  
  #calculates a decimal value for the number of minutes to hours, minutes and seconds
  hours = floor(a/60)
  minutes = floor(a%%60)
  seconds = floor((a%%floor(a))*60)
  a<-case_when(
    a >= 60 ~  sprintf("%01d:%02d:%02d", hours, minutes, seconds),
    a < 20 ~ sprintf("0:%02d:%02d", minutes, seconds)
  )
  return(a)
  
}

time_calculator2 <- function(a){
  
  #calculates a decimal value for the number of minutes to minutes and seconds
  minutes= floor(a)
  seconds = floor((a%%floor(a))*60)
  a<-sprintf("%01d:%02d", minutes, seconds)
  return(a)
  
}

album_metrics<-function(data){
  
  # create a data table grouped by the album_name, 
  # then create averages for the energy, valence, dance, loudness, danceability, instrumentalness
  # for average_track_time to minutes, seconds and album_time convert to minutes
  # then calculate the mood categories 
  dataset_album_one<-data %>% 
    group_by(album_name, album_release_year, album_id) %>% 
    summarize(avg_energy=mean(energy), 
              avg_valence=mean(valence), 
              avg_dance=mean(danceability), 
              number_tracks=max(track_number),
              avg_loudness= mean(loudness), 
              avg_danceability= mean(danceability), 
              avg_instrumentalness= mean(instrumentalness),
              avg_track_time = (mean(duration_ms)/1000.0)/60.0,
              album_time = (sum(duration_ms)/(1000.0*60))) %>% 
    mutate(mood=case_when(
      (avg_valence<=.5 & avg_energy>=.5) ~ "Ambitious",
      (avg_valence>=.5 & avg_energy<=.5) ~ "Peaceful",
      (avg_valence <=.5 & avg_energy<=.5) ~ "Sad", 
      (avg_valence>=.5 & avg_energy>=.5) ~ "Happy", 
      TRUE ~ "Misc"
    ))
  
  return(dataset_album_one)
  
}

album_metrics2<-function(data){
  
  albums_final<-data %>% 
    #select the variables of interest
    select(album_name, 
           album_release_year,
           number_tracks,
           album_time,
           avg_track_time,
           mood,
           avg_energy,
           avg_valence,
           avg_danceability) %>%
    #calculate the times for both album times and average track times
    mutate(album_time = as.character(time_calculator(album_time))) %>% 
    mutate(avg_track_time = as.character(time_calculator2(avg_track_time))) %>% 
    mutate_if(is.numeric, round, digits = 2) %>% 
    mutate(mood=as.factor(mood)) %>% 
    #rename all the column names to something more readable and simplistic
    rename("Album Name"=album_name,
           "Release Year"=album_release_year,
           "Total Tracks"= number_tracks,
           "Average Track Time (min:sec)"=avg_track_time,
           "Album Duration (hr:min:sec)"= album_time,
           "Mood"=mood,
           "Valence" = avg_valence,
           "Energy" = avg_energy, 
           "Dance Level"=avg_danceability)
  
  #drop all the repeated Album Names
  albums_final<- albums_final[!duplicated(albums_final$`Album Name`), ]
  
  
  return(albums_final)
  
}

album_sort<-function(a,b,c){
  
  #now based on the input for the sorting methods inputted by the user, sort the datasets
  if(a == "Release Year" && b =="Descending"){ #if the input is Release Year, then sort by Release Year
    c <- c %>%
      arrange(desc(`Release Year`)) 
  }else if(a == "Energy" && b == "Descending"){ #if the input is Energy, then sort by Energy
    c <- c %>%
      arrange(desc(`Energy`))
  }else if(a == "Valence" && b == "Descending"){ #if the input is Valence, then sort by Valence
    c <- c %>%
      arrange(desc(`Valence`))
  }else if(a == "Dance Level" && b == "Descending"){ #if the input is Dance Level, then sort by Dance Level
    c <- c %>%
      arrange(desc(`Dance Level`))
  }else if(a == "Release Year" && b == "Ascending"){ #if the input is Energy, then sort by Energy
    c <- c %>%
      arrange(`Release Year`)
  }else if(a == "Energy" && b == "Ascending"){ #if the input is Valence, then sort by Valence
    c <- c %>%
      arrange(`Energy`)
  }else if(a == "Valence" && b == "Ascending"){ #if the input is Dance Level, then sort by Dance Level
    c <- c %>%
      arrange(`Valence`)
  }else{ #if the input is Dance Level, then sort by Dance Level
    c <- c %>%
      arrange(`Dance Level`)
  }
  
  return(c)
  
}

format_album<- function(data){
  
  library(formattable)
  
  formatted_table <- formattable(data, 
                                 align = c("l",rep("r", NCOL(data) - 1)),
                                 #change specific names to the color black
                                 list(
                                   `ID` = formatter("span",
                                                    style = ~ style(color="black"),
                                                    font.weight = "bold"),
                                   
                                   `Album Name` = formatter("span",
                                                            style = ~ style(color="black"),
                                                            font.weight = "bold"),
                                   
                                   `Album Release Year` = formatter("span",
                                                                    style= ~style(color="black")),
                                   
                                   `Valence` = formatter("span",
                                                         style = ~ style(color="black"),
                                                         font.weight = "bold"),
                                   
                                   `Energy` = formatter("span",
                                                        style = ~ style(color="black"),
                                                        font.weight = "bold"),
                                   #change valence and energy gradient tiles based on their levels
                                   `Valence` = color_tile("olivedrab1", "olivedrab4"),
                                   
                                   `Energy` = color_tile("olivedrab1", "olivedrab4"),
                                   #match the mood to a corresponding color
                                   `Mood` = formatter("span", 
                                                      style= x~ifelse(x== "Happy", style(color = "green", font.weight = "bold"),
                                                                      ifelse(x=="Ambitious", style(color = "blue", font.weight = "bold"),
                                                                             ifelse(x=="Peaceful", style(color = "purple", font.weight = "bold"),
                                                                                    style(color = "gray", font.weight = "bold"))))
                                   ),
                                   #change track name to the color black 
                                   `Total Tracks` = formatter("span",
                                                              style= ~style(color="black")),
                                   #change dance level and release year to gradient tiles based on their levels
                                   `Dance Level` = color_tile("olivedrab1", "olivedrab4"), 
                                   
                                   `Release Year` = color_tile("white", "tan")
                                 )
  )
  
  return(formatted_table)
  
}

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$albumTable <- renderFormattable({
    
    showNotification("", action = NULL, duration = 5, closeButton = TRUE,
                     id = NULL, type = c("default", "message", "warning", "error"))
    # get the access token
    access_token <- get_spotify_access_token(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), 
                                             client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"))
    
    # get the dataset for the specific input from the user
    dataset <- get_artist_audio_features(input$artist)
    
    #pass the data to the function that adds a mood and groups by album name
    album_one<-album_metrics(dataset)
    
    #pass the previous dataset to further change the table to include simpler names and time calcuations
    album_two<-album_metrics2(album_one)
    
    #this function sorts the table according to the users input
    album_final<-album_sort(input$sort, input$arrange, album_two)
    
    #format the above table into an aesthetic table
    format_album(album_final)
    
  })
  output$Top <- renderPlot({
    scopes <- c("user-library-read","streaming","user-top-read","user-read-recently-played","user-read-private")
    get_spotify_authorization_code(scope = scopes)
    top_artists <- get_my_top_artists_or_tracks(type = "artists",limit = 50)
    top_artists <- as_tibble(top_artists %>%
                               transmute(
                                 genres = genres,
                                 Rank = name,
                                 popularity = popularity,
                                 type = type,
                                 uri = uri,
                                 followers.total = followers.total
                               )
    )
    top_artists$Rank <- factor(top_artists$Rank, levels = top_artists$Rank)
    ## User's top artists ##
    ggplot(head(top_artists,n = 10), aes(x=Rank, y = followers.total,fill = Rank)) + 
      geom_bar(stat = "identity") +
      geom_text(aes(label = followers.total),angle = 45) +
      ggtitle("YOUR TOP ARTISTS") +
      labs(x = "Name")
  })
  output$Emotion <- renderPlot({
    scopes <- c("user-library-read","streaming","user-top-read","user-read-recently-played","user-read-private")
    get_spotify_authorization_code(scope = scopes)
    top_tracks <- get_my_top_artists_or_tracks(type = "tracks",limit = 50)
    top_track_info <- get_track_audio_features(top_tracks$id)
    ggplot(data = top_track_info, aes(x = energy, y = valence, color = tempo)) + 
      geom_point(size=3) +
      geom_hline(yintercept=0.5)+
      geom_vline(xintercept = 0.5)+
      scale_color_gradient(low="blue", high="red")
    
    
  })
})
