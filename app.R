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



access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))
usethis::edit_r_environ("project")
###################################
# Authentication
###################################
scopes <- c("user-library-read","user-library-modify","playlist-read-private","playlist-modify-public")
scopes <- c("streaming","user-top-read","user-read-recently-played")
get_spotify_authorization_code <- function (client_id = Sys.getenv("SPOTIFY_CLIENT_ID"), client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"), 
                                            scope = scopes)
{
  endpoint <- oauth_endpoint(authorize = "https://accounts.spotify.com/authorize", 
                             access = "https://accounts.spotify.com/api/token")
  app <- oauth_app("spotifyr", client_id, client_secret)
  oauth2.0_token(endpoint = endpoint, app = app, scope = scope)
}

get_my_recently_played <- function (limit = 20, after = NULL, before = NULL, authorization = get_spotify_authorization_code(), 
          include_meta_info = FALSE) 
{
  stopifnot(is.null(after) | is.null(before))
  base_url <- "https://api.spotify.com/v1/me/player/recently-played"
  params <- list(limit = limit, after = after, before = before)
  res <- RETRY("GET", base_url, config(token = authorization), 
               query = params, encode = "json")
  stop_for_status(res)
  res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                  flatten = TRUE)
  if (!include_meta_info) {
    res <- res$items
  }
  return(res)
}

get_my_profile <- function(authorization = get_spotify_authorization_code()) {
  base_url <- 'https://api.spotify.com/v1/me/'
  res <- RETRY('GET', base_url, config(token = authorization), encode = 'json')
  stop_for_status(res)
  res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE) %>%
    unlist() %>%
    t() %>%
    as_tibble()
  
  return(res)
}


get_my_top_artists_or_tracks <- function(type = NULL, limit = 20, offset = 0, time_range = 'medium_term', authorization = get_spotify_authorization_code(), include_meta_info = FALSE) {
  
  if (!type %in% c('artists', 'tracks')) {
    stop('"type" must be one of "artists" or "tracks"')
  }
  
  if ((limit < 1 | limit > 50) | !is.numeric(limit)) {
    stop('"limit" must be an integer between 1 and 50')
  }
  
  if ((offset < 0 | offset > 10000) | !is.numeric(offset)) {
    stop('"offset" must be an integer between 1 and 10,000')
  }
  
  if (!time_range %in% c('short_term', 'medium_term', 'long_term')) {
    stop('"type" must be one of "short_term", "medium_term", or "long_term"')
  }
  
  base_url <- 'https://api.spotify.com/v1/me/top'
  params <- list(
    limit = limit,
    offset = offset,
    time_range = time_range
  )
  url <- str_glue('{base_url}/{type}')
  res <- RETRY('GET', url, config(token = authorization), query = params, encode = 'json')
  stop_for_status(res)
  
  res <- fromJSON(content(res, as = 'text', encoding = 'UTF-8'), flatten = TRUE)
  
  if (!include_meta_info) {
    res <- res$items
  }
  return(res)
}

get_my_top_artists_or_tracks(type = "artists",limit = 5)

###################################

get_my_recently_playey
get_my_recently_played(limit = 5) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at) %>% 
  kable()

get_my_top_artists_or_tracks(type = 'artists', time_range = 'long_term', limit = 5) %>% 
  select(name, genres) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup %>% 
  kable()

get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 5) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable()

beatles <- get_artist_audio_features('the beatles')

r <- GET("https://accounts.spotify.com/authorize",
         query = list(
           client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
           response_type = "code",
           redirect_uri = "http://localhost:1410/",
           scope = "user-read-private user-read-email"
         ))
code <- POST(
  "https://accounts.spotify.com/api/token",
  body - list(
    grant_type = "authorization_code",
    code = "AQBWTE58sm66GrRmQk7mSChRLKgKhKc6TiOPNfyyI0qHW765Tn-YH7Yu2Tn-hzXDgIonGrwP_xom7FKguOyej7uzs-HvWdbab13L1FoqWA7--L1gL7Lkcwj-4wy4GGnWtIFA1fAmsA-qXn7Bw_M5VOyYWMzmG2n2X3T-Hc3zZMNeLCvUe-dCzKZ_MObpyoh2ZbMJB8Z40Le8onpgmt_X",
    redirect_uri = "http://localhost:1410/"
  )
)
json <- content(r, as = "html")
fromJSON(json)
code <- 


p <- POST("https://accounts.spotify.com/api/token",
          query = list(
            
          ))

r <- GET("https://api.spotify.com/v1/me/top/",
         query = list(
           type = "artists"
         ))



TS <- get_artist_audio_features('Taylor Swift')


fromJSON("https://api.spotify.com/v1/me/top/artist")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Spotify Franklin"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

