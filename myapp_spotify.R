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


usethis::edit_r_environ("project")
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

###################################
# Authentication
###################################
scopes <- c("user-library-read","user-library-modify","playlist-read-private","playlist-modify-public")
scopes <- c("user-library-read","streaming","user-top-read","user-read-recently-played","user-read-private")
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


  ## User's top artists rank by follower ##
top_artists %>% 
  arrange(desc(followers.total)) %>%
  head(n= 10) %>%
  ggplot( aes(x= reorder(name,-followers.total), y = followers.total,fill = followers.total)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = name),angle = 45) + 
  ggtitle("YOUR TOP ARTISTS RANK BY FOLLOWERS")


top_tracks <- get_my_top_artists_or_tracks(type = "tracks",limit = 50)
top_tracks_10 <- get_my_top_artists_or_tracks(type = "tracks",limit = 10)

get_track_audio_features <- function (ids, authorization = get_spotify_access_token()) 
{
  stopifnot(length(ids) <= 100)
  base_url <- "https://api.spotify.com/v1/audio-features"
  params <- list(access_token = authorization, ids = paste0(ids, 
                                                            collapse = ","))
  res <- RETRY("GET", base_url, query = params, encode = "json")
  stop_for_status(res)
  res <- fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                  flatten = TRUE) %>% .$audio_features %>% as_tibble()
  return(res)
}

top_track_info <- get_track_audio_features(top_tracks$id)

ggplot(data = top_track_info, aes(x = energy, y = valence, color = loudness)) + 
  geom_point(size=3)


top_tracks_10 <- as.tibble(top_tracks_10)
top_tracks_10 %>%
  select(id) %>% get_track_audio_features()
get_artist <- function()
get_artist_audio_features <- function (artist = NULL, include_groups = "album", return_closest_artist = TRUE, 
          dedupe_albums = TRUE, authorization = get_spotify_access_token())
{
  if (is_uri(artist)) {
    artist_info <- get_artist(artist, authorization = authorization)
    artist_id <- artist_info$id
    artist_name <- artist_info$name
  }
  else {
    artist_ids <- search_spotify(artist, "artist", authorization = authorization)
    if (return_closest_artist) {
      artist_id <- artist_ids$id[1]
      artist_name <- artist_ids$name[1]
    }
    else {
      choices <- map_chr(1:length(artist_ids$name), function(x) {
        str_glue("[{x}] {artist_ids$name[x]}")
      }) %>% paste0(collapse = "\n\t")
      cat(str_glue("We found the following artists on Spotify matching \"{artist}\":\n\n\t{choices}\n\nPlease type the number corresponding to the artist you're interested in."), 
          sep = "")
      selection <- as.numeric(readline())
      artist_id <- artist_ids$id[selection]
      artist_name <- artist_ids$name[selection]
    }
  }
  artist_albums <- get_artist_albums(artist_id, include_groups = include_groups, 
                                     include_meta_info = TRUE, authorization = authorization)
  num_loops_artist_albums <- ceiling(artist_albums$total/20)
  if (num_loops_artist_albums > 1) {
    artist_albums <- map_df(1:num_loops_artist_albums, function(this_loop) {
      get_artist_albums(artist_id, include_groups = include_groups, 
                        offset = (this_loop - 1) * 20, authorization = authorization)
    })
  }
  else {
    artist_albums <- artist_albums$items
  }
  artist_albums <- artist_albums %>% rename(album_id = id, 
                                            album_name = name) %>% mutate(album_release_year = case_when(release_date_precision == 
                                                                                                           "year" ~ suppressWarnings(as.numeric(release_date)), 
                                                                                                         release_date_precision == "day" ~ year(as.Date(release_date, 
                                                                                                                                                        "%Y-%m-%d", origin = "1970-01-01")), TRUE ~ as.numeric(NA)))
  if (dedupe_albums) {
    artist_albums <- dedupe_album_names(artist_albums)
  }
  album_tracks <- map_df(artist_albums$album_id, function(this_album_id) {
    album_tracks <- get_album_tracks(this_album_id, include_meta_info = TRUE, 
                                     authorization = authorization)
    num_loops_album_tracks <- ceiling(album_tracks$total/20)
    if (num_loops_album_tracks > 1) {
      album_tracks <- map_df(1:num_loops_album_tracks, 
                             function(this_loop) {
                               get_album_tracks(this_album_id, offset = (this_loop - 
                                                                           1) * 20, authorization = authorization)
                             })
    }
    else {
      album_tracks <- album_tracks$items
    }
    album_tracks <- album_tracks %>% mutate(album_id = this_album_id, 
                                            album_name = artist_albums$album_name[artist_albums$album_id == 
                                                                                    this_album_id]) %>% rename(track_name = name, 
                                                                                                               track_uri = uri, track_preview_url = preview_url, 
                                                                                                               track_href = href, track_id = id)
  })
  dupe_columns <- c("duration_ms", "type", "uri", "track_href")
  num_loops_tracks <- ceiling(nrow(album_tracks)/100)
  track_audio_features <- map_df(1:num_loops_tracks, function(this_loop) {
    track_ids <- album_tracks %>% slice(((this_loop * 100) - 
                                           99):(this_loop * 100)) %>% pull(track_id)
    get_track_audio_features(track_ids, authorization = authorization)
  }) %>% select(-dupe_columns) %>% rename(track_id = id) %>% 
    left_join(album_tracks, by = "track_id")
  artist_albums %>% mutate(artist_name = artist_name, artist_id = artist_id) %>% 
    select(artist_name, artist_id, album_id, album_type, 
           album_images = images, album_release_date = release_date, 
           album_release_year, album_release_date_precision = release_date_precision) %>% 
    left_join(track_audio_features, by = "album_id") %>% 
    mutate(key_name = pitch_class_lookup[key + 1], mode_name = case_when(mode == 
                                                                           1 ~ "major", mode == 0 ~ "minor", TRUE ~ as.character(NA)), 
           key_mode = paste(key_name, mode_name))
}


get_artist_audio_features(artist = )
###################################
