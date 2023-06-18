## Advanced Data Visualization (QSS 19) Spring 2023
## R Review VI: Plotly and Shiny
##
## Name: Ava Scharfstein
## Date: April 28th - May 4th, 2023

library(tidyverse)
library(plotly)
library(lubridate)
library(shiny)


# Part 2: Plotly -----------------------------------------------

billboard <- read_csv(paste0(getwd(),"/billboard_dat/hot_stuff.csv"))
spotify <- read_csv(paste0(getwd(),"/billboard_dat/hot100_audio_features.csv"))

# Inner join spotify and billboard data sets
music <- spotify %>% 
  inner_join(billboard, c("SongID","Performer","Song")) %>% 
  # create a date variable
  separate(WeekID, c("month", "day", "year")) %>% 
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% 
  group_by(SongID, Performer, Song, year = year(date)) %>% 
  # for each song get the mean # of weeks on chart per year
  # doing this to reduce the number of duplicate points
  slice_max(`Weeks on Chart`) %>% 
  ungroup() %>% 
  group_by(SongID, Song, Performer, year) %>%
  distinct() %>%
  ungroup()

# count the number of occurrences of each word in spotify_genre 
music_genre_count <- music %>% 
  group_by(SongID, Song) %>% 
  summarise(country = str_count(spotify_genre, 'country'),
         pop = str_count(spotify_genre, 'pop'),
         rap = str_count(spotify_genre, 'rap'),
         rock = str_count(spotify_genre, 'rock')) %>% 
  ungroup()

# Selects genre with max count
genres <- music_genre_count %>% 
  select(-SongID, -Song) %>% 
  # the main issue with this way of getting the max value
  # is that if there is no max (eg. all zeroes), it randomly (??)
  # chooses a max
  summarize(genre = names(.)[max.col(.)]) %>% 
  pull()

# Add genre variable to music, set genre to NA if not classified
# as any of the main genres
music_genre_simplified <- music_genre_count %>% 
  mutate(genre = genres,
         genre = ifelse(country == 0 & pop == 0 & rap == 0 & rock == 0,
                        NA,
                        genre)) %>% 
  pull(genre)


music <- music %>% 
  mutate(genre = music_genre_simplified) %>% 
  drop_na() # excluding songs that are not classified by these genres

rc_pal <- c("country" = "#cc0000", 
            "pop" = "#f1c232",
            "rap" = "#1d6a3a",
            "rock" = "#0b5394")

# wrapped in ggplotly -> not as good of a plot

# p <- music %>%
#   group_by(SongID, Song, Performer, year) %>%
#   distinct() %>%
#   ungroup() %>%
#   group_by(year, genre) %>%
#   mutate(avg_woc = mean(`Weeks on Chart`)) %>% 
#   group_by(SongID, Song, Performer, year) %>%
#   distinct() %>%
#   ggplot(aes(x = year,
#              y = `Weeks on Chart`,
#              color = genre,
#              group = genre)) +
#   geom_jitter(alpha = .1) +
#   geom_line(aes(y = avg_woc)) +
#   scale_x_continuous(breaks = seq(1958, 2019, by = 10)) +
#   scale_color_manual(values = rc_pal)+
#   theme_minimal() +
#   labs(x = "Year", color = "genre")
# 
# ggplotly(p) 


# summarise by month??


music_avg <- music %>%
  # get the average WOC for each genre each year
  group_by(year, genre) %>%
  summarise(avg_woc = mean(`Weeks on Chart`)) %>% 
  ungroup()
music %>% # fix colors
  plot_ly(x = ~jitter(year),
          y = ~`Weeks on Chart`,
          color = ~genre,
          colors = rc_pal,
          type = "scatter",
          mode = "markers",
          hovertemplate = paste( 
            "<b>%{text}</b><br>",
            "Weeks on Chart in %{x:.0f}: %{y}<br>",
            "<extra></extra>"
          ),
          text = ~paste0(Song, " ", "by ",Performer),
          opacity = .1,
          showlegend = FALSE) %>%
  add_trace(data = music_avg, 
            type = "scatter",
            mode = "lines+markers",
            inherit = FALSE,
            x = ~year, 
            y = ~avg_woc,
            color = ~genre,
            colors = rc_pal,
            opacity = 1,
            showlegend = TRUE) %>% 
  layout(legend = list(title=list(text='Genre')),
         title = list(text = "Number of Weeks on Chart per Year By Top Genres",
                      y = 0.98,
                      x = .06),
         yaxis = list(title = 'Number of Weeks on Chart'),
         xaxis = list(title = 'Year'),
         margin = list(l = 100, r = 100, b = 100, t = 50, pad = 10))



# Part 3: Shiny -----------------------------------------------------------
  
ui <- fluidPage(
  selectizeInput(
    inputId = "genre", 
    label = "Select a genre", 
    choices = unique(music$genre), 
    selected = "rock",
    multiple = TRUE
  ),
  plotlyOutput(outputId = "p")
)

server <- function(input, output, ...) {
  output$p <- renderPlotly({
    music %>% 
      group_by(genre, year) %>% 
      summarise(avg_valence = mean(valence, na.rm = TRUE),
                avg_woc = mean(`Weeks on Chart`, na.rm = TRUE)) %>% 
      filter(genre %in% input$genre) %>%
      plot_ly(type = "scatter",
              mode = "markers",
              x = ~year,
              y = ~avg_valence,
              color = ~genre,
              colors = rc_pal,
              hovertemplate = paste(
                "Year: %{x:.0f}<br>",
                "Avgerage Valence: %{y:.2f}",
                "<extra></extra>"),
              size = ~avg_woc,
              opacity = .5) %>% 
      layout(legend = list(title=list(text='Genre')),
             title = list(text = "Average Valence By Top Genres",
                          y = 0.98,
                          x = .06),
             yaxis = list(title = 'Average Valence'),
             xaxis = list(title = 'Year'),
             margin = list(l = 100, r = 100, b = 100, t = 50, pad = 10))
  })
}

# click open brower to see the shiny plot and interact
shinyApp(ui, server)
  

