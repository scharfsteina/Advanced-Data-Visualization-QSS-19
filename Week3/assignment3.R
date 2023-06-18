## Advanced Data Visualization (QSS 19) Spring 2023
## R Review III: Web Scraping, Maps, Animations, and More
##
## Name: Ava Scharfstein
## Date: April 11th - April 17th, 2023

# Install Packages 
library(tidyverse)
library(rvest)
library(lubridate)
#devtools::install_github("clauswilke/ggtextures")
library(ggtextures)


# Part 2: Web Scraping ----------------------------------------------------

# 1 - Presidential Proclamations
pres_url <- "https://www.presidency.ucsb.edu/documents/app-categories/presidential"

# To be honest, this takes forever to run but if you run this for 
# like 1:20 you can see that it does the job
(urls <- stringr::str_c(pres_url,
                        "?page=", 1:8035)) 

pages <- map(urls,
             ~ read_html(.) %>%
               html_elements("h4 span") %>% 
               html_text())

unlist(flatten(pages)) %>% 
  as_tibble() %>% 
  separate(value, c("month", "day", "year")) %>% 
  count(year)

# 2 - Movies

movie_url1 <- "https://editorial.rottentomatoes.com/guide/100-best-classic-movies/1/"
movie_url2 <- "https://editorial.rottentomatoes.com/guide/100-best-classic-movies/2/"

films <- read_html(movie_url1) %>% 
  html_elements(css = ".countdown-item")

ranks <- films %>% 
  html_elements(css = ".countdown-index") %>% 
  html_text() %>% 
  str_remove(., "#") %>% 
  as.numeric()

titles <- films %>% 
  html_elements(css = ".article_movie_title a") %>% 
  html_text()

years <- films %>% 
  html_elements(css = ".start-year") %>% 
  html_text() %>% 
  gsub("[()]", "", .) %>% 
  as.numeric()

consensus <- films %>% 
  html_elements(css = ".critics-consensus") %>% 
  html_text() %>% 
  str_remove("Critics Consensus: ")

posters <- films %>% 
  html_elements(css = ".article_poster") %>% 
  html_attr(name = "src")

(movies <- data.frame(titles,years,ranks,consensus,posters))

top10 <- movies %>%
  filter(years >= 1940,
         years < 1950) %>% 
  arrange(ranks) %>% 
  head(n=10) %>% 
  mutate(left = ifelse(row_number()<=5, 0, .5)) %>%
  arrange(-ranks) %>% # sort by descending so that the figure reads top to bottom
  # use order for location on graph
  mutate(order = ifelse(left==0, row_number()-5, row_number()), 
         # put \n every 40 characters to create a paragraph
         consensus = gsub("(.{40,}?)\\s", "\\1\n", consensus)) 

top10 %>% 
  ggplot(aes(x = left, y = factor(order), image = posters)) +
  ggimage::geom_image(size = 0.2, by="height", aes(x = left+.05)) +
  geom_text(aes(label = paste0("# ",ranks, ": ", titles, ", ", years), x = left+.3), 
            size = 3, vjust = -6, fontface = "bold") +
  geom_text(aes(label = paste0('\"', consensus, '\"'), x = left+.3), size = 3, hjust = .5) +
  xlim(c(0,1)) +
  theme_void() +
  labs(title = "   Top 10 Classic Films of the 1940s",
       caption = "Ava Scharfstein  
       Source: https://editorial.rottentomatoes.com/guide/100-best-classic-movies  \n")


# Part 4: Mapping ---------------------------------------------------------

# 1 - Lakes, Rivers, and Roads

library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(grid)

north_america <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(name == "United States" | name == "Canada") %>%
  
lakes <- st_read("Week3/data/ne_10m_lakes_north_america/ne_10m_lakes_north_america.shp");
rivers <- st_read("Week3/data/ne_10m_rivers_north_america/ne_10m_rivers_north_america.shp");
roads <- st_read("Week3/data/ne_10m_roads_north_america/ne_10m_roads_north_america.shp");

# To be honest I didn't check if these would be color blind appropriate
# (Except for trying to make sure the hue and values were pretty different)
# Every time I ran this figure it took 10 minutes to run... :(
# I uploaded the output of this figure 
lake_color <- "#2a7dc8"
river_color <- "#45818e"
road_color <- "#563400"
ocean_color <- "#cfe2f3"
        
ggplot() +
  geom_sf(data = north_america)+
  geom_sf(data = lakes, fill = lake_color, color = lake_color) +
  geom_sf(data = rivers, color = river_color, fill = river_color) +
  geom_sf(data = roads, color = road_color, alpha = .2) +
  coord_sf(xlim = c(-83.604371, -61.09593), ylim = c(39.434446, 48.304894), expand = FALSE) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = ocean_color, color = ocean_color),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank()) +
  labs(title = "Lakes, Rivers, and Roads in New England",
       x = "", y = "", caption = "Ava Scharfstein  \nSource: RNatural Earth  ")

# Adding Legend

grid.draw(circleGrob(x = 0.8, 
                     y = 0.4, 
                     r = 0.009, 
                     gp = gpar(fill = lake_color, color = ocean_color)))
grid.text("Lakes", x = .86, y = .4, 
          gp = gpar(color = "black", 
                    fontsize = 8, 
                    align = "left"), 
          just = "right")

grid.draw(circleGrob(x = 0.8, 
                     y = 0.36, 
                     r = 0.009,
                     gp = gpar(fill = river_color, color = ocean_color)))
grid.text("Rivers", x = .86, y = .36, 
          gp = gpar(color = "black", 
                    fontsize = 8, 
                    align = "left"), 
          just = "right")

grid.draw(circleGrob(x = 0.8, 
                     y = 0.32, 
                     r = 0.009,
                     gp = gpar(fill = road_color, color = ocean_color)))
grid.text("Roads", x = .86, y = .32, 
          gp = gpar(color = "black", 
                    fontsize = 8, 
                    align = "left"), 
          just = "right")

# 2 - Eurostat

library(eurostat)
library(countrycode)
library(stats)
tmp <- get_eurostat_toc()
tmp %>% 
  mutate(title = tolower(title)) %>% 
  filter(str_detect(title, "poverty|inequality")) %>%
  View()

poverty <- get_eurostat("ilc_peps01") # at-risk-of poverty rate

poverty_avg <- poverty %>%
  select(geo, values) %>% 
  group_by(geo) %>% 
  mutate(values = mean(values)) %>%  # get average by country
  ungroup() 

# Merge poverty data frame with map data frame
poverty_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  filter(continent == "Europe") %>%
  # left join would display all of Europe but that takes forever to run
  # If I were to switch to left join, I would put na.value in scale_fill_gradient
  # below to grey or something
  inner_join(poverty_avg, by = c("iso_a2" = "geo")) 

# From assignment 2
(p3 <- c("#b9dae2", "#317F8C", "#061e53", "#195200", "#66b251", "#712a4d"))

poverty_map %>% 
  ggplot() +
  geom_sf() +
  theme_void() +
  scale_fill_gradient(low = p3[3], high = p3[1])+
  labs(fill = "Risk of Poverty  ",
       title = "Average Risk of Poverty in Europe from 2003-2020",
       caption = "Ava Scharfstein  \nSource: Eurostat  \n")

quantiles <- quantile(poverty_map %>% pull(values))

# Using this histogram with quantiles to help visualize the distribution
# of the data
poverty_map %>% 
  select(name, values) %>% 
  ggplot(aes(values)) + 
  geom_histogram() +
  geom_vline(aes(xintercept = quantiles[1]), color = "red") +
  geom_vline(aes(xintercept = quantiles[2]), color = "red") +
  geom_vline(aes(xintercept = quantiles[3]), color = "red") + # mean
  geom_vline(aes(xintercept = quantiles[4]), color = "red") +
  geom_vline(aes(xintercept = quantiles[5]), color = "red") +
  theme_minimal() +
  labs(x = "Average Risk for Poverty", y = "Number of Countries")

# It seems like the majority of the data is concentrated below the fourth quantile (75%).
# I think it makes sense to group the data by [Q0-Q2],[Q2-Q4] and [Q4-Q5].
# This is kind of an arbitrary decision I made, and maybe it's not the best approach?
# I think in the future, I could utilize some more robust statistical tools to better distribute
# the ordinal groups


poverty_map %>% 
  mutate(values= recode(findInterval(values, quantiles),
                        `1` = "Low risk", 
                        `2` = "Low risk", 
                        `3` = "Medium Risk", 
                        `4` = "Medium Risk", 
                        `5` = "High Risk")) %>% 
  ggplot() +
  geom_sf(aes(fill = values)) +
  theme_void() +
  labs(fill = "Risk of Poverty in Quantiles",
       title = "Degree of Risk of Poverty in Europe",
       caption = "Ava Scharfstein  \nSource: Eurostat  ") +
  scale_fill_manual(values = c(p3[3],p3[2],p3[4],p3[5],p3[1]))

