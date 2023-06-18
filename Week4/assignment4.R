## Advanced Data Visualization (QSS 19) Spring 2023
## R Review III: Web Scraping, Maps, Animations, and More
##
## Name: Ava Scharfstein
## Date: April 20th - April 26th, 2023

# Install Packages 
library(tidyverse)
library(rvest)
library(gganimate)
library(readxl)
library(scales)
library(magick)
library(viridis)

# Part 2: Web Scraping ----------------------------------------------------
# We discussed in office hours that this multiple mapping format was okay
sou <- "https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/state-the-union-addresses"

(urls <- stringr::str_c(sou,
                        "?page=", 0:9)) 


merge_lists <- function(list) {
  list %>% 
    flatten() %>% 
    unlist()
}

# We talked in office hours and this is effectively 
# the same thing as putting it into one function

dates <- map(urls,
             ~ read_html(.) %>%
               html_elements("h4 span") %>% 
               html_text()) %>% 
  merge_lists()

titles <- map(urls,
              ~ read_html(.) %>%
                html_elements(".field-title p") %>%
                html_text() %>%
                trimws()) %>%
  merge_lists()


# get all the links to the speeches
speech_links <- map(urls,
                    ~ read_html(.) %>%
                      html_elements("p a") %>%
                      html_attr("href") %>%
                      as_tibble() %>%
                      filter(str_detect(value, "/documents/")) %>%
                      pull(value)) %>%
  merge_lists()

speech_links <- stringr::str_c("https://www.presidency.ucsb.edu",speech_links)

# navigate into each speech and collect the length
speech_length <- map(speech_links,
  ~ read_html(.) %>% 
  html_elements(".col-sm-8") %>% 
  html_text() %>% 
  str_replace_all(., "\n", "") %>% 
  strsplit(., " ") %>% 
  sapply(. , length))%>% 
  merge_lists()
  
# add dates and speeches into a data frame
(df <- tibble(titles, dates, speech_length) %>% 
  separate(dates, c("month", "day", "year")) %>% 
  select(titles, year, speech_length) %>% 
  arrange(year) %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(str_detect(tolower(titles), "state of the union|address")) %>% 
  select(-titles) %>% 
  group_by(year) %>% 
  summarize(avg_length = mean(speech_length)))

a1 <- ggplot(df, aes(x = year, y = avg_length)) +
  geom_col() +
  scale_x_continuous(breaks = seq(1800,2023,25)) +
  theme_minimal() +
  labs(x = "Year", y = "Speech Length (words)",
       title = "Speech Length by Year for US State of the Unions") +
  transition_manual(year, cumulative = TRUE) +
  enter_grow()

animate(
  plot = a1, 
  nframes = length(df$year), 
  fps = 4
)


# Part 3: Animations ------------------------------------------------------

# Presidential Proclamations

pres_url <- "https://www.presidency.ucsb.edu/documents/app-categories/written-presidential-orders/presidential/proclamations?items_per_page=60"

(urls <- stringr::str_c(pres_url,
                        "&page=", 0:151))

dates <- lapply(1:length(urls),
       function(i) {
         print(paste("Page", as.character(i), "of", length(urls)))
         urls[i] %>% 
         read_html(.) %>% 
           html_elements("h4 span") %>% 
           html_text()
         })

dates %>% merge_lists()

df2 <- dates %>% 
  as_tibble() %>% 
  separate(value, c("month", "day", "year")) %>% 
  count(year) %>% 
  mutate(year = as.numeric(year))

ggplot(df2, aes(x = year, y = n)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Number of Documents",
       title = "Number of Presidential Proclamations Per Year") +
  transition_reveal(year)


# 2 - WIID plot
wiid <- read_excel("Week1/adv_hw1_data/wiid_2022_data/WIID_30JUN2022_0.xlsx")

df3 <- wiid %>%
  filter(str_detect(country,"United States|Canada")) %>% 
  #filter(country == country) %>% 
  select(country, d1:d10) %>% 
  drop_na()

p3 <- df3 %>% 
  group_by(country) %>% 
  summarise(across(d1:d10, mean)) %>% 
  as_tibble() %>% 
  pivot_longer(., cols = d1:d10, names_to = "decile", values_to = "value") %>% 
  mutate(decile = factor(decile, levels = c(stringr::str_c("d",1:10)))) %>%
  ggplot(aes(x = decile,
             y = value, fill = decile)) +
  geom_col() +
  coord_polar() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(), 
        axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
        axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL,
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(size = 8),
        legend.title = element_text(size = 7)) +
  labs(title = paste("Average Deciles of Percent Income for the US and Canada"),
       fill = "Deciles") +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(~ country) +
  transition_states(decile) +
  shadow_mark() +
  enter_grow()

animate(p3, duration = 10, 
        fps = 10,
        res = 180,
        width = 800,
        height = 800,
        renderer = magick_renderer())
