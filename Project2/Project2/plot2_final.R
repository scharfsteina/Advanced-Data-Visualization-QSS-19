library(tidyverse)
library(usmap)
library(plotly)
library(scales)
library(cowplot)
library(gganimate)

# https://www.transtats.bts.gov/homepage.asp data source

dest <- read_csv("data/deststate.csv", skip = 1)

df <- dest %>% 
  filter(!str_detect(Description, "Unknown|All rows")) %>% 
  rename(state = Description,
         state_abbr = DEST_STATE_ABR,
         year = Year,
         flights = `Sum(FLIGHTS)`) %>% 
  group_by(year) %>% 
  mutate(rank = rank(-flights),
         region = case_when(
           state_abbr %in% .midwest_region ~ "Midwest",
           state_abbr %in% .south_region ~ "South",
           state_abbr %in% .northeast_region ~ "North East",
           state_abbr %in% .west_region ~ "West")) %>% 
  ungroup() %>% 
  drop_na(region)


df %>% 
  filter(year == 2002) %>% 
  slice_max(flights, n=5) %>% 
  pull(state)

p <- df %>% 
  mutate(States = case_when(state=="California" ~ "California",
                           state=="Texas" ~ "Texas",
                           TRUE ~ "Other"),
         States = factor(States, levels = c("California", "Texas", "Other"))) %>% 
  ggplot(aes(x = year, y = flights, group = state, color = States, alpha = States)) +
  geom_line(aes(linetype = States, size = States)) +
  scale_color_manual(values = c("#2233d9", "#BF5700", "black")) +
  scale_linetype_manual(values = c("dashed","dashed","solid"))+
  scale_alpha_manual(values = c(.7,.7,.3))+
  scale_size_manual(values = c(.8,.8,.5))+
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
  scale_x_continuous(breaks = seq(1990, 2020, by = 5))+
  annotate("text", x = 2016, y = 500000, label = "Florida", color = "black", size = 3.5,
           family = "Andale Mono")+
  annotate("segment", x = 2016, y = 485000, xend = 2017, yend = 475000)+
  annotate("text", x = 2001, y = 450000, label = "Illinois", color = "black", size = 3.5,
           family = "Andale Mono")+
  annotate("segment", x = 2001, y = 430000, xend = 2002, yend = 420000)+
  annotate("text", x = 2004.5, y = 380000, label = "Georgia", color = "black", size = 3.5,
           family = "Andale Mono")+
  annotate("segment", x = 2004.5, y = 365000, xend = 2003, yend = 335000)+
  annotate("text", x = 2010, y = 340000, label = "New York", color = "black", size = 3.5,
           family = "Andale Mono")+
  annotate("segment", x = 2010, y = 330000, xend = 2009.5, yend = 310000)+
  theme_minimal() +
  labs(x = "\nYear", y = "Number of Flights\n",
       title ="<b><i style='color:#2233d9;'>California</b></i> and <b><i style='color:#BF5700;'>Texas</b></i> are Consistently the Top US Flight Destinations",
       caption = "Source: US Bureau of Transportation",
       subtitle = "\nFlorida, Illinois, Georgia, and New York trail behind") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#e4f2ff"),
        text = element_text(family = "Andale Mono", face = "bold", color = "black"),
        axis.text = element_text(color = "black", size = 12),
        legend.text = element_text(size = 12),
        plot.margin = margin(30,50,20,50, unit = "pt"),
        title = element_text(size = 14),
        plot.subtitle = element_text(size = 13),
        plot.title = element_textbox_simple())

ggdraw(p) +
  draw_image(magick::image_read("data/airplane_yellow.png"),
             scale = .09, x = .4, y = .66, hjust = 1, vjust = 1, halign = 1, valign = 1)

