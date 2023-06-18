# Project 1: WIID
# QSS 19: Advanced Data Visualization
# Ava Scharfstein, April/May 2023

library(tidyverse)
library(readxl)
library(countrycode)
library(gganimate)
library(plotly)
library(scales)

# The figure I am trying to reproduce and improve:
# https://twitter.com/Vijay__Jadhav/status/1649123605949845504
happy_raw <- read_excel("whr2023.xls")

clean_col <- function(col) {
  col %>% 
    tolower() %>% 
    str_replace_all(.,' ','_') %>% 
    str_remove_all(.,'`')
}

# Info about dataset variables:
# https://happiness-report.s3.amazonaws.com/2023/WHR+23_Statistical_Appendix.pdf

happy <- happy_raw %>% 
  rename_with(., clean_col) %>% 
  rename(gdp_log = log_gdp_per_capita) %>% 
  rename(corruption = perceptions_of_corruption) %>% 
  rename(country = country_name) %>% 
  # Cleaning up errors
  #  - Kosovo not recognized as a country in countrycode - located in Europe
  #  - Turkey mispelled as Turkiye
  mutate(country = str_replace(country, "Turkiye", "Turkey"),
         continent = countrycode(sourcevar = country,
                                 origin = "country.name",
                                 destination = "continent"),
         continent = ifelse(country=="Kosovo", "Europe", continent)) 

education_raw <- read_csv("GDL-Mean-years-schooling-data.csv")

education <- education_raw %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(education_raw)), names_to = "year", values_to = "education") %>% 
  filter(level == "National") %>% 
  select(country, year, education) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

df <- happy %>% 
  inner_join(education, by = c("country","year")) %>% 
  select(country, continent, year, life_ladder, gdp_log, corruption, education)

edu_quantiles <- quantile(df2021$education)

df2021 <- df %>% 
  group_by(country) %>% 
  # fills missing data with data from previous year
  fill(gdp_log, .direction = "down") %>% 
  ungroup() %>% 
  filter(year == 2021) %>% 
  mutate(gdp = paste0("$",as.character(round(exp(gdp_log)),-2)),
         education_level = factor(cut(education, breaks = c(0,edu_quantiles),
                                      labels = c("0-2 years",
                                                 "2-7.5 years",
                                                 "7.5-10.5 years",
                                                 "10.5-12 years",
                                                 "12-14 years"))))
  

# first draft no color
df2021 %>% 
  ggplot(aes(x = gdp_log, y = life_ladder)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Log GDP", y = "Happiness Score",
       title = "Happiness and GDP By Country in 2021")

# first draft w color
df2021 %>% 
  ggplot(aes(x = gdp_log, y = life_ladder, color = continent)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Log GDP", y = "Happiness Score",
       title = "Happiness and GDP By Country in 2021")

get_decade <- function(year) {
  year - year %% 10 
}

# Animate over time by decade (just to see what it looks like over time)
df_decades <- df %>% 
  mutate(decade = get_decade(year),
         decade = paste0(as.character(decade),"s")) %>% 
  group_by(decade, country) %>% 
  summarise(gdp_log = mean(gdp_log, na.rm = T),
            life_ladder = mean(life_ladder, na.rm = T),
            continent) %>% 
  distinct() %>% 
  ungroup()

p <- df_decades %>% 
  ggplot(aes(x = gdp_log, y = life_ladder, color = continent)) +
  geom_point() +
  theme_minimal() +
  transition_states(decade) +
  labs(x = "Log GDP", y = "Happiness Score",
     title = "Happiness and GDP By Country in {closest_state}")

animate(p, nframes=length(df_decades$decade))


# A central issue with this figure in addition to how busy it is
# is that the figure creates this narrative between the relationship of 
# gdp and happiness and doesn't really include any other variables
# people who don't understand statistics will likely come to the conclusion
# that countries are happier due to wealth
# My goal is to reinforce correlation ≠ causation and provide some 
# other variables to support that happiness is related to so many other
# things (eg. level of education)
### https://globaldatalab.org/shdi/table/msch/?levels=1&interpolation=0&extrapolation=0
# I'm not 100% decided on this, but one idea was to make a R shiny 
# app and be able to switch between variables
# OR somehow incorporate a third or fourth variable into this plot 
# I have some really rough sketches but nothing I think that is worth sharing
# at the moment

# Alternatively, I might try to do something with the time variable?


# Replacing continent with education level
df2021 %>% 
  ggplot(aes(x = gdp_log, y = life_ladder, color = education)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Log GDP", y = "Happiness Score", color = "Years of Education",
       title = "Happiness, GDP, and Education Level By Country in 2021")

# Including perceived corruption
breaks <-  c(6,7,8,9,10,11,12)

df2021 %>%
  ggplot(aes(x = corruption)) +
  geom_histogram()

quantiles <- quantile(df2021$corruption, na.rm = T)

rc_pal <- colorRampPalette(c("orange","#3d85c6"))
colors <- c("orange","#3d85c6")

# dark mode ??
# subplots 

p1 <- df2021 %>% 
  mutate(gdp = exp(gdp_log)) %>% 
  ggplot(aes(x = gdp_log, 
             y = life_ladder, 
             color = education))+#corruption, 
             #size = education)) +
  geom_point(size = 4, alpha = .7, #, 
             aes(text = paste0("(", round(gdp_log,2),
                               ", ", round(life_ladder,2),
                               ")\nCountry: ", country,
                               "\nEducation: ", round(education,2)))) +
  theme_minimal() +
  labs(x = "Log GDP", 
       y = "Happiness Score", 
       color = "Years of Education",
       #size = "Years of Education",
       title = "Happiness, GDP, and Education Level By Country in 2021") +
  #scale_color_gradient() +
  theme(legend.position = "bottom") +
  scale_color_gradient(low = colors[2], high = colors[1],
                       breaks = seq(0,16,4),
                       labels = seq(0,16,4))
p1

ggplotly(p1, tooltip = c("text"))


# gg_build --> get the bubbles information


# Switch axes
# df2021 %>% 
#   ggplot(aes(x = education, 
#              y = life_ladder, 
#              color = gdp_log, 
#              size = corruption)) +
#   geom_point(alpha = .7) +
#   theme_minimal() +
#   labs(x = "Education Level", 
#        y = "Happiness Score", 
#        color = "Log GDP",
#        size = "Perceived Corruption",
#        title = "Happiness, GDP, and Education Level By Country in 2021")


# facet by country??

df2021 %>% 
  plot_ly(x =~ gdp_log,
          y =~ life_ladder,
          opacity = 0.95,
          color =~ education,
          colors = rc_pal(200),
          size = 4,
          type='scatter',
          mode = "markers",
          hoverlabel = list(font = list(size = 12, color = "black")),
          hovertemplate = paste(
            "<b>%{text}</b>",
            "(%{x:.2f}, %{y:.2f})<br>",
            "Years of Education:%{marker.color:.2f}", 
            "<extra></extra>"),
          text = ~paste0(country,":")) %>% 
  layout(title = list(text = "Happiness, Log GDP, and Years of Education Across Countries in 2021",
                      y = 0.98,
                      x = .06),
         font = list(family = '',
                     size = 13),
         plot_bgcolor = "white",
         xaxis = list(title = "Log GDP"),
         yaxis = list(title = "Happiness Score"),
         legend = list(orientation = 'h'), 
         margin = list(l = 100, r = 100, b = 100, t = 50, pad = 10)) %>% 
  colorbar(title = "Years of\nEducation\n")
  

df2021 %>% 
  group_by(education_level) %>% 
  do(p=plot_ly(., x =~ gdp_log,
             y =~ life_ladder,
             color =~ education_level,
             colors = rc_pal(5),
             size = 4,
             alpha = .7,
             type = "scatter",
             mode = "markers") %>% 
       layout(xaxis = list(title = "Log GDP"),
              yaxis = list(title = "Happiness"))) %>%
  subplot(nrows = 1, shareX = TRUE, shareY = TRUE)

df2021 %>%  
  ggplot(aes(x = gdp_log,
           y = life_ladder))+
  facet_wrap(~education_level, scales="free_x") +
  geom_point(size = 4, alpha = .7) +
  geom_smooth(method = lm)+
  theme_minimal() +
  labs(x = "Log GDP",
       y = "Happiness Score",
       color = "Years of Education") +
  theme(legend.position = "bottom") +
  scale_color_gradient(low = colors[2], high = colors[1],
                       breaks = seq(0,16,4),
                       labels = seq(0,16,4))

subplot(simpsons, margin = 0.01) %>%
  subplot(main, margin = 0.1)

