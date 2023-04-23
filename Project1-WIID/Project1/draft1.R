# Project 1: WIID
# QSS 19: Advanced Data Visualization
# Ava Scharfstein, April 2023

library(tidyverse)
library(readxl)
library(countrycode)
# library(eurostat)
# 
# tmp <- get_eurostat_toc()
# 
# ## Getting the data source
# tmp %>% 
#   mutate(title = tolower(title)) %>% 
#   filter(str_detect(title, "happy")) %>% 
#   select(title, code) %>% View()
# 
# df <- get_eurostat("ilc_pw08")
#   
# glimpse(df)

df_raw <- read_excel("whr2023.xls")

clean_col <- function(col) {
  col %>% 
    tolower() %>% 
    str_replace_all(.,' ','_') %>% 
    str_remove_all(.,'`')
}

# Info about dataset variables:
# https://happiness-report.s3.amazonaws.com/2023/WHR+23_Statistical_Appendix.pdf

df <- df_raw %>% 
  rename_with(., clean_col) %>% 
  rename(gdp_log = log_gdp_per_capita) %>% 
  # Cleaning up errors
  #  - Kosovo not recognized as a country in countrycode - located in Europe
  #  - Turkey mispelled as Turkiye
  mutate(country_name = str_replace(country_name, "Turkiye", "Turkey"),
         continent = countrycode(sourcevar = country_name,
                                 origin = "country.name",
                                 destination = "continent"),
         continent = ifelse(country_name=="Kosovo", "Europe", continent)) 


df2022 <- df %>% 
  filter(year == 2022) %>% 
  select(country_name, year, life_ladder, gdp_log, continent) %>% 
  group_by(country_name)

# first draft no color
df2022 %>% 
  ggplot(aes(x = gdp_log, y = life_ladder)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Log GDP", y = "Happiness Score",
       title = "Happiness and GDP By Country")

# first draft w color
df2022 %>% 
  ggplot(aes(x = gdp_log, y = life_ladder, color = continent)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Log GDP", y = "Happiness Score",
       title = "Happiness and GDP By Country")

# 
df2022 %>% 
  ggplot(aes(x = gdp_log, y = life_ladder, color = continent)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Log GDP", y = "Happiness Score",
       title = "Happiness and GDP By Country")
