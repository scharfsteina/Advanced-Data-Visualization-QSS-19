# QSS 19: Advanced Data Visualization Final Project
# 6/6/23
# Ava Scharfstein

# Final product saved as final.png

# Load in necessary packages
library(tidyverse)
library(readxl)
library(shiny)
library(ggtext)
library(countrycode)
library(cowplot)
library(gridExtra)
library(grid)
library(gridtext)

happy_raw <- read_excel("data/happiness.xls")

# to rename all of the columns to be one word
clean_col <- function(col) {
  col %>% 
    tolower() %>% 
    str_replace_all(.,' ','_') %>% 
    str_remove_all(.,'`')
}

# happiness dataset that includes gdp data
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

# data from WHO about alcohol consumption
# https://www.who.int/data/gho/data/indicators/indicator-details/GHO/alcohol-drinkers-only-per-capita-(15-)consumption-in-litres-of-pure-alcohol
who <- read_csv("data/who.csv")

# select the relevant columns
glimpse(who)
(who <- who %>% 
    select(ParentLocationCode, ParentLocation, SpatialDimValueCode, Location, Period, Dim1, FactValueNumeric, FactValueNumericLow, FactValueNumericHigh))
# rename columns
colnames(who) <- c("region_abbr","region", "country_abbr", "country","year", "sex", "liters_per_capita", "liters_low", "liters_high")
glimpse(who)

# reconfigure dataset
df_who <- who %>% 
  select(-liters_low, -liters_high) %>% 
  pivot_wider(names_from = sex, values_from = liters_per_capita) %>% 
  rename(total_consumption = `Both sexes`,
         female_consumption = Female,
         male_consumption = Male)

# education datasets are from Global Data Lab
# https://globaldatalab.org/shdi/table/2021/msch+mschf+mschm/?levels=1&interpolation=0&extrapolation=0
temp <- read_csv("data/education.csv")
education <- temp %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(temp)), names_to = "year", values_to = "edu") %>% 
  filter(level == "National") %>% 
  select(country, year, edu) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

temp <- read_csv("data/education_men.csv") 
men_edu <- temp %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(temp)), names_to = "year", values_to = "male_edu") %>% 
  select(country, year, male_edu) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

temp <- read_csv("data/education_women.csv") 
female_edu <- temp %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(temp)), names_to = "year", values_to = "female_edu") %>% 
  select(country, year, female_edu) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

# Merge education datasets together
education <- inner_join(education, men_edu,
                        by = c("country","year")) %>% 
  inner_join(., female_edu, by = c("country","year"))

# Merge gdp and alcohol datasets with education dataset
df <- inner_join(df_who, happy, by = c("country","year")) %>% 
  inner_join(education, by = c("country","year")) %>% 
  filter(year >= 2010)

# Group 
df <- df %>% 
  mutate(gdp = exp(gdp_log),
         gdp = case_when(
           #gdp <= 1085 ~ "Low income",
           gdp <= 4255 ~ "Low to Mid GDP",
           gdp > 4255 & gdp <= 13205 ~ "Mid to High GDP",
           gdp > 13205 ~ "High GDP"),
         gdp = factor(gdp, levels = c("Low to Mid GDP", 
                                      "Mid to High GDP",
                                      "High GDP")))


# Facet title move down
strip_label <- data.frame(gdp = factor(levels(df$gdp), levels = levels(df$gdp)), x = rep(7.5,3), y = rep(23,3),
                          label = levels(df$gdp)) %>% as_tibble()

# cup shape on the figure
xs <- seq(-2,17,length.out=100)
## parabola for cup shape
cup <- function(x) { .000000000000000000000009*((x-7.5)**28)-1 } 
ymin <- cup(xs)
## sin curve for top of the glass shape
rim <- function(x) { .09*sin(1.1*x)+19 }
ymax <- rim(xs)
filled_cup <- data.frame(xs, ymin, ymax)

df %>%
  filter(year == 2019) %>% 
  ggplot()+
  # cup
  stat_function(fun = cup, color = "#c4b49b", alpha = .7, size = 1.5)+
  geom_ribbon(aes(x=xs, ymin=ymin, ymax=ymax), data=filled_cup, fill="#F7E7CE", alpha = .5)+
  # points by sex
  geom_point(aes(x = female_edu, y = female_consumption), size = 2.5, color = "#df5300",  #8B335C
             fill = "#df5300", shape = 21, stroke = .7, alpha = .4) +
  geom_point(aes(x = male_edu, y = male_consumption), size = 2.5, color = "#087E8B", 
             shape = 21, fill = "#087E8B", stroke = .7, alpha = .4) +
  # linear regression
  geom_smooth(aes(x = male_edu, group = gdp, y = male_consumption), 
              method = "lm", se = FALSE, show.legend = FALSE, color = "#087E8B") +
  geom_smooth(aes(x = female_edu, group = gdp, y = female_consumption), 
              method = "lm", se = FALSE, show.legend = FALSE, color = "#df5300") +
  # strip titles
  geom_text(data = strip_label,
            mapping = aes(x = x, y = y, label = label), 
            size = 5, color = "#333333", family = "Avenir", fontface = "italic") +
  scale_x_continuous(breaks = seq(0,15,3), limits = c(-2,17))+
  scale_y_continuous(breaks = seq(0,28, 5), limits = c(-1,25)) +
  labs(x = "", y = "\nAlcohol Consumption (Liters)\n",
       title = "<br><span><span style='color:#000000;'>Education Intoxication:</span> Alcohol Consumption Across Countries Stratified By <i>Sex</i> and <i>Wealth</i> in 2019</span>",
       subtitle = "<br><span>Alcohol consumption is positively correlated with education-level and wealth, particularly in high income countries and <span style='color:#087E8B;font-size:17px;'>Men</span> compared to <span style='color:#df5300;font-size:17px;'>Women</span>.</span>") +
  facet_wrap(~ gdp)+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        text = element_text(family = "Avenir", color = "#333333"),
        axis.text = element_text(color = "#333333", size = 10),
        axis.title = element_text(size = 14),
        plot.title = element_textbox_simple(lineheight = 1, size = 15, color = "#222222"),
        strip.text = element_blank(),
        plot.margin = margin(10,20,90,20),
        plot.subtitle = element_textbox_simple(lineheight = 1, size = 11))

# bottom of the cups
grid.draw(ellipseGrob(x=.235, y=.145, size = 7, ar = .2, angle = pi/2,
                      gp = gpar(fill = "#c4b49b", color = "#c4b49b", alpha = .2, lwd = 4)))

grid.draw(ellipseGrob(x=.535, y=.145, size = 7, ar = .2, angle = pi/2,
                      gp = gpar(fill = "#c4b49b", color = "#c4b49b", alpha = .2, lwd = 4)))

grid.draw(ellipseGrob(x=.835, y=.145, size = 7, ar = .2, angle = pi/2,
                      gp = gpar(fill = "#c4b49b", color = "#c4b49b", alpha = .2, lwd = 4)))
# stem of the cups
grid.draw(linesGrob(x = c(.235,.235), y = c(.27,.15), gp = gpar(color = "#c4b49b", alpha = .2, lwd = 4)))
grid.draw(linesGrob(x = c(.535,.535), y = c(.27,.15), gp = gpar(color = "#c4b49b", alpha = .2, lwd = 4)))
grid.draw(linesGrob(x = c(.835,.835), y = c(.27,.15), gp = gpar(color = "#c4b49b", alpha = .2, lwd = 4)))
# caption
grid.draw(textGrob(x = .86, y = .02, label = "Sources: WHO, Global Data Lab, and World Happiness Index",
                   gp = gpar(color = "#333333", fontsize = 9, fontfamily = "Avenir")))
# x axis label
grid.draw(textGrob(x = .51, y = .09, label = "Years of Education", 
                   gp = gpar(color = "#333333", fontsize = 14, fontfamily = "Avenir")))

# Note: you will have to resize this figure to be 1216 by 659 in the zoom mode in order 
# to see the proper proportions. I have saved the figure in the proper proportions
# for your convenience as "final.png"