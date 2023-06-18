# Project 1: WIID
# QSS 19: Advanced Data Visualization
# Ava Scharfstein, May 2023

library(tidyverse)
library(readxl)
library(countrycode)
library(gganimate)
library(plotly)
library(scales)
library(showtext)

# library(extrafont)
# font_import()

# https://twitter.com/Vijay__Jadhav/status/1649123605949845504
happy_raw <- read_excel("whr2023.xls")

clean_col <- function(col) {
  col %>% 
    tolower() %>% 
    str_replace_all(.,' ','_') %>% 
    str_remove_all(.,'`')
}

# population

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

# Get the countries that we have data for in 2012 and 2022
happy_10y <- happy %>% 
  drop_na(life_ladder) %>% 
  drop_na(gdp_log) %>% 
  group_by(country) %>% 
  filter(year == 2019 | year == 2009) %>% 
  count(country) %>% 
  filter(n == 2) %>% 
  select(country) %>% 
  ungroup() %>% 
  inner_join(happy, by = "country") %>% 
  filter(year == 2019 | year == 2009) %>% 
  mutate(year = factor(year))

df2009 <- happy_10y %>% 
  filter(as.numeric(year)==1) %>% 
  summarise(country, gdp_log09 = gdp_log, life_ladder09 = life_ladder)

df2019 <- happy_10y %>% 
  filter(as.numeric(year)==2) %>% 
  summarise(country, gdp_log19 = gdp_log, life_ladder19 = life_ladder)

df <- df2009 %>% 
  inner_join(df2019, by = "country") %>% 
  mutate(same = (gdp_log09 - gdp_log19)/(life_ladder09-life_ladder19)>0,
         same = ifelse(same, "Same Direction", "Different Direction"))

# fit a regression for every country as a function of time
# colorcode the arrows by con
df %>% 
  plot_ly(x =~ gdp_log19,
          y =~ life_ladder19,
          color = ~same,
          colors = c("orange","#3d85c6"),
          size = 4,
          alpha = .7,
          type='scatter',
          mode = "markers",
          marker = list(symbol = "circle"),
          hoverlabel = list(font = list(size = 12, color = "black")),
          hovertemplate = paste(
            "<b>%{text}</b><br>",
            "Log GDP: %{x}<br>",
            "Happiness: %{y}",
            "<extra></extra>"),
          text = ~paste0(country,": 2019")) %>%
  # add_trace(data = df,
  #           inherit = F,
  #           x =~ gdp_log09,
  #           y =~ life_ladder09,
  #           mode = "markers",
  #           marker = list(color = "black", size = 2),
  #           type = "scatter",
  #           hoverlabel = list(font = list(size = 12, color = "white")),
  #           hovertemplate = paste(
  #             "<b>%{text}</b><br>",
  #             "Log GDP: %{x}<br>",
  #             "Happiness: %{y}",
  #             "<extra></extra>"),
  #           text = ~paste0(country,": 2009"),
  #           showlegend = FALSE) %>% 
  add_annotations(data = df %>% 
            filter(same == "Same Direction"),
          xend =~ gdp_log19,
          yend =~ life_ladder19,
          xref = "x", yref = "y",
          axref = "x", ayref = "y",
          arrowcolor = "#3d85c6",
          ax = ~gdp_log09,
          ay = ~life_ladder09,
          arrowhead = 1,
          arrowsize = .75,
          text = "",
          showarrow = TRUE,
          showlegend = TRUE) %>% 
  add_annotations(data = df %>% filter(same == "Different Direction"),
                  inherit = FALSE,
                  xend =~ gdp_log19,
                  yend =~ life_ladder19,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  arrowcolor = "orange",
                  ax = ~gdp_log09,
                  ay = ~life_ladder09,
                  arrowhead = 1,
                  arrowsize = .75,
                  text = "",
                  showarrow = TRUE,
                  showlegend = TRUE) %>% 
  layout(title = list(text = "Money Doesn't Always Buy Happiness",
                      y = 0.98,
                      x = .06),
         font = list(family = '',
                     size = 13),
         plot_bgcolor = "white",
         xaxis = list(title = "Log GDP"),
         yaxis = list(title = "Happiness Score"),
         legend = list(orientation = 'h'), 
         margin = list(l = 100, r = 100, b = 100, t = 50, pad = 10))


# for each country fit a regression

# happy_10y <- happy %>% 
#   drop_na(life_ladder, gdp_log) %>% 
#   filter(year >= 2012 & year <= 2022) %>% 
#   count(country) %>% 
#   filter(n==10) %>% 
#   select(country) %>% 
#   inner_join(happy %>% filter(year >= 2012 & year <= 2022), by = "country")
# 
# happy_10y %>% 
#   ggplot(aes(x = gdp_log, y = life_ladder, color = continent)) +
#   geom_point() +
#   geom_smooth(method = "lm", aes(group = continent), color = "red", se = F)+
#   facet_wrap(~continent)

happy_10y <- happy %>% 
  # make sure there are data points for 2010 and 2019
  filter(year == 2010 | year == 2019) %>% 
  select(country) %>% 
  distinct() %>% 
  left_join(happy, by = "country") %>% 
  filter(year >= 2010 & year <= 2019) %>% 
  select(country, year, gdp_log, life_ladder)

regression <- happy_10y %>% 
  mutate(year = year-2010) %>% 
  nest_by(country) %>% 
  mutate(mod = list(lm(life_ladder ~ year, data = data))) %>% # happiness by time
  summarize(broom::tidy(mod)) %>% 
  select(-std.error, -statistic, -p.value) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>% 
  rename("slope" = "year",
         "intercept" = `(Intercept)`)
  
# intercept = happiness for 2010 vs gdp_log 09
# h+m*9 = happiness for 2019 vs gdp_log 19

df <- happy_10y %>% 
  filter(year == 2010 | year == 2019) %>% 
  pivot_wider(names_from = year, values_from = c(life_ladder, gdp_log)) %>% 
  inner_join(regression, by = "country") %>% 
  mutate(delta_gdp = gdp_log_2019-gdp_log_2010) %>%  # true = +, false = -
  summarise(country,
            delta_gdp, slope,
            x = gdp_log_2010, y = intercept,
            xend = gdp_log_2019, yend = intercept + slope*9,
            direction = ifelse((delta_gdp>0) == (slope>0), "Same Direction", "Different Direction")) %>% 
  drop_na() %>% 
  filter(country != "Venezuela")

# minimal is good
# add more personal touches (fonts)
# more decorations with additional information
# dropdown, back and forth between the two groups

plot_ly(data = df,
        x =~ xend,
        y =~ yend,
        color = ~direction,
        colors = c("#f2571c","#7eac6a"),
        opacity = .2,
        type='scatter',
        mode = "markers",
        marker = list(symbol = "circle", size = 1),
        hoverlabel = list(font = list(size = 12, color = "black", family = "montserrat thin")),
        hovertemplate = paste("%{text}", "<extra></extra>"),
        text = ~paste0("<b>",country, "</b><br>\U0394 Log GDP: ", round(delta_gdp,2),
                       "\n\U0394 Happiness: ", round(slope,2)),
        opacity = .5,
        showlegend = FALSE) %>%
  add_annotations(data = df %>% 
                    filter(direction == "Same Direction"),
                  xend =~ xend,
                  yend =~ yend,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  arrowcolor = "#7eac6a",
                  opacity = .5,
                  ax = ~x,
                  ay = ~y,
                  arrowhead = 1,
                  arrowsize = .75,
                  text = "",
                  showarrow = TRUE) %>% 
  add_annotations(data = df %>% filter(direction == "Different Direction"),
                  inherit = FALSE,
                  xend =~ xend,
                  yend =~ yend,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  arrowcolor = "#f2571c",
                  ax = ~x,
                  ay = ~y,
                  arrowhead = 1,
                  arrowsize = .75,
                  text = "",
                  showarrow = TRUE) %>% 
  layout(title = list(text = "Economic Growth Doesn't Always Mean Rising Happiness",
                      y = 0.98,
                      x = .06),
         font = list(size = 14,
                     family = "montserrat thin"),
         annotations = list(x = list(-.02,.063,.065,.01,.21,.215),
                            y = list(1.03,.9,.9,.93,.87, .84),
                            text = list("Self-Reported Happiness vs GDP Per Person From 2009-2019 Across 115 Countries",
                                        "'09 ",
                                        " '19",
                                        "<b>Happiness and GDP:</b>",
                                        "Moving in the<span style = 'color:#7eac6a'><i>same</i></span> direction ",
                                        "Moving in a <i>different</i> direction "),
                            yref = "paper",
                            xref = "paper",
                            ax = list(NA, -60,NA, NA, -30, -30),
                            ay = list(NA, 0,NA, NA, 0, 0),
                            arrowcolor = list(NA, "grey", NA, NA, "#7eac6a","#f2571c"),
                            
                            font = list(color = list("grey","grey","grey","grey","#7eac6a","#f2571c"),
                                        size = list(13,13,13,14,13,13),
                                        family = "montserrat thin"),
                            xanchor = list("middle","left", "middle", "middle", "right","right"),
                            yanchor = list("top","middle", "middle", "middle", "middle","middle"),
                            showarrow = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)),
         shapes = list(type = "rect",
                       x0 = 6.85, x1 = 7.95, y0 = 7.05, y1 = 7.85, fillcolor = "#EEEEEE",xref = "x", yref = "y"),
         plot_bgcolor = "white",
         xaxis = list(title = "GDP Per Person\n(Logarithmic Scale)",
                      tickvals = map(list(1000,10000,50000,100000,150000), log),
                      ticktext = paste0("$",c("1K","10K","50K","100K","150K"))),
         yaxis = list(title = "Self-Reported Happiness (1-10)",
                      ticktext = list("<b>Unhappy</b> 3","4","5","6","7","<b>Happy</b> 8"),
                      tickvals = 3:8),
         legend = list(orientation = 'h'),
         margin = list(l = 100, r = 100, b = 100, t = 60, pad = 10))

  
  

df %>% 
  group_by(country) %>% 
  slice_max(gdp_log) %>% 
  summarise(max_gdp_log = gdp_log)

# happy %>% 
#   filter(year >= 2010 & year < 2020) %>% 


