library(tidyverse)
library(readxl)
library(countrycode)
library(gganimate)
library(plotly)
library(scales)
library(showtext)
library(shiny)
library(extrafont)

font_import()

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

same <- "#22a282"
different <- "#f2571c"

# subtitle <- list(x = -.02, y = 1.03, text = "Self-Reported Happiness vs GDP Per Person From 2009-2019 Across 115 Countries",
#                  yref = "paper", xref = "paper",
#                  font = list(color = "grey", size = 13, family = "montserrat thin"),
#                  xanchor = "middle",
#                  yanchor = "top",
#                  showarrow = FALSE)
# year09 <- list(x = .063, y = .9, text = "'09", xref = "paper", ax = -60, ay = 0, arrowcolor = "grey",
#                font = list(color = "grey", size = 13, family = "montserrat thin"),
#                xanchor = "middle",
#                yanchor = "top",
#                showarrow = TRUE)
# 
# year19 <- list(x = .065, y = .9, text = " '19", yref = "Paper", 
#                font = list(color = "grey", size = 13, family = "montserrat thin"),
#                xanchor = "middle", yanchor = "middle")
# 
# legend_title <- list(x = .01, y = .93, text = "<b>Happiness and GDP:</b>",
#                      xref = "paper", yref = "paper", 
#                      font = list(color = "grey", size = 13, family = "montserrat thin"),
#                      xanchor = "middle", yanchor = "middle")
# 
# legend_same <- list(x = .21, y = .87, text = "Moving in the <span style = 'color:#22a282'><i>same</i></span> direction ",
#                     xref = "paper", ax = -30, ay = 0, arrowcolor = same,
#                font = list(color = "grey", size = 13, family = "montserrat thin"),
#                xanchor = "right",
#                yanchor = "middle",
#                showarrow = TRUE)
# legend_different <- list(x = .215, y = .84, text = "Moving in a <span style = 'color:#f2571c'><i>different</i></span> direction ",
#                     xref = "paper", ax = -30, ay = 0, arrowcolor = different,
#                     font = list(color = "grey", size = 13, family = "montserrat thin"),
#                     xanchor = "right",
#                     yanchor = "middle",
#                     showarrow = TRUE)


plot_ly(data = df,
        x =~ xend,
        y =~ yend,
        color = ~direction,
        colors = c(different, same),
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
                  inherit = FALSE,
                  opacity = .5,
                  xend =~ xend,
                  yend =~ yend,
                  xref = "x", yref = "y",
                  axref = "x", ayref = "y",
                  arrowcolor = same,
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
                  arrowcolor = different,
                  ax = ~x,
                  ay = ~y,
                  arrowhead = 1,
                  arrowsize = .75,
                  text = "",
                  showarrow = TRUE) %>%
  layout(title = list(text = "Economic Growth Doesn't Always Mean Rising Happiness",
                      y = 0.98,
                      x = .06,
                      size = 16),
         font = list(size = 14,
                     family = "montserrat thin"),
         #annotations = list(subtitle, year09, year19, legend_title, legend_same, legend_different),
         annotations = list(x = list(-.02,.063,.065,.01,.21,.215),
                            y = list(1.03,.9,.9,.93,.87, .84),
                            text = list("Self-Reported Happiness vs GDP Per Person From 2009-2019 Across 115 Countries",
                                        "'09 ",
                                        " '19",
                                        "<b>Happiness and GDP:</b>",
                                        "Moving in the <span style = 'color:#22a282'><i>same</i></span> direction ",
                                        "Moving in a <span style = 'color:#f2571c'><i>different</i></span> direction "),
                            yref = "paper",
                            xref = "paper", #3d85c6
                            ax = list(NA, -60,NA, NA, -30, -30),
                            ay = list(NA, 0,NA, NA, 0, 0),
                            arrowcolor = list(NA, "grey", NA, NA, same,different),
                            xanchor = list("middle","left", "middle", "middle", "right","right"),
                            yanchor = list("top","middle", "middle", "middle", "middle","middle"),
                            showarrow = c(FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)),
         shapes = list(type = "rect",
                       x0 = 6.85, 
                       x1 = 7.95, 
                       y0 = 7.05, 
                       y1 = 7.85, 
                       fillcolor = "#EEEEEE",xref = "x", yref = "y"),
         plot_bgcolor = "white",
         xaxis = list(title = "GDP Per Person\n(Logarithmic Scale)",
                      tickvals = map(list(1000,10000,50000,100000,150000), log),
                      ticktext = paste0("$",c("1K","10K","50K","100K","150K"), c("\n\n<b>Poorer</b>","","", "\n\n<b>Wealthier</b>"))),
         yaxis = list(title = "Self-Reported Happiness (1-10)",
                      ticktext = list("<b>Unhappier   </b> 3","4","5","6","7","<b>Happier   </b> 8"),
                      tickvals = 3:8),
         legend = list(orientation = 'h'),
         margin = list(l = 160, r = 160, b = 100, t = 60, pad = 10))
         # updatemenus = list(
         #   list(type = "buttons",
         #        buttons = list(
         #          list(x = .5, y = 0.5, label = "Same", method = "update",
         #               args = list(list(visible = TRUE),
         #                           list(annotations = test)))))))

# change the width of the border of the legend, shift the legend over, move the title over? or subtitle

fig

# ui <- fluidPage(
#   selectizeInput(inputId = "direction",
#                  label = "Select ")
#   # selectizeInput(
#   #   inputId = "genre", 
#   #   label = "Select a genre", 
#   #   choices = unique(music$genre), 
#   #   selected = "rock",
#   #   multiple = TRUE
#   # ),
#   plotlyOutput(outputId = "p", height = 800)
# )
# 
# server <- function(input, output, ...) {
#   output$p <- renderPlotly({
#     fig
#   })
# }
# 
# # click open brower to see the shiny plot and interact
# shinyApp(ui, server)
