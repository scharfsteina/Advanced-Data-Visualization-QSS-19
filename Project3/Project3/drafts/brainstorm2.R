library(tidyverse)
library(readxl)
library(shiny)
library(ggtext)
library(countrycode)
library(cowplot)
library(gridExtra)
library(grid)
#library(Rgraphics)
library(gridtext)

#loadfonts()

happy_raw <- read_excel("happiness.xls")

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


who <- read_csv("who.csv")
glimpse(who)
(who <- who %>% 
    select(ParentLocationCode, ParentLocation, SpatialDimValueCode, Location, Period, Dim1, FactValueNumeric, FactValueNumericLow, FactValueNumericHigh))

colnames(who) <- c("region_abbr","region", "country_abbr", "country","year", "sex", "liters_per_capita", "liters_low", "liters_high")
glimpse(who)
df_who <- who %>% 
  select(-liters_low, -liters_high) %>% 
  pivot_wider(names_from = sex, values_from = liters_per_capita) %>% 
  rename(total_consumption = `Both sexes`,
         female_consumption = Female,
         male_consumption = Male)

temp <- read_csv("education.csv")
education <- temp %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(temp)), names_to = "year", values_to = "edu") %>% 
  filter(level == "National") %>% 
  select(country, year, edu) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

temp <- read_csv("education_men.csv") 
men_edu <- temp %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(temp)), names_to = "year", values_to = "male_edu") %>% 
  select(country, year, male_edu) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

temp <- read_csv("education_women.csv") 
female_edu <- temp %>% 
  rename_with(., clean_col) %>% 
  pivot_longer(., cols = c(7:length(temp)), names_to = "year", values_to = "female_edu") %>% 
  select(country, year, female_edu) %>%  # education variable = avg years of education
  mutate(year = as.numeric(year))

education <- inner_join(education, men_edu,
                        by = c("country","year")) %>% 
  inner_join(., female_edu, by = c("country","year"))

df <- inner_join(df_who, happy, by = c("country","year")) %>% 
  inner_join(education, by = c("country","year")) %>% 
  filter(year >= 2010)

#df_2010 <- df %>% filter(year == 2010)

#cor(df_2010$total, df_2010$edu)

#lm(df_2010$total~df_2010$edu+df_2010$gdp_log)

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



strip_label <- data.frame(gdp = factor(levels(df$gdp), levels = levels(df$gdp)), x = rep(7.5,3), y = rep(23,3),
                          label = levels(df$gdp)) %>% as_tibble()

xs <- seq(-2,17,length.out=100)
cup <- function(x) { .000000000000000000000009*((x-7.5)**28)-1 }
ymin <- cup(xs)
rim <- function(x) { .09*sin(1.1*x)+19 }
ymax <- rim(xs)

filled_cup <- data.frame(xs, ymin, ymax)

df %>%
  filter(year == 2019) %>% 
  ggplot()+
  stat_function(fun = cup, color = "#c4b49b", alpha = .7, size = 1.5)+
  geom_ribbon(aes(x=xs, ymin=ymin, ymax=ymax), data=filled_cup, fill="#F7E7CE", alpha = .5)+
  geom_point(aes(x = female_edu, y = female_consumption), size = 2.5, color = "#df5300",  #8B335C
             fill = "#df5300", shape = 21, stroke = .7, alpha = .4) +
  geom_point(aes(x = male_edu, y = male_consumption), size = 2.5, color = "#087E8B", 
             shape = 21, fill = "#087E8B", stroke = .7, alpha = .4) +
  geom_smooth(aes(x = male_edu, group = gdp, y = male_consumption), 
              method = "lm", se = FALSE, show.legend = FALSE, color = "#087E8B") +
  geom_smooth(aes(x = female_edu, group = gdp, y = female_consumption), 
              method = "lm", se = FALSE, show.legend = FALSE, color = "#df5300") +
  scale_x_continuous(breaks = seq(0,15,3), limits = c(-2,17))+
  scale_y_continuous(breaks = seq(0,28, 5), limits = c(-1,25)) +
  labs(x = "", y = "\nAlcohol Consumption (Liters)\n",
       title = "<br><span><span style='color:#000000;'>Education Intoxication:</span> Alcohol Consumption Across Countries Stratified By <i>Sex</i> and <i>Wealth</i> in 2019</span>",
       subtitle = "<br><span>Alcohol consumption is positively correlated with education-level and wealth, particularly in <span style='color:#087E8B;font-size:17px;'>Men</span> compared to <span style='color:#df5300;font-size:17px;'>Women</span></span>") +
  geom_text(data = strip_label,
            mapping = aes(x = x, y = y, label = label), 
            size = 5, color = "#333333", family = "Avenir", fontface = "italic") +
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

grid.draw(ellipseGrob(x=.235, y=.145, size = 7, ar = .2, angle = pi/2,
                      gp = gpar(fill = "#c4b49b", color = "#c4b49b", alpha = .2, lwd = 4)))

grid.draw(ellipseGrob(x=.535, y=.145, size = 7, ar = .2, angle = pi/2,
                      gp = gpar(fill = "#c4b49b", color = "#c4b49b", alpha = .2, lwd = 4)))

grid.draw(ellipseGrob(x=.835, y=.145, size = 7, ar = .2, angle = pi/2,
                      gp = gpar(fill = "#c4b49b", color = "#c4b49b", alpha = .2, lwd = 4)))

grid.draw(linesGrob(x = c(.235,.235), y = c(.27,.15), gp = gpar(color = "#c4b49b", alpha = .2, lwd = 4)))
grid.draw(linesGrob(x = c(.535,.535), y = c(.27,.15), gp = gpar(color = "#c4b49b", alpha = .2, lwd = 4)))
grid.draw(linesGrob(x = c(.835,.835), y = c(.27,.15), gp = gpar(color = "#c4b49b", alpha = .2, lwd = 4)))
grid.draw(textGrob(x = .86, y = .02, label = "Sources: WHO, Global Data Lab, and World Happiness Index",
                   gp = gpar(color = "#333333", fontsize = 9, fontfamily = "Avenir")))
grid.draw(textGrob(x = .51, y = .09, label = "Years of Education", 
                   gp = gpar(color = "#333333", fontsize = 14, fontfamily = "Avenir")))
# grid.draw(richtext_grob(x = .235, y = .77, gp = gpar(fontsize = 11),
#                         text = paste0("<span><i style = 'color:#df5300;'>R = ", 
#                                      cor(df %>% filter(as.numeric(gdp)==1) %>% pull(female_consumption), 
#                                           df %>% filter(as.numeric(gdp)==1) %>% pull(edu)) %>% round(.,3), 
#                                      "</i>, <i style = 'color:#087E8B;'>R = ",
#                                      cor(df %>% filter(as.numeric(gdp)==1) %>% pull(male_consumption), 
#                                           df %>% filter(as.numeric(gdp)==1) %>% pull(edu)) %>% round(.,3), 
#                                      "</i> </span>")))



#ggsave("plot.png", plot = last_plot(), width = 2402, height = 1256, unit = "px", dpi = 150)

df %>%
  filter(year == 2019) %>% 
  ggplot()+#aes(shape = gdp)) +
  geom_point(aes(x = female_edu, y = female_consumption), size = 2.5, color = "#17a0aa", alpha = .6, show.legend = FALSE) +
  geom_point(aes(x = male_edu, y = male_consumption), size = 2.5, color = "#f25e35", alpha = .6, show.legend = FALSE) +
  geom_smooth(aes(x = male_edu, group = gdp, y = male_consumption), method = "lm", se = FALSE, show.legend = FALSE, color = "#f25e35") +
  geom_smooth(aes(x = female_edu, group = gdp, y = female_consumption), method = "lm", se = FALSE, show.legend = FALSE, color = "#17a0aa") +
  scale_x_continuous(breaks = seq(0,15,3))+
  scale_y_continuous(breaks = seq(0,25,5)) +
  labs(x = "\nYears of Education\n", y = "\nAlcohol Consumption (Liters)\n",
       color = "Log GDP", title = "\nIntoxicating Inequalities: Alcohol Consumption By Sex, Education, and Wealth",
       subtitle = "<span><span style='color:#f25e35'>Men</span> in more educated and wealthier countries drink more than <span style='color:#17a0aa;'>women</span></span><br>") +
  facet_wrap(~gdp)+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#EEEEEE"),
        panel.grid.major = element_line(color = "#DDDDDD"), 
        panel.grid.minor = element_line(color = "#DDDDDD"),
        text = element_text(family = "Avenir", color = "#444444"),
        plot.title = element_text(size = 15),
        strip.text = element_text(size = 12, face = "italic", color = "#444444"),
        plot.margin = margin(10,20,10,10),
        plot.subtitle = element_textbox_simple(lineheight = 1, size = 11))



df %>%
  filter(year == 2019) %>% 
  ggplot(aes(color = gdp)) +
  geom_point(aes(x = female_edu, y = Female), show.legend = FALSE, size = 2.5, alpha = .6, shape = "U+2640") +
  geom_point(aes(x = male_edu, y = Male), show.legend = FALSE, size = 2.5, alpha = .6, shape = 2) +
  geom_smooth(aes(x = male_edu, group = year, y = Male), method = "lm", se = FALSE, show.legend = FALSE) +
  geom_smooth(aes(x = female_edu, group = year, y = Female), method = "lm", se = FALSE, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0,14,3))+
  scale_color_manual(values = c("#08415C","#998FC7","#AB28A2"))+
  labs(x = "\nYears of Education\n", y = "\nAlcohol Consumption (Liters)\n",
       color = "Log GDP", title = "\nAlcohol Consumption Increases Amongst More Educated and Wealthier Countries",
       subtitle = "<span><span style='font-size:12pt;font-family:Avenir;color=#000000;'>GDP Income Class: </span><span style = 'color:#08415C;'>**Lower Middle**</span>
           <span style = 'color:#998FC7;'>**Higher Middle**</span> <span style = 'color:#AB28A2;'>**High**</span></span>") +
  facet_wrap(~gdp)+
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#EEEEEE"),
        panel.grid.major = element_line(color = "#DDDDDD"), 
        panel.grid.minor = element_line(color = "#DDDDDD"),
        text = element_text(family = "Avenir", color = "#444444"),
        plot.title = element_text(size = 15),
        strip.text = element_text(size = 12, face = "italic"),
        plot.margin = margin(10,20,10,10),
        plot.subtitle = element_textbox_simple(lineheight = 1, size = 11)) +




# put correlation on line
# men drink more than women
# relationship between education and alcohol consumption varies by (stratified by) country income
# Gender specific relationships between education and alcohol consumption
# Shiny -------------------------------------------------------------------

ui <- fluidPage(
  selectInput("year", "Year",
              c("2010" = 2010,
                "2015" = 2015,
                "2019" = 2019)),
  plotOutput(outputId = "p")
)

server <- function(input, output, ...) {
  output$p <- renderPlot({ df %>%
      filter(year == input$year) %>% 
      ggplot(aes(x = edu, y = total, color = gdp, aes(group = year))) +
      geom_point(show.legend = FALSE) +
      geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
      scale_x_continuous(breaks = seq(0,14,3))+
      scale_color_manual(values = c("#170312","#998FC7","#AB28A2"))+
      labs(x = "\nYears of Education\n", y = "\nAlcohol Consumption (Liters)\n",
           color = "Log GDP", title = "\nAlcohol Consumption Increases as Education and GDP Levels Rise\n") +
      facet_wrap(~gdp)+
      theme_minimal() +
      theme(plot.background = element_rect(fill = "#EEEEEE"),
            panel.grid.major = element_line(color = "#DDDDDD"), 
            panel.grid.minor = element_line(color = "#DDDDDD"),
            text = element_text(face = "bold", family = "Avenir"),
            strip.text = element_text(size = 12, face = "italic"),
            plot.margin = margin(10,20,10,10)) })
  
}

# click open brower to see the shiny plot and interact
shinyApp(ui, server)
