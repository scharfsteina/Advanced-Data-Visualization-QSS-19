#library(survivoR)
library(tidyverse)
library(countrycode)
library(ggimage)
library(cowplot)
library(ggridges)
#library(ggwaffle)
library(waffle)
library(extrafont)

extrafont::loadfonts(quiet = TRUE)
extrafont::font_import()
fonts()[grep("Awesome", fonts())]

extrafont::fonttable() %>% 
  as_tibble() %>% 
  filter(grepl("Awesom", FamilyName)) %>% 
  select(afmfile, FullName, FamilyName, FontName)

fa_list()

# jury_votes
# vote_history
# viewers %>% View()
# 
# viewers %>% 
#   group_by(season) %>% 
#   summarise(imdb_rating, episode) %>% 
#   ggplot(aes(x = episode, y = imdb_rating, color = season, group = season)) +
#   geom_line()
# 
# vote_history %>% 
#   group_by(season, episode) %>% 
#   summarise()

drinks <- read_csv("drinks_csv.csv") %>% 
  mutate(continent = countrycode(sourcevar = country,
                     origin = "country.name",
                     destination = "continent"),
         country = stringr::str_to_title(country))

drinks_by_type <- drinks %>% 
  select(-total_litres_of_pure_alcohol) %>%
  rename(beer = beer_servings,
        spirit = spirit_servings,
        wine = wine_servings) %>% 
  pivot_longer(2:4, names_to = "type", values_to = "servings") %>% 
  mutate(image = paste0(type,".png"),
         type = factor(type))

drinks_by_type %>% 
  arrange(-servings)

df_country <- drinks_by_type %>% 
  filter(country == "Grenada")

drinks_by_type %>% 
  ggplot(aes(x = type, y = servings)) +
  #geom_point() +
  geom_image(aes(image = image), by = "height", size = .07, asp = 1.3)+ 
  theme_minimal() +
  labs(x = "Type of Alcohol", y = "Annual Servings Per Person",
       title = "Alcohol Consumption in Grenada, 2010") +
  scale_y_continuous(limits = c(0,500), breaks = seq(0,500, 50)) +
  scale_x_discrete(labels = c("\n\nBeer","\n\nSpirits", "\n\nWine"))

# df_country %>% filter(type == "beer") %>% pull(servings)/500
ggdraw(p_country) +
  # draw_image(magick::image_read("beer.png"), width = 1, height = 1, x = .46, y = 1, 
  #            hjust = 1, vjust = 1, halign = 1, valign = .15) +
  # draw_image(magick::image_read("wine.png"), width = 1, height = .75, x = .76, y = 1, 
  #            hjust = 1, vjust = 1, halign = 1, valign = .15) +
  draw_image(magick::image_read("spirit.png"), width = 1, height = .75, x = 1, y = 1)

drinks %>% 
  arrange(-total_litres_of_pure_alcohol)


who <- read_csv("who.csv")
glimpse(who)
(df_who <- who %>% 
  select(ParentLocationCode, ParentLocation, SpatialDimValueCode, Location, Period, Dim1, FactValueNumeric, FactValueNumericLow, FactValueNumericHigh))

colnames(df_who) <- c("region_abbr","region", "country_abbr", "country","year", "sex", "liters_per_capita", "liters_low", "liters_high")
glimpse(df_who)

df_who_compiled <- df_who %>% 
  select(-liters_low, -liters_high) %>% 
  pivot_wider(names_from = sex, values_from = liters_per_capita) %>% 
  rename(total = `Both sexes`)

df_who_sexes <- df_who %>% 
  filter(str_detect(sex, "Female|Male")) %>%
  mutate(sex = str_replace(sex, "Female", "Women"),
         sex = str_replace(sex, "Male", "Men")) %>% 
  group_by(sex, year) %>% 
  mutate(average = mean(liters_per_capita))


ggplot(df_who_compiled, aes(x = year, y = total)) +
  geom_line(aes(group = country, color = region), alpha = .2) +
  facet_wrap(~region)+
  # geom_point(aes(x = year, y = liters_per_capita), alpha = .2, color = "red")+
  # geom_point(data = df_who_male,
  #           aes(x = year, y = liters_per_capita), alpha = .2, color = "blue")+
  theme_minimal()

df_who %>% 
  filter(is.na(liters_per_capita))

df_who_sexes$year %>% unique()


ggplot() +
  # geom_vline(aes(xintercept = average, color = sex,), 
  #            linetype = "dashed", show.legend = FALSE) +
  geom_density_ridges(data = df_who_sexes %>% filter(region == "South-East Asia"),
                      aes(x = liters_per_capita, 
                          y = year, 
                          fill = sex,
                          group = year), 
                      rel_min_height = 0.01, alpha = .6, show.legend = FALSE,
                      quantile_lines = T, quantile_fun = mean,#quantiles = 2,
                      color = "#222222", fill = "red") +
  geom_density_ridges(data = df_who_sexes %>% filter(region == "Eastern Mediterranean"),
                      aes(x = liters_per_capita, 
                          y = year, 
                          fill = sex,
                          group = year), 
                      rel_min_height = 0.01, alpha = .6, show.legend = FALSE,
                      quantile_lines = T, quantile_fun = mean,#quantiles = 2,
                      color = "#222222", fill = "blue") +
  scale_y_continuous(breaks = c(df_who$year %>% unique()))+
  #scale_fill_manual(values = c("#fd7e22","#32adbb")) +
  facet_wrap(~sex, scales = "free_x") + 
  labs(x = "\nLiters Consumed Per Capita\n", y = "\nYear\n", 
       title = "\nDrinking Patterns Have Barely Changed In The Past 20 Years\n") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#999999"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "DIN Alternate", face = "bold", color = "white",size = 16),
        axis.text = element_text(color = "white", size = 11),
        strip.text = element_text(color = "white", size = 11.5))
  

ggplot(df_who_compiled) +
  geom_density_ridges(aes(x = total, 
                          y = year, 
                          group = year), 
                      rel_min_height = 0.01, alpha = .6, show.legend = FALSE,
                      quantile_lines = T, quantile_fun = mean,
                      color = "#222222",
                      fill = "#005500") +
  scale_y_continuous(breaks = c(df_who$year %>% unique()))+
  #facet_wrap(~sex, scales = "free_x") + 
  labs(x = "\nLiters Consumed Per Capita\n", y = "\nYear\n", 
       title = "\nDrinking Patterns Have Barely Changed In The Past 20 Years\n") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#999999"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(family = "DIN Alternate", face = "bold", color = "white",size = 16),
        axis.text = element_text(color = "white", size = 11),
        strip.text = element_text(color = "white", size = 11.5))

df <- data.frame(value = c(8,20), type = c("wine","beer"))

ggplot(df, aes(group = type, values = value, fill = type)) +
  geom_waffle()

df_who_compiled %>% 
  group_by(country) %>% 
  mutate(model = lm(total~year)[2])
