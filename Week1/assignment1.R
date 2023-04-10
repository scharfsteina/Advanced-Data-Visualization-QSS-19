# Ava Scharfstein
# Assignment 1
# 4/3/23

library(tidyverse)
library(lubridate)
library(readxl)
#library(extrafont)

# Part 1 ------------------------------------------------------------------

# 1
(type <- c("dog", "cat", "elephant", "tiger", "grizzly bear", "snowy owl"))

# 2
(weight <- c(60, 8, 11000, 500, 600, 5))

# 3
(domestic <- c(rep("domestic",2), rep("wild",4)))

# 4
(animals <- data.frame(type,
                      weight,
                      domestic))

(animals <- animals %>% 
  # 5
  mutate(dom_fact = factor(domestic)) %>% 
  # 6
  mutate(weight_kg = weight*0.454))

# 7
(weird_zoo <- list(animals %>% filter(str_detect(domestic, "domestic")),
                  animals %>% filter(str_detect(domestic, "wild"))))

# 8
for (i in 1:length(weird_zoo)) {
  weird_zoo[[i]] <- weird_zoo[[i]] %>% 
    mutate(weight = ifelse(type == "tiger", 525, weight),
           weight_kg = ifelse(type == "tiger", 525*.454, weight))
}
weird_zoo

# Part 2 ------------------------------------------------------------------

# 1
dim(diamonds)
# 53940 x 10

# 2
class(diamonds$color)
typeof(diamonds$color)

# 3 
data_dir <- "Week1/adv_hw1_data/"
(wiid <- read_excel(paste0(data_dir,"wiid_2022_data/WIID_30JUN2022_0.xlsx")))

head(wiid)
tail(wiid)
glimpse(wiid)

# 4
wiid %>% 
  filter(region_un=="Africa") %>% 
  summarise(country, region_un, gini) %>% 
  group_by(country) %>% 
  slice_max(gini) %>% 
  ungroup()

# 5
wiid %>% 
  filter(str_detect(region_un, "America")) %>% 
  summarise(country, region_un, region_un_sub, gini) %>% 
  group_by(region_un_sub) %>% 
  slice_min(gini)

# 6
wiid <- wiid %>% 
  mutate(european = ifelse(region_un == "Europe", TRUE, FALSE))

wiid %>% 
  filter(oecd == "OECD") %>% 
  group_by(european) %>% 
  summarise(avg_gini = mean(gini, na.rm = TRUE))


# Part 3 ------------------------------------------------------------------

chi_town <- read_csv(paste0(data_dir, "chi_town.csv"))

# 1 
chi_town %>% 
  filter(year(DATE) >= 1960,
         year(DATE) <= 1980) %>% 
  mutate(TAVG = ifelse(is.na(TAVG), 
                       (TMAX+TMIN)/2, TAVG)) %>% 
  ggplot(aes(x = DATE, y = TAVG)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Day", y = "Average Temperature", title = "Average Temperature by Day for 1960-1980")
  
# 2 
chi_town %>% 
  group_by(month = month(DATE),
           day = day(DATE)) %>% 
  summarise(TMAX = mean(TMAX, na.rm = TRUE),
            TMIN = mean(TMIN, na.rm = TRUE)) %>% 
  # year doesn't matter it's just for the formatting
  mutate(date = as.Date(paste(2023, month,day, sep="-"))) %>% 
  ggplot(aes(x = date)) +
  geom_ribbon(aes(ymax = TMAX, ymin = TMIN), 
              fill = "blue", color = "dark blue", alpha = .5) +
  theme_minimal() +
  scale_x_date(breaks = "1 month",
               date_labels = "%b") +
  labs(x = "Day", y = "Temperature",
       title = paste("Minimum and Maximum Temperatures Averaged from", year(min(chi_town$DATE)), "to", year(max(chi_town$DATE))))

# 3
wiid %>% 
  ggplot(aes(x = gini, y = log(gdp), color = region_un)) +
  geom_point(alpha = .3) +
  labs(x = "Gini", y = "Log GDP", color = "Continent", title = "Log GDP vs Gini By Continent")+
  theme_minimal() 

# 4
wiid %>% 
  mutate(africa = ifelse(region_un=="Africa", "African Country", "Other UN Country")) %>% 
  ggplot(aes(x = gini, y = log(gdp), color = africa)) +
  geom_point(alpha = .3) +
  labs(x = "Gini", y = "Log GDP", color = "",
       title = "Log GDP vs Gini For African and Non-African Countries")+
  theme_minimal() +
  theme(legend.position="bottom")

# 5 
wiid %>% 
  filter(region_un_sub == "Western Europe" | region_un_sub == "Southern Africa") %>% 
  group_by(region_un_sub) %>% 
  summarise(Q1 = mean(q1, na.rm = TRUE),
            Q2 = mean(q2, na.rm = TRUE),
            Q3 = mean(q3, na.rm = TRUE),
            Q4 = mean(q4, na.rm = TRUE),
            Q5 = mean(q5, na.rm = TRUE)) %>% 
  pivot_longer(cols = Q1:Q5, names_to = "quintiles", values_to = "averages") %>% 
  ggplot(aes(x = "", y = averages, fill = quintiles)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start=0) +
  facet_wrap(~ region_un_sub)+
  labs(fill = "Quintiles",
       title = "Average Percent Income for Quintiles in Western Europe and Southern Africa",
       x = "",
       y = "") +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

# 6 

# Following this tutorial for now, just to get me started: https://rpubs.com/mclaire19/ggplot2-custom-themes
theme_ava <- function (base_size = 11, base_family = "", base_line_size = base_size/22, 
          base_rect_size = base_size/22) {
  
  # Want to figure out how to integrate these colors below into the theme's color scheme
  dark <- "#070E31"
  light <- "#D9E4F3"
  grey <- "#707992"
  reg_dark <- "#0F146F"
  reg_med <- "#2957C2"
  reg_light <- "#89BCE4"
  contrast <- "#FF7930"
    
  
  theme_minimal() %+replace%
    theme(
      
      text = element_text(face = "plain",
                          colour = dark, size = base_size,
                          lineheight = 0.9,  hjust = 0.5,
                          vjust = 0.5, angle = 0, 
                          margin = margin(), debug = FALSE), 
      rect = element_rect(fill = light,
                          color = light,
                          size = 0.5, linetype = 1),
      
      # grid elements
      panel.grid.minor = element_blank(),    
      panel.grid.major = element_blank(),
      plot.background = element_rect(fill = light), 
      axis.ticks = element_blank(),          
      panel.background = element_rect(fill = light,
                                      color = "white",
                                      size = 0.5, 
                                      linetype = "solid"),
      
      # text elements
      plot.title = element_text(             
        size = 14,               
        hjust = 0,                
        vjust = 2,
        color = dark),             
      
      plot.subtitle = element_text(          
        size = 10),               
      
      plot.caption = element_text(           
        size = 9,                 
        hjust = 1),              
      
      axis.title = element_text(            
        size = 11),               
      
      axis.text = element_text(              
        size = 9),               
      
      axis.text.x = element_text(     
        margin=margin(5, b = 10)))
  
}

test <- data.frame(x = sample(1:30), 
                   y = sample(seq(60, 1, -2)), 
                   color = sample(rep(c(0:2),10)))

test %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(color = factor(color)),
             size = 6) +
  scale_color_manual(values = c(contrast, reg_dark, reg_light))+
  theme_ava() +
  labs(x = "X-Axis Label", y = "Y-Axis Label", color = "Type", title = "Sample Figure with Theme Ava 4/1/23")
