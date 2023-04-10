library(tidyverse)
library(colorBlindness)
library(scales)
library(devtools)
library(readxl)
library(ggdist)
library(ggridges)
library(ggtext)
library(ggwaffle)
library(ggstream)
library(lubridate)
library(plotrix)


# Part 2 ------------------------------------------------------------------

# 1 -- Sequential Palette
p1 <- colorRampPalette(c("#061e53", "#317f8c"))
p1(9)
show_col(p1(9))

# 2 -- Diverging Palette
p2 <- colorRampPalette(c("#317f8c","#ffffff", "#75c25f"))
p2(9)
show_col(p2(9))

# 3 -- Qualitative Color Palette
(p3 <- c("#b9dae2", "#317F8C", "#061e53", "#195200", "#66b251", "#712a4d"))

plot1 <- mtcars %>%
  ggplot(aes(x = factor(carb), fill = factor(carb))) +
  geom_bar() +
  scale_fill_manual(values = p3) +
  theme_minimal() 

colorBlindness::cvdPlot(plot1)

# 4 
rect <- data.frame(x1 = 1.5, x2 = 4, y1 = 2, y2 = 5); rect
rect1 <- data.frame(x1 = 4, x2 = 6.5, y1 = 2, y2 = 5); rect1

# Triangle for a geom_polygon. 

(triangle <- data.frame(x = c(2, 6, 5),
                        y = c(4.5, 4, 3)))

left <-  data.frame(x = c(2, 4, 4),
                      y = c(4.5, 4.25, 3.5))
right <- data.frame(x = c(4, 6, 5, 4),
                    y = c(4.25, 4, 3, 3.5))

ggplot(data= rect) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#317F8C") +
  geom_rect(data = rect1, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#66b251") +
  #geom_polygon(data = triangle, aes(x = x, y = y), alpha = .5)
  geom_polygon(data = left, aes(x = x, y = y), fill = "#315a62") +
  geom_polygon(data = right, aes(x = x, y = y), fill = "#4d7641") +
  theme_void()
  

# 5 -- 

data_dir <- "Week1/adv_hw1_data/"
(wiid <- read_excel(paste0(data_dir,"wiid_2022_data/WIID_30JUN2022_0.xlsx")))

plot2 <- wiid %>% 
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

# I would consider this figure very unaccessible
# For Deuteranopia and Protonopia, it is hard to distinguish between the hues,
# especially between the pink/blue colors (which display as purple). The other colors
# are still pretty close in hue, but could be distinguishable if I looked hard.
# For Desaturated, the values are essentially indistinguishable
# It would be very difficult for someone with color blindness
# To extract information from this figure
colorBlindness::cvdPlot(plot2)


# Part 3 ------------------------------------------------------------------

# 1 -- Ridge Plot
chi_town <- read_csv(paste0(data_dir, "chi_town.csv")) %>% 
  mutate(TAVG = ifelse(is.na(TAVG),  # not sure if this is what he means by distribution of temperatures
                (TMAX+TMIN)/2, TAVG))

chi_town %>% filter(year(DATE) >= 1960,
                    year(DATE) < 2020,
                    month(DATE) == 6) %>% 
  summarise(TAVG, decade = paste0(as.character(year(DATE) - year(DATE)%%10),"s")) %>% 
  ggplot(aes(x = TAVG, y = decade, fill = decade)) +
  geom_density_ridges(color = "white") +
  scale_fill_manual(values = p3)+
  labs(x = "Mean Temperature", y = "Decade", fill = "Decade",
       title = "Distribution of Mean Daily Temperatures in June Per Decade",
       subtitle = "<span>This figure compares the distribution of mean daily temperatures in Chicago during June across the
       <span style = 'color:#b9dae2;'>**1960s**</span>, <span style = 'color:#317F8C;'>**1970s**</span>, 
       <span style = 'color:#061e53;'>**1980s**</span>, <span style = 'color:#195200;'>**1990s**</span>, 
       <span style = 'color:#66b251;'>**2000s**</span>, & <span style = 'color:#712a4d;'>**2010s**</span>.</span>") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        plot.subtitle = element_textbox_simple(
          size = 11,
          lineheight = 1,
          padding = margin(5.5, 0, 5.5, 0)))

# 2 - Raincloud plot

chi_town %>% filter(year(DATE) >= 1960,
                    year(DATE) < 2020,
                    month(DATE) == 6) %>% 
  group_by(decade = paste0(as.character(year(DATE) - year(DATE)%%10),"s")) %>%
  summarise(TAVG) %>% 
  ggplot(aes(x = TAVG, y = decade)) +
  geom_violin(adjust = .5,  # geom_distribution
                       .width = c(0.5, .1), 
                       point_color = NA,
              aes(fill = decade), alpha = .5) + 
  geom_jitter(aes(color = decade), alpha = .7)+
  geom_boxplot(aes(fill = decade), alpha = .3) +
  scale_fill_manual(values = p3)+
  scale_color_manual(values = p3)+
  labs(x = "Mean Temperature", y = "Decade", fill = "Decade",
       title = "Distribution of Mean Daily Temperatures in June Per Decade") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")


# 3 - Waffle Plot
chi_town %>% 
  filter((year(DATE) >= 1960 & year(DATE) <= 1964) | (year(DATE) >= 2016 & year(DATE) <= 2020)) %>%
  group_by(month = month(DATE)) %>% 
  summarise(above_avg = mean(TAVG) < TAVG,
            period = ifelse(year(DATE) <= 1964, "1960-1964", "2016-2020")) %>% 
  ungroup() %>% 
  count(above_avg, period) %>% 
  group_by(period) %>% 
  mutate(n = round(100*n/sum(n))) %>% 
  uncount(n) %>% 
  mutate(y = rep(1:10, 10),
         x = rep(1:10, each = 10),
         above_avg = ifelse(above_avg, "Above", "Below")) %>% 
  ggplot(aes(x = x, y = y, fill = above_avg)) +
  geom_waffle() +
  facet_wrap(~ period) +
  theme_minimal() +
  scale_fill_manual(values = c(p3[6],p3[2])) +
  labs(x = "", y = "", fill = "Relation to Average Monthly Temperature?",
       title = "Percent of Days Above and Below Average Monthly Temperature") +
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 12, face = "bold"))

## Stream plot
wiid %>% 
  filter(year >= 1960 & year <= 2010 & str_detect(region_un, "America")) %>% 
  group_by(region_un_sub, year) %>% 
  summarise(q_mean = mean(c(q1,q2,q3,q4,q5), na.rm = T)) %>% 
  drop_na() %>% 
  ggplot(aes(x = year, y = q_mean, fill = region_un_sub)) +
  geom_stream() +
  theme_minimal() +
  labs(fill = "Americas UN Subregions",
       x = "Year",
       y = "Average Quintile Values",
       title = "Average Quintile Values Per Year Across Americas UN Subregions") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = p3[2:5])


# Part 4: Recreation ------------------------------------------------------

library(extrafont)

dubois <- read_csv("Week2/dubois_data.csv") %>% 
  mutate(Group = ifelse(str_detect(Group, "N"), "Blacks", Group),
         Label = paste0(as.character(Percentage), "%")) %>%
  # Adding fake data
  add_row(Group = "Whites", Occupation = "Other1", Percentage = round(100/3), Label = "") %>% 
  add_row(Group = "Whites", Occupation = "Other2", Percentage = round(100/3), Label = "") %>% 
  add_row(Group = "Blacks", Occupation = "Other1", Percentage = round(100/3), Label = "") %>%
  add_row(Group = "Blacks", Occupation = "Other2", Percentage = round(100/3), Label = "") %>%
  mutate(Occupation = factor(Occupation, ordered = TRUE,
                             levels = c("Other1",
                                        "Agriculture, Fisheries and Mining",
                                        "Domestic and Personal Service",
                                        "Manufacturing and Mechanical Industries",
                                        "Trade and Transportation",
                                        "Professions",
                                        "Other2")),
         # Adapted from: https://r-charts.com/part-whole/pie-chart-labels-outside-ggplot2/
         csum = rev(cumsum(rev(Percentage))), 
         pos = Percentage/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percentage/2, pos),
         size = Percentage/20) %>% 
  arrange(Occupation)

bc <- "#e9ddce"
fills <- c(bc, "red", "#f5cc38","#4a6bff","#d1d1b8","#BD9A7A", bc)

#png(file="Week2/dubois.jpg")

dubois %>% 
  ggplot(aes(x="", y=Percentage, fill=Occupation, group = Group)) +
  geom_col(width=1) +
  geom_text(aes(label = Label, size = size, x = 1.3), 
            position = position_stack(vjust = 0.5),
            color = "#423d35", family = "Courier New") +
  coord_polar("y", start=-80, clip = "off") +
  scale_fill_manual(values = fills) +
  labs(title="\nOCCUPATIONS OF BLACKS AND WHITES IN GEORGIA",
       caption = "Source: W.E.B. Du Bois\nAva Scharfstein, April 2023")+
  scale_y_continuous(breaks = dubois$pos, labels = dubois$Occupation) +
  theme(panel.background = element_rect(fill = bc),
        plot.background = element_rect(fill = bc),
        legend.background = element_rect(fill = bc),
        legend.title = element_blank(),
        legend.position="none",
        plot.title = element_text(color = "#423d35", size=17, family="Courier New", face = "bold",),
        axis.title.x= element_blank(),
        axis.title.y= element_blank(),
        axis.ticks = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())

# https://bookdown.org/rdpeng/RProgDA/the-grid-package.html
# https://www.rdocumentation.org/packages/grid/versions/3.6.2/topics/grid.text
library(grid)

grid.draw(circleGrob(x = 0.2, y = 0.53, r = 0.02,
                     gp = gpar(fill = fills[2], color = bc)))
grid.text("AGRICULTURE, FISHERIES\nAND MINING", x = .23, y = .53, 
          gp = gpar(color = bc, fontsize = 6,
                    fontfamily = "Courier New",
                    align = "left"),
          just = "left")

grid.draw(circleGrob(x = 0.2, y = 0.45, r = 0.02,
                     gp = gpar(fill = fills[4], color = bc)))
grid.text("MANUFACTURING AND\nMECHANICAL INDUSTRIES", x = .23, y = .45, 
          gp = gpar(color = bc, fontsize = 6,
                    fontfamily = "Courier New"),
          just = "left")

grid.draw(circleGrob(x = 0.8, y = 0.53, r = 0.02,
                     gp = gpar(fill = fills[3], color = bc)))
grid.text("DOMESTIC AND\nPERSONAL SERVICE", x = .77, y = .53, 
          gp = gpar(color = bc, fontsize = 6,
                    fontfamily = "Courier New"),
          just = "right")

grid.draw(circleGrob(x = 0.8, y = 0.45, r = 0.02,
                     gp = gpar(fill = fills[6], color = bc)))
grid.text("PROFESSIONS", x = .77, y = .45, 
          gp = gpar(color = bc, fontsize = 6,
                    fontfamily = "Courier New"),
          just = "right")

grid.draw(circleGrob(x = 0.8, y = 0.37, r = 0.02,
                     gp = gpar(fill = fills[5], color = bc)))
grid.text("TRADE AND\nTRANSPORTATION", x = .77, y = .37, 
          gp = gpar(color = bc, fontsize = 6,
                    fontfamily = "Courier New"),
          just = "right")


#dev.off()

  
