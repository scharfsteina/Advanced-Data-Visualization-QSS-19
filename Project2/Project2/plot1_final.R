library(tidyverse)
library(lubridate)
#library(ggmosaic)
library(extrafont)
library(cowplot)
library(scales)

#font_import()

# https://www.transtats.bts.gov/homepage.asp data source

format_table <- function(table, name) {
  table %>% 
    rename("airline_code" = ...1, airline = ...2) %>% 
    select(-length(table)) %>% 
    mutate_at(vars(-("airline")), as.numeric) %>% 
    pivot_longer(3:(length(table)-1), names_to = "month", values_to = name)
}

merge_data <- function(topic) {
  
  csvs <- stringr::str_c("data/carrier_", topic, 2018:2022,".csv")
  
  lst <- map(csvs, function(x) {
    year <- str_extract(x, "(\\d)+")
    x %>% 
      read_csv(., skip = 3) %>% 
      format_table(.,topic) %>% 
      mutate(year = year) 
  })
  
  bind_rows(lst) %>% 
    select(-airline_code)
  
}

flights <- merge_data("flights")
delays <- merge_data("delays")
cancelled <- merge_data("cancelled")

join_vars <- c("airline", "month", "year")

df <- flights %>% 
  full_join(delays, by = join_vars) %>% # Don't want to exclude 2021 just because it has missing data
  full_join(cancelled, by = join_vars) %>%
  filter(!str_detect(airline, "All Rows")) %>% 
  mutate(month = match(month, month.name),
         month = as.Date(paste(year, month, 1, sep = "-"), "%Y-%m-%d"),
         year = as.numeric(year),
         `On Time` = flights-(delays+cancelled)) %>%
  rename(Delayed = delays,
         Cancelled = cancelled) %>% 
  relocate(year, .after = month)

df_delta <- df %>% 
  filter(str_detect(airline, "Delta"))

pstacked <- df_delta %>% 
  pivot_longer(5:7, names_to = "status", values_to = "count") %>%
  mutate(status = factor(status, levels = c("On Time","Delayed", "Cancelled"))) %>% 
  ggplot(aes(x = month, fill = status, y = count)) +
  # shadow to make a building effect
  geom_bar(aes(x = month+13, y=count+225), 
           group = "ontime", fill="#000033", alpha = .5, width = 20, stat = "identity") +
  geom_col(aes(group = status), alpha = .8) +
  scale_x_date(breaks = c(as.Date("2018-01-01"),as.Date("2018-07-01"),
                          as.Date("2019-01-01"),as.Date("2019-07-01"),
                          as.Date("2020-01-01"),as.Date("2020-07-01"),
                          as.Date("2021-01-01"),as.Date("2021-07-01"),
                          as.Date("2022-01-01"),as.Date("2022-07-01"),
                          as.Date("2023-01-01")),
               date_labels = "%b %Y")+
  scale_fill_manual(values = c("#E3132C", "#9B1631", "#220000")) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3, sep = ""))+
  annotate("segment", x = as.Date("2018-09-01"), y = 94500, 
           xend = as.Date("2018-08-15"), yend = 90000, color = "white",
           arrow = arrow(type = "closed", length = unit(0.01, "npc")))+
  annotate("text", x = as.Date("2018-12-20"), y = 96000, label = "Peaks in the Summer",
           size = 3, color = "white", family = "Andale Mono") +
  annotate("segment", x = as.Date("2019-04-15"), y = 95700, 
           xend = as.Date("2019-07-15"), yend = 93500, color = "white",
           arrow = arrow(type = "closed", length = unit(0.01, "npc")))+
  annotate("text", x = as.Date("2020-04-01"), y = 55000, label = "COVID-19 Surge:\nMajor Cancellations",
           hjust = 0, size = 3, color = "white", family = "Andale Mono") +
  annotate("segment", x = as.Date("2020-06-15"), y = 50000, 
           xend = as.Date("2020-04-15"), yend = 30000, color = "white",
           arrow = arrow(type = "closed", length = unit(0.01, "npc")))+
  annotate("text", x = as.Date("2021-01-01"), y = 63000, label = "Post\nCOVID-19\nRevival",
           hjust = 0, size = 3, color = "white", family = "Andale Mono") +
  annotate("segment", x = as.Date("2021-03-15"), y = 65000,
           xend = as.Date("2021-07-15"), yend = 74000, color = "white",
           arrow = arrow(type = "closed", length = unit(0.01, "npc")))+
  theme_minimal() +
  labs(x = "\nMonth", y = "Number of Flights\n",
       title = "Seasonality of Delta Airline Flights 2018-2022",
       subtitle =  "Ontime, Delayed, and Cancelled Flights",
       caption = "Source: US Bureau of Transportation",
       fill = "") +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#003268"),
        text = element_text(family = "Andale Mono", face = "bold", color = "white"),
        axis.text = element_text(color = "white", size = 13),
        plot.margin = margin(30,50,20,50, unit = "pt"),
        title = element_text(size = 16))


ggdraw(pstacked) +
  draw_image(magick::image_read("data/airplane3.png"),
             scale = .2, x = .85, y = .93, hjust = 1, vjust = 1, halign = 1, valign = 1)
