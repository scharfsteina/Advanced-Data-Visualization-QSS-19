library(tidyverse)
library(lubridate)
library(ggmosaic)


flights <- read_csv("data/carrier_flights_by_month2022.csv", skip = 3)
delays <- read_csv("data/carrier_delay_by_month2022.csv", skip = 3)
cancellations <- read_csv("data/carrier_cancellations_by_month2022.csv", skip = 3)


format_table <- function(table, name) {
  table %>% 
    rename("airline_code" = ...1, airline = ...2) %>% 
    select(-length(table)) %>% 
    pivot_longer(3:(length(table)-1), names_to = "month", values_to = name)
}

join_vars <- c("airline_code","airline", "month")

df2022 <- format_table(flights, "flights") %>% 
  inner_join(format_table(delays, "delays"), by = join_vars) %>% 
  inner_join(format_table(cancellations, "cancellations"), by = join_vars) %>% 
  filter(airline_code != "All Rows") %>% 
  mutate(month = match(month, month.name),
         month = as.Date(paste(2022, month, 1, sep = "-"), "%Y-%m-%d"))

top <- df2022 %>% 
  group_by(airline_code, airline) %>% 
  summarise(flights = sum(flights)) %>% 
  ungroup() %>% 
  slice_max(flights, n = 2) %>% 
  pull(airline_code)

bottom <- df2022 %>% 
  group_by(airline_code, airline) %>% 
  summarise(flights = sum(flights)) %>% 
  ungroup() %>% 
  slice_min(flights, n = 2) %>% 
  pull(airline_code)

df2022 %>% 
  filter(airline_code %in% top) %>% 
  group_by(airline_code, airline) %>% 
  ggplot(aes(x = month, group = airline_code, y = delays, color = airline_code)) +
  geom_line()

df2022 %>% 
  filter(airline_code %in% top) %>% 
  group_by(airline_code, airline) %>% 
  ggplot(aes(x = month, group = airline_code, y = delays, fill = airline_code)) +
  geom_area()

df2022 %>% 
  group_by(airline_code, airline) %>%
  summarise(flights = sum(flights),
            delays = sum(delays),
            cancellations = sum(cancellations)) %>% 
  mutate(per_cancelled = 100*cancellations/flights,
         per_delayed = 100*delays/flights) %>% 
  arrange(-per_delayed)

df2022 %>% 
  filter(airline_code %in% top) %>% 
  mutate(airline = factor(airline)) %>% 
  ggplot(aes(x = month, fill = airline, color = airline)) +
  geom_area(aes(y = flights), alpha = .4) +
  geom_area(aes(y = delays), alpha = .5) +
  geom_area(aes(y = cancellations), alpha = .7) +
  geom_line(aes(y = flights), alpha = .3, linetype = "dashed") +
  geom_line(aes(y = delays), alpha = .3, linetype = "dashed") +
  geom_line(aes(y = cancellations), alpha = .3, linetype = "dashed") +
  facet_wrap(~airline) +
  theme_minimal() +
  labs(x = "Month", y = "Number of Flights",
       title = "Proportion of Flights Ontime, Delayed, and Cancelled By Airline in 2022") +
  theme(legend.position = "none")


df2022 %>% 
  filter(airline_code %in% bottom)%>% 
  mutate(airline = factor(airline)) %>% 
  ggplot(aes(x = month, fill = airline, color = airline)) +
  geom_area(aes(y = flights), alpha = .4) +
  geom_area(aes(y = delays), alpha = .5) +
  geom_area(aes(y = cancellations), alpha = .7) +
  geom_line(aes(y = flights), alpha = .3, linetype = "dashed") +
  geom_line(aes(y = delays), alpha = .3, linetype = "dashed") +
  geom_line(aes(y = cancellations), alpha = .3, linetype = "dashed") +
  facet_wrap(~airline) +
  theme_minimal() +
  labs(x = "Month", y = "Number of Flights",
       title = "Proportion of Flights Ontime, Delayed, and Cancelled By Airline in 2022") +
  theme(legend.position = "none")


mosaicdf <- df2022 %>% 
  mutate(airline_code = fct_reorder(airline_code, flights)) %>% 
  group_by(airline_code) %>% 
  summarise(airline,
            Delayed = delays,
            Cancelled = cancellations,
            OnTime = flights-(Delayed+Cancelled)) %>% 
  distinct() %>% 
  pivot_longer(3:5, names_to = "flight_status", values_to = "count") %>%
  mutate(flight_status = factor(flight_status)) %>% 
  uncount(count)

mosaic_perdf <- df2022 %>% 
  mutate(airline_code = fct_reorder(airline_code, flights)) %>% 
  group_by(airline_code) %>% 
  summarise(airline,
            Delayed = round(100*delays/sum(flights)),
            Cancelled = round(100*cancellations/sum(flights)),
            OnTime = 100-(Delayed+Cancelled)) %>% 
  distinct() %>% 
  pivot_longer(3:5, names_to = "flight_status", values_to = "count") %>%
  mutate(flight_status = factor(flight_status)) %>% 
  uncount(count)

# add more data
# individual airline profile

ggplot(mosaicdf) +
  geom_mosaic(aes(x = product(airline_code),
                  fill = flight_status),
              color = "white") +
  labs(title = "",
       subtitle = "Domestic Flight Status By Airline in 2022",
       fill = "Flight Status",
       x = "Airline",
       y = "Number of Flights") +
  scale_fill_manual(values = c("#FB5607", "#5BC0BE","#1C5D99"),
                    guide = guide_legend(reverse = TRUE)) +
  theme_minimal()+
  theme(line = element_blank(), rect = element_blank(), 
        #axis.title = element_blank(), 
        axis.text.y = element_blank(), #axis.text.x = element_text(), 
        axis.ticks.length.x.top = NULL,
        axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
        axis.ticks.length.y.right = NULL, legend.box = NULL, 
        legend.key.size = unit(1.2, "lines"), legend.position = "right", 
        panel.ontop = FALSE) #+ 
  #coord_flip()


df2022 %>% 
  filter(airline_code %in% top | airline_code %in% bottom) %>% 
  mutate(airline_code = factor(airline_code))

