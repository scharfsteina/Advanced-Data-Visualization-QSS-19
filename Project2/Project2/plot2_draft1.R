library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)

weather <- read_csv("data/state_weather_delays.csv", skip = 3) %>% 
  rename("state_abbr" = `...1`,
         "state" = `...2`)

df <- weather %>% 
  select(-length(weather)) %>%   
  mutate_at(3:(length(weather)-1), as.numeric) %>% 
  pivot_longer(3:(length(weather)-1), names_to = "month", values_to = "weather_delays") %>% 
  mutate(weather_delays = ifelse(weather_delays == 0, NA, weather_delays),
         month = factor(month, levels = c("January","February","March","April",
                                          "May", "June", "July","August","September", "October", "November",
                                          "December"))) %>% 
  filter(!str_detect(state_abbr, "All Rows|TT|VI"))
  #mutate(state = tolower(state))
  
ggplot(df, aes(month, state_abbr, fill= weather_delays)) + 
  geom_tile()

# total <- df %>% 
#   mutate(season = case_when(
#     month == "December" | month == "January" | month == "February" ~ "Winter",  
#     month == "March" | month == "April" | month == "May" ~ "Spring",
#     month == "June" | month == "July" | month == "August" ~ "Summer",
#     month == "September" | month == "October" | month == "November" ~ "Fall")) %>% 
#   group_by(state, season) %>% 
#   summarise(n = sum(weather_delays))

states <- map_data("state") %>% inner_join(total, by = c("region" = "state"))


states

ggplot(data=states, aes(x=long, y=lat, fill=n, group=group)) + 
  geom_polygon() + 
  theme_void()+
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3) +
  facet_wrap(~season)

# library(sf)
# 
# states <- read_sf("resources/us_map/US-State-Boundaries-Census-2014.shp")


ts <- read_csv("data/carrier_flights_ts.csv", skip = 1) %>% 
  rename(flights = `Sum(FLIGHTS)`,
         airline = Description,
         airline_code = OP_UNIQUE_CARRIER,
         year = Year) %>% 
  filter(!str_detect(airline_code, "All Rows"))

top <- ts %>% filter(str_detect(airline, "Southwest") | str_detect(airline, "Delta"))

top %>% 
  ggplot(aes(x = year, y = flights, group = airline, color = airline)) +
  geom_line()

load_data <- function(year) {
  read_csv(paste0("data/distance",year,".csv"), skip = 3) %>% 
    select(-1) %>% 
    rename(distance = `...2`,
           total = `All Columns (including those not displayed)`,
           one_way = `No Round Trip Itinerary`,
           round_trip = `Round Trip Itinerary`) %>% 
    filter(!str_detect(distance, "All Rows")) %>% 
    mutate(year = year,
           distance = str_replace(distance, " Miles", ""),
           distance = str_replace(distance, "Less Than 500", "[0,500)"),
           distance = str_replace(distance, "500-999", "[500,1000)"),
           distance = str_replace(distance, "1000-1499", "[1000,1500)"),
           distance = str_replace(distance, "1500-1999", "[1500,2000)"),
           distance = str_replace(distance, "2000-2499", "[2000,2500)"),
           distance = str_replace(distance, "2500-2999", "[2500,3000)"),
           distance = str_replace(distance, "3000-3499", "[3000,3500)"),
           distance = str_replace(distance, "3500-3999", "[3500,4000)"),
           distance = str_replace(distance, "4000-4499", "[4000,4500)"),
           distance = str_replace(distance, "4500-4999", "[4500,5000)"),
           distance = str_replace(distance, "5000-5499|5500-5999", "[5000,6000)"),
           distance = str_replace(distance, "6000-6499|6500-6999", "[6000,7000)"),
           distance = str_replace(distance, "7000-7499|7500-7999", "[7000,8000)"),
           distance = str_replace(distance, "8000-8499|8500-8999", "[8000,9000)"),
           distance = str_replace(distance, "9000-9499|9500-9999", "[9000,10000)"),
           distance = str_replace(distance, "10000-10499|10500-10999", "[10000,11000)"),
           distance = str_replace(distance, "11000-11499|11500-11999", "[11000,12000)"),
           distance = str_replace(distance, "12000 and Greater", "12000+")) %>% 
    mutate(distance = factor(distance, levels = c("[0,500)","[500,1000)","[1000,1500)","[1500,2000)",
                                                  "[2000,2500)","[2500,3000)","[3000,3500)","[3500,4000)",
                                                  "[4000,4500)","[4500,5000)","[5000,6000)","[6000,7000)",
                                                  "[7000,8000)","[8000,9000)","[9000,10000)","[10000,11000)",
                                                  "[11000,12000)","12000+"))) %>% 
    group_by(distance) %>% 
    mutate(total = sum(total),
           one_way = sum(one_way),
           round_trip = sum(round_trip)) %>% 
    ungroup()
   
    
}

ninetythree <- load_data(1993) #%>% 
  #mutate(total = -total)

twentytwo <- load_data(2022)

df <- bind_rows(ninetythree, twentytwo) %>% 
  mutate(year = factor(year))

#range <- range(df$total)

# ecdf = cumulative distribution 
# normalize it, percentage of travelers

ggplot(df,
       aes(x = total,
           y = distance,
           fill = year)) +
  geom_col() +
  theme_minimal() #+
  # scale_x_continuous(breaks  = seq(round(range[1],-5), round(range[2],-5), by = 1500000),
  #                    labels = abs(seq(round(range[1],-5), round(range[2],-5), by = 1500000)))
  # 


ggplot(mapping = aes(x = distance,
                     y = total)) +
  geom_col(data = twentytwo,
           alpha = .3, fill = "blue")+
  geom_smooth(data = twentytwo, color = "blue") +
  geom_col(data = ninetythree,
           alpha = .3, fill = "red")+
  geom_smooth(data = ninetythree, color = "red") +
  theme_minimal()
