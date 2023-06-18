# ADV: MORE PLOTLY PLOTS.

library(tidyverse)
library(plotly)
library(readxl)

wiid <- read_excel(file.choose())


# Plotly in the quickest possible nutshell. 

# Data + traces + layout. 
# Layout includes lots of specification. 
# Of note: updatemenus for buttons and dropdowns. 
# Many of the arguments/attributes show up in the form of a list. 

###########################

# We can also add geometries, called 'traces' in plotly. 

objects('package:plotly') # Check all the add_trace options. 

# We can thus use the same basic 'aesthetics' underneath and build. 

wiid <- read_excel(file.choose()) 
glimpse(wiid)

# Start with a basic WIID scatterplot. 
# Note: no layout has been specified, so we are in the default. 

wiid %>%
  group_by(country, year, region_un, region_un_sub) %>%
  summarize(mean_gini = mean(gini, na.rm=TRUE)) %>%
  filter(year > 1939) %>%
  plot_ly(x =~year, 
          y=~mean_gini) 

# This produces the same basic plot as: 

wiid_plot <- wiid %>%
  group_by(country, year, region_un, region_un_sub) %>%
  summarize(mean_gini = mean(gini, na.rm=TRUE)) %>%
  filter(year > 1939) %>%
  ggplot(aes(x = year, y = mean_gini, color = region_un)) +
  geom_point() +
  theme_minimal()

# We can quickly wrap a ggplotly.
# Most of the plotly functions can be applied to a ggplotly plot. 
# But you cannot control all features. 
# For this feature to be maximized, do a lot of smart data work underneath.

ggplotly(wiid_plot) %>%
  layout(hovermode = "x unified") # 

# We can add to the ggplotly function as well. 

ggplotly(wiid_plot) %>%
  layout(hovermode = "x unified") # Many opions in the layout. 


# Alternatively, you can use the plot_ly function. 

wiid %>%
  group_by(country, year, region_un, region_un_sub) %>%
  summarize(mean_gini = mean(gini, na.rm=TRUE)) %>%
  filter(year > 1939) %>%
  plot_ly(x =~year, 
          y=~mean_gini, color=~region_un, type = "scatter", mode="markers") 

# To add details, many arguments/attributes are fed into plotly as lists. 

# As always, control your colors!

rc_pal <- colorRampPalette(c("#5d3bb5", "#2739d5", "#b7e2a5", "#f0da99", "#db841c"))

# Text labels for hover information can be controlled with HTML tags. 


wiid %>%
  group_by(country, year, region_un, region_un_sub) %>%
  summarize(mean_gini = mean(gini, na.rm=TRUE)) %>%
  filter(year > 1939) %>%
  plot_ly(x =~year, 
          y=~mean_gini,
          opacity = 0.7,
          color=~region_un,
          colors = rc_pal(5),
          hoverinfo=paste('text', 'y'),
          text =~country, 
          type='scatter',
          mode='markers') %>%
  layout(title = "WIID Plot, Updated",
         font = list(family = '',
                     size = 14, 
                     color = "#7c7c7c"),
         plot_bgcolor = "white",
         xaxis = list(title = "Year"),
         yaxis = list(title = "Average Gini Score"),
         legend = list(orientation = 'h'))


#### 
# Solid, simple WIID example, with a partially transparent hover box. 

# Do some data work ahead of time!

wiid_sub <- wiid %>% 
  group_by(country, year, region_un, region_un_sub) %>%
  summarize(meangini = mean(gini, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(region_un, year) %>%
  mutate(region_mean = mean(meangini, na.rm=TRUE)) %>%
  filter(year > 1949) %>%
  ungroup()

wiid_regions <- wiid %>% 
  group_by(country, year, region_un, region_un_sub) %>%
  summarize(meangini = mean(gini, na.rm=TRUE)) %>%
  ungroup() %>%
  group_by(region_un, year) %>%
  summarize(region_mean = mean(meangini, na.rm=TRUE)) %>%
  filter(year > 1949) %>%
  ungroup()

wiid_regions
glimpse(wiid_sub)

rc_pal <- colorRampPalette(c("#800020", "#6fa8dc", "#ffd966"))
rc_pal(5)

fig <- plot_ly(wiid_sub, 
               x = ~year, 
               y = ~meangini,
               color = ~region_un,
               colors = rc_pal(5),
               type = "scatter",
               mode = "markers",
               hoverinfo = "none",
               #text = ~paste0(year, ": ", "(",country, ")"),
               opacity = .4)

plot1 <- fig %>% 
  layout(
    font = list(
      family = "American Typewriter"
    ),
    xaxis = list(
      title = "Year",
      tickprefix = "y",
      hoverformat = ".0f"
    ),
    yaxis = list(
      title = "Gini index",
      tickformat = ".2f",
      range = c(0, 100)
    ),
    title = list(
      text = "Income inequality Over Time",
      x = 0.5
    ))

plot1 %>%
  add_trace(type = "scatter", 
            mode = "lines+markers", 
            data = wiid_regions,
            inherit = FALSE,
            x = ~year,
            y = ~region_mean, 
            color = ~region_un,
            hoverlabel = list(
              bgcolor = "black",
              font = list(
                family = "American Typewriter", color = "white", size = 15)
            ),
            hovertemplate = paste( # Use HTML tags below. 
              "<b>%{text}</b><br><br>",
              "Mean Gini Index: %{y:.2f}<br>",
              "<extra></extra>"
            ),
            text = ~paste0(year, ": ", "(",region_un, ")"))
      

