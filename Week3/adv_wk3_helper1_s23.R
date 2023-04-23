# Data Visualization
# Spring 2023
# Week 3 geographic data and mapping. Plus web scraping.  
# Robert A. Cooper

# MAPPING: ALL ABOUT POINTS, LINES, AND POLYGONS. 

# For some of the following packages, you may need to install from Github. 

library(tidyverse) # for data work and plotting. 
library(devtools) # for Github installations. 
library(sf) # for working with 'simple feature' format. 
library(rgdal) # For reading in sp objects. TO BE RETIRED SOON. 
library(ggspatial) # For sp and sf to data frame. Use rarely. 
library(USAboundaries) # For a set of US map data points. 
library(USAboundariesData) #  Datasets related to USAboundaries.
library(tmap) # One way to easily map sf and sp objects.
library(rnaturalearth) # For great mapping data. 
library(rnaturalearthdata) # For high-resolution natural earth data.
library(rnaturalearthhires)


# There is also a 'terra' package, useful for raster data. 
# To install the collection of rnaturalearth packages, try the code below. 

# devtools::install_github("ropenscilabs/rnaturalearth")
# devtools::install_github("ropenscilabs/rnaturalearthdata")
#install.packages("rnaturalearthhires",
#                 repos = "http://packages.ropensci.org",
#                 type = "source")


# We will discuss three types of mapping objects. 
# NOTE: if you get very/overly interested in mapping, I suggest long-term that 
# you also take a look at QGIS, a separate, open-sources GIS software. 
# QGIS can connect to both R and Python, for those interested. 

# data frame/spatial data frames. Easiest, but easiest to screw up.
# sf. simple features. Best to work with, in my opinion.
# sp. spatial polygon data. GIS style. Complicated, nested structure. 

###

# By levels of complexity: sp > sf > data frame. 

###

# For sp objects, you are importing shapefiles, which are folders of multiple files, including a .shp file. 
# DO NOT EVER REMOVE THE .SHP FILE FROM THE FOLDER WITH THE OTHER SPATIAL FILES. 

# Mapping in r. A few object classes to be familiar with: sp, sf, and spatial data frames. 
# sp objects: S4 object, extra-nested list-like object. A list on steroids. 
# sf objects: S3: data frame + "geometry", a list object.  
# spatial data frames: s3: regular data frames with longitude and latitude variables. 

# Mapping data: so many sources, it is impossible to list them all. 

# ggplot2: has its own map data.
# maps: has many data sets. 
# USAboundaries: many data sets.
# USAboundariesData: many data sets. 
# the list goes on...

 # https://www.naturalearthdata.com/downloads/

# Back to spatial data objects. 
# REPEAT: 
# sp: spatial objects. 'S4' class objects with different nesting structure. 
# sf: simple feature objects. Have a 'geometry' variable for coordinates.
# data frames: polygon data with long and lat as separate variables.

###
# sp objects. sp objects are 'S4' class. All others you are working with in this course are 'S3'
# Thus, sp objects look different. Only going to focus on basics here. 
# Subsetting looks different. We have slots (fields).
# The '$' is essentially replaced with '@', and slot() replaces [[]]

# From rgdal package.

# First line requires rgdal; second requires sf. 


spdat1 <- readOGR("/Users/robertcooper/Desktop/dv_plot_data/US_Climate/US_Climate.shp") # rgdal.

class(spdat1)
spdat1
spdat1@proj4string

spdat1@data %>%
  glimpse()


# rgdal increasingly being folded into sf package. We will focus primarily on the sf package and sf objects.

# st_read IN PLACE OF readOGR.

sp_sfdat <- st_read("/Users/robertcooper/Desktop/dv_plot_data/US_Climate/US_Climate.shp") # sf.
glimpse(sp_sfdat)
head(sp_sfdat)
sp_sfdat$geometry

# NOTE: the two read-ins create different object types. 

spdat1 # spdat1 is an 'sp' object, which is an S4 object.
spdat1@data# sp object. With 'slots'. 

# The sf object is `simplified`. 

glimpse(sp_sfdat)
sp_sfdat # sf object, which is an S3 object. 
r
isS4(spdat1)
class(spdat1); class(sp_sfdat)

# Typically, what information are we looking for? 
# (1) Longitude and latitude.
# (2) Instructions on if and how to draw the points together. 
#     - Points can form (a) points, (b) lines, or (c) polygons.
#     - They can also be organized into small square areas, called rasters. 
# (3) A 'bounding box', the box with the smallest measure within which all the points lie.
# (4) A coordinate reference system, or CRS. These can be 'horizontal' or 'vertical'. 
# (5) How and where to draw our data onto our flat map. "Projection" 

# The CRS contains a lot of information:
#   - The geographic coordinate reference system
#   - The most common is WGS 84, or World Geodetic System 1984. 
#   - Another fairly common is EPSG (European Petroeum Study Group)

# https://pro.arcgis.com/en/pro-app/latest/help/mapping/properties/coordinate-systems-and-projections.htm
# Most importantly for you, the CRS for different objects has to match if you want to map them together!!!

# For sf objects, all of this exists inside a single list variable called GEOMETRY. 
# For sp objects, these pieces of information are in various 'slots' in the object. 

sp_sfdat$geometry[1]

# Subsetting an 'sp' uses "slots" with the '@' symbol, for starters.

spdat1@data # Features about the climate stations here. 
spdat1@coords
spdat1@bbox # There will always be a 'bbox': a "bounding box" (smallest box encompassing all points in set)
spdat1@proj4string # There will always be a projection string. 

# Like a normal list, you can keep subsetting further and transform/tidy data. 

spdat1@data$STATE

# So, how do you make a map with an sp object? You can use tmap. Or spplot.
# OR... convert the sp object to sf or spatial data frame. Choose sf!
# I generally recommend the latter. sf objects are flexible and
# easier to work with. 

#####
# sf objects. These look and act like data frames, except for the geometry. 
# If you have trouble finding USAboundariesData, then...
# dev.off()

# library(devtools)
devtools::install_github("https://github.com/ropensci/USAboundariesData")

library(USAboundariesData)
library(USAboundaries) # A number of levels of mapping data available. Investigate. 

# library(usmap) # This can also work with ggplot2.

# Maps comes in a range of low to high-resolution forms. 
# The choice depends on what you want to do with the data and your computer's ability to produce the map.

state_dat <- us_states() #map data. 
state_dat

class(state_dat)

state_dat$geometry
sp_sfdat$geometry[1]

# One example of the continental U.S.
# For typical maps of the US, Alaska and Hawaii are problems unless you inset them. 

state_dat %>%
  ggplot() +
  geom_sf(size = 0.1) +
  coord_sf(xlim = c(-135, -60)) + # To drop Alaska and Hawaii. 
  theme_minimal()


# To add Hawaii and Alaska back, we have some options:
# (1) fiftystater. 
# (2) usmaps package. plot_usmap function. 
# (3) Use inset plotting. Make 3 plots; inset 2. 
 
# library(devtools)

devtools::install_github("wmurphyrd/fiftystater")

# For fiftystater...

library(fiftystater)

fifty_states
glimpse(fifty_states)

fifty_states %>%
  ggplot(aes(x = long, y = lat, group = group, order = order)) +
  geom_polygon(fill = "gray80", color = "gray20", size = 0.2) +
  coord_map() +
  theme_minimal()

# A note for map data frames and sfs. They mix in ggplot. 

# Let's say I wanted to map the stations for June temperatures. 
# The plot below produces only the points from the climate stations. 

sp_sfdat

sp_sfdat %>%
  ggplot() +
  geom_sf(aes(col = T06), size = 0.3) +
  theme_minimal() +
  coord_sf()

# We can, of course, combine the underlying map data with the climate data.
# To do correctly, the CRS of one has to match the other. 

state_dat

state_dat %>%
  ggplot() +
  geom_sf(size = 0.1, fill = "gray95") + 
  geom_sf(data = sp_sfdat, aes(col = T06), size = 0.8) +
  theme_minimal() +
  coord_sf(xlim = c(-130, -55), ylim = c(20, 55))

#########################
# Data frame with x and y long & lat variables. 
# This is fine, but instructions are needed. 
# sp and sf objects come with all their instructions ready to go. 
# Data frame objects have to get the grouping and ordering JUST RIGHT to work. 

# More maps. This time from ggplot2 itself. 

?map_data

world <- map_data("world") # from ggplot2. Normal data frame. 
head(world); class(world)

states1 <- map_data("state") # from ggplot2. Normal data frame.
head(states1)

# Simple map of US from a data frame. 
# Thus, we use geom_polygon instead. 
# NOTE: don't go from sp to sf to data frame.
# Use data frame data if the map data are already in data frame format!

states1 %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "gray20", size = 0.1) +
  coord_map() +
  theme_minimal()

###########
# Another way: mix data frame and sf together!

ggplot(data = states1) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "gray25", color = "black", size = 0.1) +
  geom_sf(data = sp_sfdat, aes(col = T01), size = 0.9)  +
  labs(title = "U.S. Climate Data: Measurement Stations",
       col = "Degrees \n Celsius") +
  theme_minimal() +
  coord_sf(xlim = c(-130,-60), ylim = c(23, 50)) +
  scale_color_gradientn(colors = c("blue", "yellow")) +
  theme(legend.position = c(.9, .3))

#########
# Let's work a bit with the USAboundaries data. 
# If you have trouble finding USAboundariesData, then...

library(devtools)
devtools::install_github("https://github.com/ropensci/USAboundariesData")

library(USAboundariesData)
library(USAboundaries) # A number of levels of mapping data available. Investigate. 
library(ggspatial) # Easily transform sp and sf objects into data frames. 

class(state_dat); glimpse(state_dat)
state_dat # US states, sf object. 

# ggspatial allows you to convert directly to data frame from sf or sp.
# NOTE: this is a cautionary tale. Don't go to data frames unless you have no options. 

statez <- df_spatial(state_dat)
class(statez); glimpse(statez)
head(statez)

# From the converted data frame object...
# WHAT IS WRONG HERE? Can you tell?

statez %>%
  ggplot(aes(x = x, y = y, group = feature_id)) +
  geom_polygon()

# The above fails across various grouping variables. So just use sf!!!!!

state_dat %>%
  ggplot() +
  geom_sf() +
  coord_sf() +
  theme_minimal()

# tmap is also an option. Useful for working with sp objects. 
# tmap is a very nice package. 

library(tmap)

state_dat %>%
  tm_shape() +
  tm_borders()

###########################################################################
# USABoundaries. Historical data. 
# The USABoundaries data allow us to map the US and its historical borders. 

usa <- us_states("1865-01-01")
class(usa); head(usa)

# We could try tmap, just to see how it is different. tmap vs. sf below.  

# tmap takes sf objects or sp objects. 

usa %>%
  tm_shape() +
  tm_borders()

# ggplot can take data frames or sf objects. 

usa %>%
  ggplot() +
  geom_sf()+
  coord_sf() +
  theme_minimal()

### You can also get cities data from USABoundaries.  

us_cities()
cty_dat <- USAboundaries::us_cities()

glimpse(cty_dat)
class(cty_dat) # See class 'sf' and 'data.frame'. 

##########
##### Spatial data frames. From ggplot itself. 
# These are normal data frames with longitude and latitude as variables.
# You need to be especially careful about grouping. 
# Now we can make a map with geom_polygon. 

# Let's grab some map data from ggplot2. 
# Finding the grouping variable won't always be this easy.

world <-map_data("world2")
head(world)

county <- map_data("county")
head(county)

world %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "gray80", color = "black", size = 0.1) +
  theme_minimal() +
  coord_map()

###
# Another great data source: rnaturalearth.
# One advantage of rnaturalearth is that you have choices about level of resolution.

library("rnaturalearth")
library("rnaturalearthdata")


world <- ne_countries(scale = "medium",
                      returnclass = "sf") 


# Let's take a quick look at the resulting map data.

glimpse(world)

# Once again, you could use tmap or sf::geom_sf, no problem.  

world %>%
  ggplot() +
  geom_sf(size  = 0.2) +
  coord_sf() +
  theme_minimal()


################################################################################

# Web Scraping with rvest.
# Prof. Robert A. Cooper


library(tidyverse)
library(rvest)
library(selectr) # Translating css to xpath. 


# API (application programming interface) v. no API. 

# HTML. We are searching the HTML for the data. 

# CSS v. XPATH. Two different pattern/style searching ways to locate the data in the HTML. 

# XPATH is often quite finicky. CSS is more generic and a bit easier. 
# XPATH tends to be inconsistent, but it good at searching for specific content. 
# XPATH stands for XML path.
# XML is a query language. Defining procedures for getting info from web sites or info systems. 

# CSS = 'cascading style sheets' are more general and do not search for specific content.
# CSS looks for an HTML style or general class. 
# When in doubt, use CSS. 

# Good article on CSS v. XPATH. 
# https://medium.com/dataflow-kit/css-selectors-vs-xpath-f368b431c9dc


# To find a good CSS path, SelectorGadget can be very useful to you. 
# SelectorGadget goes in your Bookmarks toolbar in your web browser. 


# When it comes to scraping, we will generally boil it down to three types
# of html elements: tables, text, and attributes.
# Tables and text are generally self-explanatory.
# Attributes are other data, or metadata, required for the construction of the web page. 

# To convert from CSS to XPATH, you can use selectr.

selectr::css_to_xpath("td", translator = "html")


#####
## Example: NBA Standings. 

library(rvest)
library(ggrepel)
library(gridExtra)

team_stand <- read_html("https://www.espn.com/nba/standings") 
team_stand

team_st <- team_stand %>% 
  html_nodes(css = "table") %>%
  html_table()

team_st
class(team_st)
length(team_st)

# Explore the html components if necessary. 

team_stand %>%
  html_elements("section")

# Use html_node(s)  to locate the information.
# Use table/text/attrs to extract and store the info. 

team_st <- team_stand %>% 
  html_nodes(css = "table") %>%
  html_table()

# How does it differ if you use html_node? Brings back the first node/element in list. 

team_stand %>% 
  html_node(css = "table") %>%
  html_table()

team_st

# Produces a list of 4 tables. 

east1 <- team_st[[1]] 
east2 <- team_st[[2]]

east1
east2

# Get the two conferences together. 
east
east <- cbind(east1, east2)
west <- cbind(team_st[[3]], team_st[[4]])

west

# Combine. 

allteam <- rbind(east, west); allteam

# NBA Plots. 

plot1 <- allteam %>%
  ggplot(., aes(x = PPG, y = PCT, size = DIFF)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = "Points Per Game Scored", y = "Win Percentage",
       title = "Does Great Offense Win More Games in the NBA?") +
  theme_minimal() +
  theme(legend.position = "none")

plot2 <- allteam %>%
  ggplot(., aes(x = `OPP PPG`, y = PCT, size = DIFF)) +
  geom_point(alpha = 0.3) +
  geom_smooth() +
  labs(x = "Points Per Game Allowed", y = "Win Percentage",
       title = "Or Does Great Defense?",
       caption = "Source: https://www.espn.com/nba/stats") +
  theme_minimal() +
  theme(legend.position = "none")

grid.arrange(plot1, plot2, ncol = 2)

##### Example 2. Fortune 500.


fort <-  "https://en.wikipedia.org/wiki/Fortune_Global_500"
f500 <- read_html(fort)

ftab <- f500 %>%
  html_nodes("table") %>%
  html_table()

ftab # Guest what it is? A list!

fdat <- ftab[[1]]; fdat

##### Example 3. Exploring the Ivy League Wiki. 

ivy <- "https://en.wikipedia.org/wiki/Ivy_League"
ivyy <- read_html(ivy)

ivytab <- ivyy %>%
  html_nodes("table") %>%
  html_table(fill = TRUE)

ivydat <- ivytab[[2]] # Correct table. 
ivydat
glimpse(ivydat)
ivydat$Colors[1]

ivycols_attrs <- ivyy %>% 
  html_nodes("td:nth-child(8) span") %>% # This will get you official colors. 
  html_attrs() 

ivycols_attrs

# Code below will get you the colors right from hex codes on website HTML. 

ivycols <- ivyy %>% 
  html_nodes("td:nth-child(8) span") %>% # This will get you official colors. 
  html_attrs() %>%
  unlist() %>%
  str_extract_all(., "\\#[:alnum:][:alnum:][:alnum:][:alnum:][:alnum:][:alnum:]") %>%
  unlist()

ivycols

ivycolors <- ivycols[c(1, 4, 6, 8, 10, 13, 16, 17)]
ivycolors

# Plot. Minus the colors. Add colors from code above. 

ivydat %>%
  mutate(enroll = parse_number(Undergraduates)) %>%
  ggplot(aes(x = reorder(Institution, enroll), y = enroll, fill = Institution)) +
  geom_col() +
  labs(y = "Undergraduate Enrollment",
       title = "Ivy League Undergraduate Enrollment") +
  scale_fill_manual(values = ivycolors) +
  theme_minimal() +
  theme(legend.position = "none") + 
  coord_flip()

dev.off()










