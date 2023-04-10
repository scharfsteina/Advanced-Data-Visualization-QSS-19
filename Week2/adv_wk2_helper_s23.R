# Advanced Data Visualization Spring 2023
# Color Theory Helper Script 
# Professor Robert A. Cooper

library(tidyverse) # everything!
library(colorspace) # conversion of RGB to hex code, among other things. 
library(colorBlindness) # cool color blind tester. 
library(gridExtra) # This is for multiple plots on one visualization. 
library(scales)

####
# COLOR THEORY.

# HUE. What is the true underlying color from color wheel.
# VALUE. Lightness/darkness. Translates to grayscale.
# INTENSITY. Purity of hue. Is there any white or black added to hue?

# TINTING V. SHADING. People screw these up all the time. 

# https://www.color-hex.com/
# USE HEX CODES FOR COLORS! 16,777,216 combinations!

###############################################################################
# COLOR THEORY RECTANGLES. 
# Let's make some colors. 
# Be creative. Try your own. 

rect <- data.frame(x1 = 3, x2 = 5, y1 = 2, y2 = 5); rect
rect2 <- data.frame(x1 = 5, x2 = 7, y1 = 2, y2 = 5); rect2
rect3 <- data.frame(x1 = 3.75, x2 = 4.25, y1 = 3, y2 = 4); rect3
rect4 <- data.frame(x1 = 5.75, x2 = 6.25, y1 = 3, y2 = 4); rect4
rect5 <- data.frame(x1 = 4.25, x2 = 5.75, y1 = 3.3, y2 =  3.7); rect



# Can you make two colors look like one?
# Can you make one color look like two? 

ggplot() +
  geom_rect(data = rect, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#ffe599") +
  #geom_rect(data = rect2, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#1f00e3") +
  geom_rect(data = rect2, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#256ed5") +
  geom_rect(data = rect3, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#c2dd85") +
  geom_rect(data = rect4, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#c2dd85") +
  theme_minimal()

# Let's explore the notion of `transparent` colors. 
# It's not really transparency that is happening; it's averaging over hue, intensity, value. 

# Vertical/long rectangles.

rect6 <- data.frame(x1 = 3, x2 = 4, y1 = 2, y2 = 5); rect6
rect7 <- data.frame(x1 = 4, x2 = 5, y1 = 2, y2 = 5); rect7
rect8 <- data.frame(x1 = 5, x2 = 6, y1 = 2, y2 = 5); rect8
rect9 <- data.frame(x1 = 6, x2 = 7, y1 = 2, y2 = 5); rect9
rect10 <- data.frame(x1 = 7, x2 = 8, y1 = 2, y2 = 5); rect10
rect11 <- data.frame(x1 = 8, x2 = 9, y1 = 2, y2 = 5); rect11
rect12 <- data.frame(x1 = 9, x2 = 10, y1 = 2, y2 = 5); rect12
rect13 <- data.frame(x1 = 10, x2 = 11, y1 = 2, y2 = 5); rect13

# Triangle for a geom_polygon. 

triangle1 <- data.frame(x = c(4, 5.1, 5.3),
                     y = c(3.1, 4.4, 2.7))

triangle2 <- data.frame(x = c(3.4, 6.1, 6.3),
                        y = c(4.8, 4.4, 2.7))


# Rectangles plus triangle. 
# USE THIS TEMPLATE FOR THE ASSIGNMENT PROBLEM. 

colorplot1 <- ggplot(data= rect) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#f7693e") +
  geom_rect(data = rect2, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#056671") +
 # geom_polygon(data = triangle1, aes(x = x, y = y), alpha = 0.4) +
  geom_polygon(data = triangle2, aes(x = x, y = y), alpha = 0.4) +
  theme_void()


######################

# To get our palette for our colors, we can use colorRampPalette. 
# That creates a function. The argument allows us to return as many break points 
# as we like. 

rc_pal <- colorRampPalette(c("#de582b", "#ffac91")) # For the vermillion colors. 
rc_pal(9)

# To display and evaluate our colors, we can use show_col from the scales package.  

show_col(rc_pal(9))

hallow_pal <- colorRampPalette(c("#fb7417", "#f0a16b"))

# Other colors...

# Colors interact with each other and vibrate. 
# Example: Blues and red-orange fields with another color band. 
# Practice on your own. 

ggplot() +
  geom_rect(data = rect6, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#0b2fb2") +
  geom_rect(data = rect7, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#2849bf") +
  geom_rect(data = rect8, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#4c68d0") +
  geom_rect(data = rect9, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#7f95e1") +
  # geom_rect(data = rect10, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#DE582B") +
  # geom_rect(data = rect11, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#E26439") +
  # geom_rect(data = rect12, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#E77048") +
  #geom_rect(data = rect13, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "#EC7C56") +
  theme_minimal()


#################################################################################

# Practical color lessons in R. Color brewer, palettes, etc.
# USE COLOR HEX CODES! There are almost 17 million colors!

# One color is okay. But why waste an opportunity?

data("mtcars")  

mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point(color = "red") +
  theme_minimal()

# You could let color vary by...a lot of things. 

# Color can reinforce movement. 
# Color here can reinforce the notion of fuel efficiency.
# Think 'heaviness of the carbon footprint'. 

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = mpg)) +
  geom_point() +
  labs(x = "Weight (in Thousands of Pounds)",
       y = "Miles Per Gallon",
       title = "Fuel Efficiency by Weight") +
  scale_color_gradientn(colors = c("black", "gray85")) +
  theme_minimal() +
  theme(legend.position  = "none")

# Color can reference another variable altogether. 

# colorRampPalette creates a function for any color scale.
# Pick the colors you want. 
# Doesn't have to be just two. This is how you can get creative. 


rc_pal <- colorRampPalette(c("blue", "green", "purple"))

# Then choose the number of breaks you want. 
# The object created is a FUNCTION. 

rc_pal(9)

# You can also just stuff the function inside the plot.

# Check your number of categories; the palette has to match. 

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  distinct(cyl_fact)

# In a discrete scale, the number inside should match. 

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  labs(x = "Weight (in Thousands of Pounds)",
       y = "Miles Per Gallon",
       title = "Fuel Efficiency",
       subtitle = "By Weight and # of Cylinders",
       color = "Cylinders") +
  scale_color_manual(values = rc_pal(3)) + # See the function stuffed here. 
  theme_minimal() +
  theme(legend.position  = c(.8, .7),
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.title = element_text(size = 9))

# Using the colorspace library. 

# Let's say you want discrete colors from a continuous scale. 
# hcl =  hue (wavelength/'color' in layman's terms), chroma (intensity), & luminosity (value)    

# Figuring out your own funky palettes. 
# RGB: red-green-blue, 8 bits each, 0 to 255. 

# colorRamp v. colorRampPalette
# colorRamp returns RGB.
# colorRampPalette returns hex codes.
 
rc_pal <- colorRamp(c("orange", "blue"))
rc_pal(1) # Returns RGB values at lowest end.
rc_pal(0) # Returns at highest end.

rc_pal2 <- colorRampPalette(c("orange", "red"))
rc_pal2(3)
rc_pal2(7) # Returns hex codes. 

# Alternate ways to get a scale. 
# From the colorspace package. 

sequential_hcl(4, "Purples 3")
sequential_hcl(3, c(rc_pal(0), rc_pal(1))) 

# Convert between hex codes and RGB and vice versa.
# Also from colorspace package, for the hex2RGB function. 
# col2rgb is from grDevices, like colorRampPalette and colorRamp. 


hex2RGB("#ff0000") # Returned as RGB proportion. 
col2rgb("#ff0000") # Returned in units. Base R graphical device. 

##

library(tidyverse)
library(colorBlindness)

# Revisiting principles and best practices.

# Color theory. 

# A couple new geoms. 

# Details and Additions. 
# Facets. Scales, legends. Axes. Titles. Themes. 

# install.packages("colorBlindness")

plot1 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar()

# Check the effect of various color blindnesses. 

colorBlindness::cvdPlot(plot1)

# Deuteranopia = Blindness to green, green cones absent.
# Protanopia = Blindness to red, red cones absent. 
# Tritanopia = Blindness to blue, exceptionally rare. 
# Rod cells: brightness, shape, size.
# Cone cells: color. Red, blue, and green-sensitive cones.  

# Taking advantage of the likelihood of color blindness afflictions,
# the best bet is (a) vary value/lightness, and potentially (b) use blues. 

# Let's apply a new color scheme. 
# It works well, focuses on value/luminosity differences. 

col_new <- c("#304e8d", "#92e5ad", "#ee542d")

plot2 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = col_new) +
  theme_minimal()

colorBlindness::cvdPlot(plot2)

# You do not need the viridis scale, but it is a useful one. 

plot3 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_viridis_d() +
  theme_minimal()

colorBlindness::cvdPlot(plot3)

# One hue, different values (and intensities, a bit). 
# Purplish blues. 

col_new2 <- c("#322671", "#5a48ba", "#9d8dee")

plot4 <- mtcars %>%
  ggplot(aes(x = factor(cyl), fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = col_new2) +
  theme_minimal()

colorBlindness::cvdPlot(plot4)

# Result is as good as viridis!
# Except for

################################################################################
# Plot customization. 

# Grabbing plot-build data. 

# OECD line plots by year. Note: points underneath smoother would be an improvement.

library(readxl)

wiid <- read_excel(file.choose()) 
glimpse(wiid)
wiid_sub <- wiid %>%
  filter(year > 1960 & year < 2002) 

wiid_sub %>%
  distinct(year) %>%
  arrange(year) %>%
  print(n = nrow(.))

wiid_smooth <- wiid_sub %>%
  group_by(oecd, year) %>%
  summarize(med_gini = median(gini, na.rm = TRUE)) %>%
  ggplot(., aes(x = year, y = med_gini)) +
  geom_point(aes(color = oecd), alpha = 0.5) +
  geom_smooth(aes(color = oecd), span = 0.5) +
  ylim(c(30, 60)) +
  labs(x = "Year",
       y = "Median Gini Score",
       title = "OECD vs. Non-OECD Countries and Income Inequality",
       color = "OECD") +
  theme_minimal()

wiid_smooth

# Let's say I want a geom_ribbon, like the W. Playfair plots. 
# Aesthetics: x OR y, then xmin/xmax or ymin/ymax. 

# 1. Start with a basic smoother. 
# 2. Save the build data from ggplot. 
# 3. Get into the list object that is created. 

plot_dat <- ggplot_build(wiid_smooth)
class(plot_dat)  
is.list(plot_dat) # Recall: special objects are sometimes made by packages. 
# That does not mean they do not have basic underlying formats that you know. 
length(plot_dat)

# Work with these data. 

plot_dat

# Investigate: 

plot_dat[[1]]
plot_dat[[1]][[1]] # These happen to be the plot data for the points. 
plot_dat[[1]][[2]] # These happen to be the plot data for the lines. 

# We want the line data, obviously. 

plot_dat2 <- data.frame(plot_dat[[1]][[2]]) # Just the line data. 

class(plot_dat2) # Just to double-check. 
glimpse(plot_dat2)
tail(plot_dat2)

# Let's look at the data underneath the build.
# We will be particularly interested in the 'group' variable.

head(plot_dat2)

plot_dat2 %>%
  count(group) # 80 values per group, it seems. 

plotdat2.2 <- plot_dat2 %>% 
  mutate(newid = rep(1:80, 2)) 

# We are going to use the 'newid' for a join!

plot_dat_list <- plotdat2.2 %>% 
  group_split(group) 

plot_dat_list
plot_dat_list[[1]]
plot_dat_list[[2]]

# We will use the 'newid' as our key variable in a join.

plot_join_dat <- full_join(plot_dat_list[[1]][,c("x", "y", "group", "newid")], 
                           plot_dat_list[[2]][,c("x", "y", "group", "newid")], 
                           by = c("newid"))

# One key issue is that occasionally the estimated X might not match up.
# IF the series from 1 group is longer/shorter than the other. 

wiid_smooth_data <- wiid %>%
  filter(year > 1960 & year < 2002) %>%
  group_by(oecd, year) %>%
  summarize(med_gini = median(gini)) 

# Make a ribbon plot. 

plot_join_dat %>%
  ggplot(aes(x = x.x, ymin = y.x, ymax = y.y)) +
  # geom_smooth(data = wiid_smooth_data, 
  #            aes(x = year, y = med_gini, color = oecd), 
  #            span = 0.5, 
  #            inherit.aes = FALSE) +
  #geom_line(aes(y = y.x), color = "red", size = 0.3) +
  #geom_line(aes(y = y.y), color = "red", size = 0.3) +
  geom_ribbon(fill = "#7fc7fd", alpha = 0.4) +
  geom_line(aes(y = y.x), color = "red", size = 0.3) +
  geom_line(aes(y = y.y), color = "red", size = 0.3) +
  # geom_textpath(aes(x = x.x, y = y.y, z = label)) + 
  ylim(c(20,60)) +
  labs(x = "Year",
       y = "Median Gini Scores",
       title = "OECD v. Non-OECD") +
  theme_minimal()

# We could even add text. 

library(geomtextpath)

text <- "Non-OECD Countries"
text2 <- "OECD Countries"

text_df <- data.frame(x = )

plot_join_dat %>%
  ggplot(aes(x = x.x, ymin = y.x, ymax = y.y)) +
  geom_ribbon(fill = "#7fc7fd", alpha = 0.4) +
  # geom_line(aes(y = y.x), color = "red", size = 0.3) +
  # geom_line(aes(y = y.y), color = "red", size = 0.3) +
  geom_textpath(aes(x = x.x, y = y.x), label = text, size = 2, color = "red") + 
  geom_textpath(aes(x = x.x, y = y.y), label = text2, size = 2, color = "red") + 
  ylim(c(20,60)) +
  labs(x = "Year",
       y = "Median Gini Scores",
       title = "OECD v. Non-OECD") +
  theme_minimal()

################################################################################
# Creating new variables for aesthetics. 
# Probably the most common way to make a plot `your own`. 

# Two examples from the assignments so far:

# 1: Labels for the scatterplot/dotplot of European countries flipped.

data("mtcars")
  
  
library(tidyverse)

wiid_labs <- wiid %>%
  filter(region_un == "Europe") %>%
  group_by(country) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE)) %>%
  mutate(lab_pos = ifelse(country == "Bosnia and Herzegovina", -.1,
                          ifelse(country == "Serbia and Montenegro", -.02,
                                 ifelse(country == "Macedonia, former Yugoslave Republic of", -.02, 0)))) 

wiid_labs %>%
  ggplot(., aes(x = reorder(country, mean_gini, na.rm = TRUE), y = mean_gini)) +
  geom_point() +
  geom_text(aes(label = country, y = mean_gini, hjust = lab_pos), size = 2.5, nudge_y = 0.5) +
  labs(x = "", y = "Average Gini Index Scores", 
       title = "Income Inequality in Europe") +
  coord_flip() +
  ylim(c(20,45)) +
  theme_minimal() +
  theme(axis.text.y = element_blank())



################################################################################
# Using things like inset plots from cowplot. 

library(tidyverse)
library(cowplot)

pacdat <- data.frame(x = c(1, 1, 1, 1, 1, 2, 3, 4, 5, 6),
                     y = c(1, 2, 3, 4, 5, 1, 1, 1, 1, 1))

pacdot_plot <- pacdat %>%
  ggplot() +
  geom_point(aes(x = x, y = y), size = 5, color = "white") +
  expand_limits(x = 0, y = 0) +
  labs(title = "PAC MAN!", color = "white") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5,
                                  color = "white",
                                  size = 20),
        plot.background = element_rect(fill = "#365096"))

pacdot_plot

pacman <- data.frame(y = c(rep("Pac", 13), rep("Man", 3)))

pacplot <- pacman %>%
  ggplot() +
  geom_bar(aes(x = 1, fill = y)) +
  coord_polar(theta = "y", 
              start = 1.6*pi) +
  scale_fill_manual(values = c("#365096", "#f6c704")) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "transparent"),
        rect = element_rect(fill = "transparent"),
        panel.border = element_rect(fill = "transparent",
                                    color = "#4360ae"))

pacplot

# Using ggdraw to inset a plot. Super useful. 

ggdraw() +
  draw_plot(pacdot_plot) +
  draw_plot(pacplot, x = .33, y = -.26, scale = 0.25)








  







