# Advanced Data Visualization
# Spring 2023
# Animating plots with gganimate. 

##########

library(readxl)
library(tidyverse)
library(gganimate)

# Animated plots are really just flipping through slide decks. 
# To begin, set up a normal plot. 

# Pay attention to which variable you want to animate on. 
# Almost always, it is either a grouping variable or your time variable(s).

# Key elements to an animated plot:
# 1: transitions. These are the most fundamental to an animation. 
# 2: views. These allow your plotting window to change along with the data. 
# 3: shadows. Do you want traces or memory? Shadows are where you create those. 
# 4: enters and exits: How should the data enter the plot and leave the plot? 
# 5: easings: the movement during the transition, defined by some function. 


# The first thing you need is a transition. 

# some notes on transitions.
# transition_states # think like a faceting. Needs to be categorical.
# transition_reveal # time-based, but calculates intermediary values.
# transition_time # time-based, discrete time points. 
# transition_manual # discrete states, with no animation allowed.     

# transition_states example. 

wiid <- read_excel(file.choose())

glimpse(wiid)

wiid_anim1 <- wiid %>%
  filter(year > 2000 & region_un_sub == "Northern America") %>%
  group_by(country, year) %>%
  summarize(mean_gini = mean(gini)) %>%
  ggplot(aes(x = year, y = mean_gini, color = country)) +
  geom_line() + #
  geom_point(size = 0.5) +
  labs(x = "Year", y = "Gini Index") +
  theme(legend.position = "none") +
  transition_reveal(along = year) + 
  shadow_wake(wake_length = 0.2, size = 1, alpha = FALSE, colour = 'grey92') +
  theme_minimal()

# Then we feed it into the animate() function.

# Extra arguments determine the length of the animation and rendering. 

# Duration refers to total time to cycle. 
# nframes is the number of total frames (including, thus, copies)
# fps is the frames per second. 
# renderer is format. Default is gif. 
# If you have any trouble animating your plots,
# you may need to install the gifski package and specify 
# the renderer argument (renderer = gifski_renderer)

animate(wiid_anim1, fps = 10, duration= 10) 

# Increase the FPS, smooth out the animation. 
# Note: Higher FPS takes longer to render. 

animate(wiid_anim1, fps = 15, duration= 10) 

animate(wiid_anim1, nframes = 150, duration= 10) 



# Then we save the animated plot. 

# gifski might need to be installed. 

anim_save("wiid_anim1.gif")

##### Let's take some of our old examples and animate them. 

data('mtcars')
head(mtcars)

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() +
  ggtitle('{closest_state}',
          subtitle = 'Frame {frame} of {nframes}') +
  
  # gganimate code starts here.
  
  transition_states(
    gear, # similar to a 'faceting' variable.
    transition_length = 2, # timing of transition.
    state_length = 1 # timing of state. 
  ) +
  enter_fly(x_loc = -1,
            y_loc = -1) + 
  exit_shrink() +
  ease_aes('sine-in-out') # sets the 'easing' (how to transition)

#?ease_aes

# There's something wrong with this plot, conceptually. Can you guess? 

# Technically speaking we shouldn't transition between different groups.
# Especially across units that cannot change from group to group.
# Can cars morph from one type of engine to another?
# Technically, yes. But

# Your title can animate along with your plot. 

ggplot(mtcars, aes(wt, mpg, group = gear)) + 
  geom_point() +
  ggtitle('{closest_state}',
          subtitle = 'Frame {frame} of {nframes}') +
  theme_minimal() +
  
  # gganimate code starts here.
  
  transition_states(
    gear, # similar to a 'faceting' variable.
    transition_length = 2, # timing of transition.
    state_length = 1 # timing of state. 
  ) +
  enter_fly(x_loc = -1) + 
  exit_shrink() 


### Some options work better than others depending on grouping/animating.
# You wouldn't really use an easing with discrete groups, for example. 

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = cyl_fact)) +
  geom_point() +
  theme(legend.position = "none") +
  transition_states(cyl_fact,
                    transition_length = 2,
                    state_length = 1) +
  # enter_fly(x_loc = -1) +
  # exit_shrink() +
  theme_minimal()

# Note: I did not use animate() this time, which prevents me from using some 
# useful specfications and arguments. 

anim_save("mtcars_anim.gif")

# Or, if you want to render the animation in a different way. 
# Save the animation plot as an object; then animate and render it.
# You can 'render' the animation in different formats. movie, gif, etc. 

mtcars %>%
  mutate(cyl_fact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = cyl_fact)) +
  geom_point() +
  theme(legend.position = "none") +
  transition_states(cyl_fact,
                    transition_length = 2,
                    state_length = 1) +
  #ease_aes("cubic-in-out") +
  #shadow_wake(wake_length = 0.2, size = 1, alpha = FALSE, colour = 'grey92') +
  #enter_fly(x_loc = -1) +
  #exit_shrink() +
  theme_minimal()


plot1dat <- wiid %>%
  filter(year > 1990) %>%
  mutate(population2 = population/1000000,
         log_gdppc = log(gdp)) %>%
  group_by(country, year, region_un, population2) %>%
  mutate(mean_gini = mean(gini, na.rm = TRUE),
         year2 = as.integer(year)) %>%
  summarize(mean_gini = mean(gini, na.rm = TRUE),
            mean_loggdp = mean(log_gdppc, na.rm = TRUE))

head(plot1dat)


# Play with the code below to mimic a "gapminder"-style animation. 

p1 <- plot1dat %>%
  ggplot(., aes(x = mean_loggdp, 
                mean_gini, 
                size = population2, 
                color = region_un, 
                group = country)) +
  geom_point(alpha = 0.7) +
  #scale_colour_manual(values = country_colors) +
  # scale_size(range = c(2, 12)) +
  # scale_x_continuous(trans = "log") +
  scale_y_continuous(limits = c(15, 65)) +
  # Here comes the gganimate specific bits
  #labs(title = 'Year: {closest_state}', x = 'GDP per capita', y = 'Gini') +
  transition_states(year,
                   transition_length = 1,
                    state_length = 2) +
  # transition_reveal(along = year) +
  labs(title = 'Income Inequality and Per Capita Income in {frame_time}',
       y = "Gini Index", x = "Logged Per Capita Income",
       color = "UN Region", size = "Population \n in Millions") +
   ease_aes('sine-in-out') +
  theme_minimal()

anim1 <- animate(p1,
                 duration = 100,
                 fps = 10)
anim1
anim_save("wiid_anim.gif", anim1)
