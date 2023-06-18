# Advanced Data Visualization 
# Prof. Robert A. Cooper
# Spring 2023

# Week 4 Helper 1. 

library(gganimate)
library(magick)

# Basic work with purrr map functions, etc. 
# Combined animations with magick. 
# The camcorder package for your project stories. 

# Note your working directory. 

getwd()
# setwd() # Use this if you wish to change your working directory. 

# A basid ouble animation. 
# Animation 1. 

anim1 <- mtcars %>%
  mutate(cylfact = factor(cyl)) %>%
  ggplot(aes(x = wt, y = mpg, color = cylfact)) +
  geom_point() +
  labs(x = "Weight in 1000s of Pounds",
       y = "Miles per Gallon",
       title = "Fuel Efficiency and Car Weight by Engine Cylinders",
       color = "Engine \n Cylinders") +
  transition_states(cylfact,
                    transition_length = 3,
                    state_length = 1) +
  ease_aes("cubic-in-out") +
  enter_fly(x_loc = -1, # These locations are based on your axes/scales/values.
            y_loc = 35) +
  theme_minimal()

# Note: you need the magick_renderer as opposed to the default gifksi. 

anim1_gif <- animate(anim1, 
                     duration = 18, 
                     fps = 10,
                     res = 180,
                     width = 800,
                     height = 800,
                     renderer = magick_renderer())

## Make another animation. 

anim2 <- mtcars %>%
  mutate(cylfact = factor(cyl)) %>%
  group_by(cylfact) %>%
  summarize(avgwt = mean(wt, na.rm=TRUE)) %>%
  ggplot(aes(x =  cylfact, y = avgwt, fill = cylfact)) +
  geom_col(width = .7) +
  transition_states(cylfact,
                    transition_length = 3,
                    state_length = 1) +
  ease_aes("cubic-in-out") +
  enter_grow() +
  theme_minimal()


anim2_gif <- animate(anim2, 
                     duration = 18, 
                     fps = 10,
                     res = 180,
                     width = 800,
                     height = 800,
                     renderer = magick_renderer())

#Initiating/seeding the combined gif.

final_gif <- image_append(c(anim1_gif[1], anim2_gif[1]))

# Looping through the remaining frames. 

 for(i in 2:180) {
   combined <- image_append(c(anim1_gif[i], anim2_gif[i]))
   final_gif <- c(final_gif, combined)
 }

image_write(final_gif, "rc_double_cars2.gif", format = "gif")

# Note: there is another way we can create a double animation. 
# Using cowplot. Recall the 'customization' script I gave you.
# The baseline of the code would be something like the pacman plot. 


##########
# Functions. 

# What is a functions, and how do we create our own? 
# A function is essentially a wrapper for other code. 
# Maybe a bundle of functions you do often. 

# This is the essence of a function.
# Arguments go in the parentheses. 
# Code/functions go inside the brackets. 

easy_math <- function(x, y){
  stuff <- x + y^2
  print(stuff)
}

# Like other functions, I can reverse arguments if I name them explicitly.

easy_math(4, 8)
easy_math(y = 4, x = 8)

# You can also write functions that do not require an argument. 

lettersampler <- function(){
  letz <- sample(letters, 40, replace = TRUE)
  LETZ <- sample(LETTERS, 40, replace = TRUE)
  print(str_c(letz, LETZ))
}

# Note: functions require a print or a return, depending on the purpose of the output. 
# I rarely use return, unless the output is just going to be used by another function. 


lettersampler()

##############################################

## Looking up objects and their defined values. 
# R can look a level up; all objects don't have to be defined inside function.

# In this first case, all objects are defined within the function. 

f <- function(){
  x <- 1
  y <- 2
  c(x,y)
}
f() # See result.
rm(f) # rm is for 'remove'. 

#  A function looking one layer up, meaning outside the function. 

x <- 99988
ww <- 9843
g <- function(){
  y <- 2
  print(c(x, y))
}
g() # See result. 
rm(g) # Remove object. 

##########
### For loops. 

## commands inside loops: for, while, repeat, break, next. Also if. 

### Example 1. 

x <- 1:10
z <- matrix(rep(NA, 10), ncol = 2)

x; z

# seq_along() is not the ONLY way to set up.  
# Check against length() and note the difference. 

for(i in seq_along(x)){   # Instead of seq_along, can be length as well.
  z[i] <- i^2
  print(z)
} 

# Length can produce funky errors if x happens to be of length zero. 
# Just use seq_along. 

for(i in length(x)){  
  z[i] <- i^2
  print(z)
} 

z; class(z) # Numeric vector with all 10 values. 

### Example 2. A for loop inside and outside a function. What's the difference?

x <- c(2,5,3,9,8,11,6)
count <- 0

for (val in x) {
  if(val %% 2 == 0)  count = count+1
}
print(count)

# Note that count changes every time you keep running the for loop. 

# Note: you'll get a different result if you stuff all that in a function. 
# The 'fresh start' principle of functions. 

digitz <- sample(1:40, 1000, replace=TRUE)
digitz

count_func <- function(x){
  count <- 0
  for (val in x) {
    if(val %% 2 == 0)  count = count+1
  }
  print(count)
}

count_func(digitz)

# Note: now count_func always the same value. 
# Create a vector filled with random normal values

## Coding your own dice game or deck of cards!
#
## Simple function rolling two dice. Note: NO arguments defined. 

roll <- function(){
  die <- 1:6
  dice <- sample(die, 2, replace = TRUE)
  sum(dice) # Sum also produces a printed output. 
  output <- list(dice = dice, sumdice = sum(dice))
  print(output)
}

roll() # You need the parentheses to activate the function and run it. 

## Rewrite the above function, but add an argument. 
## Now it doesn't have to be six-sided dice. 

roll2 <- function(bones){
  dice <- sample(bones, 2, replace = TRUE)
  print(dice) # Need to print the results you want to see!
  sum(dice) # Sum also produces a printed output. 
}
roll2(bones = 1:10) 

#######################################################################
# Functional programming with apply-like functions from purrr. 

data(mtcars) # Example data. 

# Mapping from purrr.
# purrr maps apply functions to lists or atomic vectors. 

mtcars2 <- mtcars # Making a copy. 
glimpse(mtcars)

# Below the map function is used on data frames, which are also lists.  

head(mtcars)

# Using map to iterate through the variables to see their class. 
# This works because data frames are also lists. 

class(map(mtcars, is.numeric))

# The following works well because all variables start as numeric in mtcars. 

mtcars %>%
  map(., quantile) # Give 0-25-50-75-100 percentile.  

# The ending for the map function is based on what the output should be. 

mtcars %>%
  group_split(am) %>% # Splitting into two data frames by automatic v. manual. 
  map(., `[`, "cyl") # Subsets, keeping only the cylinder variable. 

# A slight change to the map function allows us to change the output. 

mtcars %>%
  group_split(am) %>%
  map_dfr(., `[`, "cyl") # Pulls them back together into one data frame. 

# Replace above with your own function!

# Write your own quantile function! Then apply it with map() function. 

quant2 <- function(x){
  quantile(x, probs = c(.1, .5, .9))
}

head(mtcars)

confind_cars <- mtcars %>%
  map_dfr(quant2) # Your own custom function can be applied. Save to data frame. 

confind_cars

##########
# EXPAND ON THIS. 

# Apply functions are equivalent to map functions in most ways. 

sapply(mtcars, mean) #Vector-based. Returns a vector. 
lapply(mtcars, mean) # produces a list. 
mapply(rep, 1:3, 3:1) # sapply with multiple elements. 

# Continued use of map. 

mtcars %>%
  group_split(cyl) %>% # Split the data into three groups. 
  map(~ lm(mpg ~ wt, data = .)) %>% # Run some OLS models. 
  map(summary) %>%
  map_dbl("r.squared") # type of map shows what type of object it will return.

# Let's break it down bit by bit. At least the first part. 

# Split dataset into a list. 

mtcars %>%
  group_split(cyl)

# Use map to apply a function. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) # Apply the linear regression function to a list. 

# You can then continue to apply functions. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) 

# You can keep adding functions to apply. 

mtcars %>%
  group_split(cyl) %>% 
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared") 


carmodel <- lm(mpg ~ wt, data = mtcars)
summary(carmodel) # The summary object is a list. 
length(summary(carmodel)) # It has 11 elements. 


# And so on and so on...

################################################################################

# Data Visualization: 
# GIFs using the magick package. 
# Robert A. Cooper

# Magick. 

library(magick)

# First, you need to know import and export functions. 


monkeycat1 <- image_read(file.choose())
monkeycat2 <- image_read(file.choose())

monkeycat1
monkeycat2

# Working with images. Cropping. 

monkeycat1 %>%
  image_crop("250x250+330")

# You can scale down by pixels. 

image_scale(monkeycat1, "500")
?image_scale

# You can rotate. 

image_rotate(monkeycat1, 45)

# You can flip. 

image_flip(monkeycat1)


# We can manipulate the colors of our images through image_modulate.

image_modulate(monkeycat1, brightness = 150, saturation = 150, hue = 150)

# We can change base color with image_colorize.

image_colorize(monkeycat1, color ="blue", opacity = 50)


# You can fill certain parts of the image, if you know pixel location. 

image_fill(monkeycat1, "blue", point = "+500+360", fuzz = 20) 

# You can add text wherever you like. 

bluetail <- image_fill(monkeycat1, "blue", point = "+500+360", fuzz = 20) %>%
  image_annotate(., "Hey, why is my tail blue??", size = 30, color = "white", location = "+100+55")

bluetail

bluetail2 <- image_fill(monkeycat1, "blue", point = "+500+360", fuzz = 20) %>%
  image_annotate(., "Hey, why is my tail blue??", size = 30, color = "white", location = "+10+10")

bluetail2

# Technically speaking, these image transformations are done in layers. 
# You can compress the layers into one with image_flatten.  

image_flatten(bluetail)

# Let's make a GIF. Save your basic image. 

mc1 <- monkeycat1

# You can repeat/replicate an image just like you can in other vectors, with rep(). 

mc_gif <- rep(mc1, 3)
length(mc_gif)

mc_gif

# You can recode/replace just like data vectors. 

mc_gif[2] <- bluetail

mc_gif

# Let's add one more. 

mc_gif2 <- c(mc_gif, rep(monkeycat2, 3))
mc_gif2

# To write/save your new GIFs, use the image_write function. 

image_write_gif(mc_gif, "monkeycat.gif")


###############################################################################
# You can also pull GIFs from elsewhere and manipulate them. 

img_doc <- "https://media.giphy.com/media/rULGb0wtaeAEM/giphy.gif" #Dr Emmett Brown. 
img_cat <-"https://media.giphy.com/media/mlvseq9yvZhba/giphy.gif" # cat.
img_bomb <- paste0("https://media.giphy.com/media/wFmJu7354Csog/giphy.gif")
img_bron <- paste0("https://media3.giphy.com/media/3o72F9VC4WjFDCPTag/",
                   "giphy.gif?cid=ecf05e470ceb6cd8542e8c243d0e0a2282c3390e5c",
                   "72fd17&rid=giphy.gif")

bron <- magick::image_read(img_bron)
bomb <- magick::image_read(img_bomb)
cat <- magick::image_read(img_cat)
doc <- magick::image_read(img_doc)

# Show image. Or reverse of image. 

doc
bomb # Let's say we want to rescale the size of the image. 
cat
bron

length(doc)
length(bron)

bomb2 <- image_scale(bomb, "490x280")
bomb2.1 <- image_scale(bomb, "x288") 
?image_scale

bomb2
bomb2.1

# rev(img) is a fun option for reversing GIFs. 

rev(1:10)
rev(bron)
rev(bomb)

# LeBron plus bomb. 

dunkbomb <- c(bron, bomb2.1) # Combine two GIFs back-to-back.
dunkbomb

length(dunkbomb)

dunk_sub <- dunkbomb[1:5]
dunk_sub


dunkbomb[30:34] <- image_annotate(dunkbomb[30:34], 
                                  "WOW", 
                                  size = 70,
                                  gravity = "southwest", 
                                  color = "green")

dunkbomb

# Dr. Emmett Brown plus a-bomb. 

docbomb <- c(rep(doc, 2), bomb)
docbomb # Some editing required here for dimensions. 

# To save your GIFs. 

image_write_gif(dunkbomb, "/Users/robertcooper/Desktop/dunkbomb.gif")


# There are a host of image transformations available to you. Way too many to show. 

cat

image_scale(cat, "300")
image_charcoal(cat)

# GIFs broken down by frame, so you can do some neat pause and reverse stuff. 

length(bron)
bron

# Combo of slowing down, reversing, etc. Very Missy Elliott. 

bron[c(1:44, rep(c(rep(44, times = 30), 43:30, 
                   rep(30, times = 5), 31:43), 
                 times = 2), rev(1:44))]


bron[rep(1:44, each = 4)]

# You can slow down your GIF. 

bron[c(1:30, rep(31:44, each = 3))]

# Lots of fun to be had here. 
# The LOGIC is the same with animated plots. 
# Functionally, they are essentially slide decks of images. 




