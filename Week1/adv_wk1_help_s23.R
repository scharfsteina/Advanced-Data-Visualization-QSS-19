# Advanced Data Visualization
# Prof. Robert A. Cooper
# Dartmouth College, Spring 2023

################################################################################
## Week 1 Review Material. 

## DAY 1. 

##  BREAK TO R #1. 

# First, let's open a new project. 

# Next, let's install a package. Might as well be tidyverse. 

# R has a LOT of code already in its base programming. 

# That said, there is a lot of code out there to be installed as 'packages'. 
# You install said code from CRAN repository,
# or maybe Github if it's a developer version. 

# There are at least 3 ways to install a package.

install.packages("tidyverse") # One way. 

# Go to Tools -> Install Packages # Way 2. 
# Go to Packages tab in plotting window, click Install, type package name. # Way 3.

# Once you have the package downloaded, you must activate/attach it. 

library(tidyverse)

# Next, let's practice creating an object. 

banana <- 15

banana
print(banana)

banana*7

# To show what's contained in the object, type it and enter or print() it. 

banana
print(banana)

# You can create multiple objects and interact them. 

is.vector(apple)

apple <- 6
banana * apple

# Warning: you can easily overwrite objects. Easy mistakes made here.
# Lesson: more often than not, it's a bad idea to overwrite objects.  

apple <- 8

# Objects. 
 
# Objects are containers with labels. Inside is some piece of information. 
# Things in the R universe have classification, like plants v. animals. 
# Certain functions and forms work with certain classes and not others. 

# First classification distinction: homogeneous or heterogeneous?

# Some objects are all the same inside. Some can be made up of different types.

# Homogeneous: vectors, matrices, arrays.
# Heterogeneous: data frames, lists. 

# The assignment operator. '<-' or '=' (I suggest '<-')

# Vectors. "Atomic" vectors.  

# Vectors are 'flat' or one-dimensional. They can have length, but not
# more than one dimension. 

# Types: double/numeric, integer, factor, character/string, logical. 
# Rare types: complex, raw. Don't worry about it. 

a <- 4 # This is actually a vector of length 1. 

# Again, the mildest difference between typeof() and class(). 

typeof(a) # "double", same as below.
class(a) # "numeric", same as above. 

b <- 3
d <- 5
e <- 7

# You can combine vectors with the c() function ("combine" or "concatenate")
# You can also create vectors with the c() function. 

# Vectors stay 1D, even when nesting. 
  
numbers <- c(a, b, d)
numbers
class(numbers)

numbers2 <- c(c(a, b), c(d, e)) # Note: c(a, b, d, e) would have done the same. 
numbers; numbers2
numbers2

# Vectors can take names with the names() function.
# Note: the `names' of the vector are empty and are taking the assignment.  

names(numbers2) <- c("a", "b", "d", "e")
numbers2 # Now, when, I print numbers2, I see the name labels as well. 
class(numbers2)

# Integer vector. The 'L' is the way to indicate integer/no decimal. 

int1 <- 3L; int1

int2 <- c(5L, 29L); int2
class(int2)

int3 <- c(int1, int2); int3
typeof(int3); class(int3)

# Character/string/text vector. 

dart <- c("Dartmouth", "rules"); dart
typeof(dart)
class(dart)

# Logical vector. TRUE/FALSE.
# Understanding these is very important to data transformation. 

logic1 <- TRUE
logic2 <- c(FALSE, FALSE, TRUE, TRUE, FALSE)
logic3 <- c(logic1, logic2)
logic3
class(logic3)
typeof(logic3)

# Logical operators. (When the end results in a TRUE/FALSE vector)
# You can use rules with comparison/logical operators to your advantage.

numbers2

numbers2 > 3
numbers2 != 3

##  Factors. 
# Factors are a combination of integers and character strings, in a sense. 
# They exist for categorical variables. 
# They have integer values. They have labels. 
# Note: they have predetermined acceptable levels.
# If you do not set the predefined levels, all unique values will become levels.

# letters is a vector of the English alphabet. 

letters
LETTERS

letterz <- sample(letters, 100, replace = TRUE)
letterz

letfact <- factor(letterz)
letfact # Note the lack of quotations. No longer a string variable. 

# We can check the levels of the factor with 'levels' or with 'attributes'

levels(letfact)
attributes(letfact)

class(letfact) # You see and work with it as a 'factor' with a label.
typeof(letfact) # R stores it as an integer in the memory.

# If we instead define the set of acceptable levels, the variable changes. 

letterz
letfact2 <- factor(letterz, levels = c("a", "b", "c", "d"))
letfact2

# If we want an ordered factor, add the ordered = TRUE argument.
# When you print an ordered factor, you can see the levels and their order. 

letfact3 <- factor(letterz,levels = c("a", "b", "e", "d", "c"), ordered = TRUE)
letfact3

letfact3

# Depending on the earlier sample function, one will be true, the other false. 

letfact[8]
letfact[12]

letfact3[8] > letfact3[12] 
letfact3[8] < letfact3[12] 

summary(letfact3)
table(letfact3)

## COERCION. VERY important to understand.##  

# When two objects of different classes are combined in a vector, 
# one is 'coerced' into the class of the other. 

# Best way to figure out the rules? Two-way competitions.
# Rock-paper-scissors style. 

vect1 <- c(numbers, int3); class(vect1) # Numerics beat integers. 
vect1 # Can't really 'see' the difference, but it's there. 

vect2 <- c(numbers, dart); class(vect2) # Character/strings beat numerics. 
vect2

vect3 <- c(int3, dart); class(vect3) # Character/string beats integers.
vect3

vect4 <- c(logic3, numbers); class(vect4) # INTERESTING! Numeric beats logicals.
vect4

vect5 <- c(dart, logic3); class(vect5) # Character beats logicals. 
vect5

vect6 <- c(dart, letfact); class(vect6) # Character beats factors. 
vect6

vect7 <- c(numbers, letfact); class(vect7) # numeric beats factors. 
vect7

vect8 <- c(int3, letfact); class(vect8) # integer beats factors. 
vect8

vect9 <- c(logic3, letfact); class(vect9) # INTEGER wins!!!! What???
vect9

# Just so we are clear what just happened. 

logic3
letfact

# Lesson: character will coerce ANYTHING. And integers often linger beneath. 

####################################################################
##### Matrices. 

mat1 <- matrix(1:9, nrow = 3, byrow = FALSE); mat1

# Matrices have two dimensions. Row and column. 
# Matrices are defined by row x column and whether you fill by row or column. 

dim(mat1)

# They cannot take different types of data or classes of objects. 
# They CAN BE different types of objects, as long as each element is the same.

mat2 <- matrix(letters, ncol = 2, byrow = TRUE)
mat3 <- matrix(letters, ncol = 2, byrow = FALSE)


####################################################################
##### Data Frames. 

# Let's call a dataset already in base R. 
data("mtcars") # Use the 'data' function to call native R data sets.

# R has many base datasets to practice with. I encourage you to use them.

# We can look at the data with 'head' or 'glimpse' or 'str' 

# Recall class and typeof can occasionally be different. 

class(mtcars)
typeof(mtcars)

head(mtcars)
glimpse(mtcars) #str(mtcars) # stands for 'structure'

# Use the double colon to specify exactly what package a function is coming from.
# This is most helpful when two packages have functions with the same name.
# When this happens, one package function overtakes the other. "Masking."

# R has many base datasets to practice with. I encourage you to use them.

# Let's work with mtcars. 

head(mtcars)

# First, let's work through some base R subsetting of a data frame. 

# Using hard brackets, we can subset by row or column, as we see fit. 

mtcars[3,] # row by column. Thus, row on the left of comma, column on the right. 

# Data frames can take row and column names as identifiers. 

head(mtcars)
dim(mtcars)

mtcars[4,] # Extracts the fourth row. 
mtcars[, 4] # Extracts the fourth column's values
mtcars[3, 7] # The quarter-mile time for the Datsun 710. 

mtcars[,"disp"]
mtcars["Datsun 710",]
mtcars["Datsun", 3]

mtcars$disp[7]

rownames(mtcars)
colnames(mtcars)

#######################################################################
# Lists. 
# Lists have length, but do not have dimension. 
# Lists can take many different types and classes of objects. 

objects()

list1 <- list(mtcars, dart, apple, banana, vect5, mat1)
str(list1)

length(list1)
dim(list1) # returns NULL. 

# What happens if we just print it?

list1
list1[2] # points to the list element. 
list1[[2]] # gets me inside the list element. 

list1[[2]][2] <- "is the best"
list1

dart[1] <- "dart" # recoding. 

list1[[1]][3,4]

#####

# Now we introduce plotting via ggplot2 and the tidyverse.
# In addition, I will introduce a few more pieces of code for data transformation, etc.

# Some terms: aesthetics, mapping, themes, fill v. color, histograms,
# bar plots, line plots, etc. 

# First, an important bit of code for data work: the pipe operator

# Load the mtcars data. Already in base R. 

data("mtcars")
head(mtcars)

# Piping with magrittr (part of the Tidyverse).
# Piping replaces turducken-style coding.
# Instead of stuffing functions inside functions. 
# You'll see it below. 

# Here is some sad, turducken coding: 

summarize(group_by(mutate(mtcars, newwt = wt*1000, factcyl = factor(cyl)), factcyl), mean_wt = mean(wt))

# Here is some awesome, piped coding:

mtcars %>%
  mutate(newwt = wt*1000,
         factcyl = factor(cyl)) %>%
  group_by(factcyl) %>%
  summarize(mean_wt = mean(wt))

# The difference in the options above is pretty darned obvious.

# Tibbles. Just a different way to present a data frame or data table. 
# Compare mtcars to car_tibble just to see the difference. 

car_tibble <- as_tibble(mtcars); car_tibble

mtcars
car_tibble

#  Filtering. How do I know the inputs for the filter are working??

mtcars %>% # The '%>%' is the pipe operator. 
  filter(cyl == 6) # Subset the data to cars with 6 cylinders only. 

mtcars %>% # The '%>%' is the pipe operator. 
  filter(cyl == 6 & gear == 4) # Subset the data by 2 variables instead of 1. 

mtcars %>%
  filter(wt > 2.8)

# Arranging. This just changes the view of the data. 

mtcars %>%
  arrange(wt) # Arrange data in ascending order of weight in tons. 

mtcars %>%
  arrange(desc(wt))

# Mutate. 
# Mutate is how you create a new variable in the tidyverse.
# Mutate attaches the new variable to the end of the data frame/tibble.
# MUST: name new variable, then define it. See below. 

mtcars %>%
  mutate(mean_weight = mean(wt), # New variable created with mean weight of all cars. 
         fact_cyl = factor(cyl)) # New variable created turning 'cyl' into a factor variable. 


# Note: I did not save as a new object above. Thus, the code is temporary and unsaved.
# If I open mtcars again, I will not see these new variables. 

mtcars2 <- mtcars %>%
  mutate(mean_weight = mean(wt), # New variable created with mean weight of all cars. 
         fact_cyl = factor(cyl)) # New variable created turning 'cyl' into a factor variable. 

mtcars; mtcars2


# Select.
# Select subsets your data by choosing columns to keep/discard. 
# Select works with `tidyselect` functions in addition to variable names. 
# Select can also be used to rearrange your variables. 

mtcars2 %>%
  select(fact_cyl, wt, mpg) # Just subsetting by these variables. 

# We can also use tidyselect helpers. 

mtcars2 %>%
  select(contains("cyl")) # Subset using a tidyselect helper. Like a stringr for variable names.

mtcars2 %>%
  select(starts_with("m")) # Another tidyselect helper. 

# We can use the tidyselect helper everything() with select to rearrange the whole data frame.

mtcars2 %>%
  select(wt, mpg, fact_cyl, mean_weight, everything()) # Tell it which goes first, followed by everything. 

# group_by.
# group_by sets a quiet rule. 
# only a tibble will show you if a grouping rule has been set.
# Otherwise, you have to follow it with a summarize() or mutate() to see it work. 

mtcars2 %>%
  group_by(fact_cyl)

mtcars2 %>% # Without grouping, no line at top identifying number of groups.
  tibble()

# group_by will produce different results, depending on mutate v. summarize.

mtcars2 %>%
  group_by(fact_cyl) %>%
  mutate(mean_mpg = mean(mpg, na.rm = TRUE))

mtcars2 %>%
  group_by(fact_cyl) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE))

# group_by can group on multiple variables.

mtcars2 %>%
  group_by(fact_cyl, am) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE))


# summarize. 
# summarize() creates new variables, but it also kicks you out of the data frame/tibble.
# summarize creates a new tibble altogether.
# What does summarize bring forward into the new tibble?
# ONLY: (1) the variables you group on and (2) the new variables you create. 

mtcars2 %>%
  group_by(fact_cyl, am) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE),
            max_mpg = max(mpg, na.rm = TRUE),
            min_mpg = min(mpg, na.rm = TRUE))


##PLOTTING AND GGPLOT2. 

# ggplot2 works differently from base R. 

# Key: THE AESTHETIC IS THE MOST IMPORTANT THING TO KNOW. 
# Key 2: THE GEOM IS THE NEXT MOST IMPORTANT THING TO KNOW. 

# The aesthetic connects the variation in your variables to you plot.
# It 'maps' a variable on to some element of your plot: size, color, line, etc. 
# If you make an arbitrary choice in a plot (line color = red, for example)...
# then that does NOT go inside the aesthetic. 
# ALL aesthetics are tied to variables. 

# On geoms. Geoms are all the geometric/physical ways our data can be expressed. 
# The list of possible geoms expands with each package added. 
# In this course, we will focus on a subset, but feel free to explore others.

# https://ggplot2.tidyverse.org/reference/

# Example 1: Simple plot of car weight against fuel efficiency.

# Simple scatterplot, ggplot-style.
# Kind of ugly, but just because there are so many features not yet discussed.
# In this workshop, we start simple and unadorned and slowly add features.

glimpse(mtcars)

mtcars %>%
  ggplot(., aes(x = wt, y = mpg)) +
  geom_point()

# Let's try a bar plot for fun. 
# Count up the number of cars with 4, 6, and 8 cylinders. 
# geom_bar does the count automatically for you!

mtcars %>%
  ggplot(aes(x = cyl)) +
  geom_bar()

# NOTE: X-axis looks a little odd. Why do you think that is? 

glimpse(mtcars)

# First real lesson in object classes!
# How do we fix this?

mtcars %>%
  ggplot(aes(x = factor(cyl))) +
  geom_bar()

# Line plot. 
# For these to be most useful, sometimes we might want to add a THIRD aesthetic. 

# The following example looks odd, on purpose. 

mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_line()

# Let's go back to scatterplot, but let's keep the third aesthetic. 
# This is probably the most sensible format for visualization.

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point()

# 'Faceting'. We could also break up the plot into multiple windows.
# There is little difference between facet_grid and facet_wrap.
# Won't really see it until you have more categories. 

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  facet_grid(cols = vars(cyl)) 

# You can facet by multiple variables. Buyer beware if you go beyond two!

mtcars %>%
  ggplot(aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point() +
  facet_grid(cols = vars(cyl), rows = vars(am)) 

# Histograms and frequency polygons. 
# Characterize individual continuous variables, but in buckets or bins. 
# The plot below will be more appropriate with larger datasets. 

mtcars %>%
  ggplot(aes(x = mpg)) +
  geom_histogram(bins = 10) 

# GEOMS. General note on geoms. Wide variety. MANY not covered here. 

df1 <- data.frame(x = c(1, 7, 3),
                  y = c(13, 4, 17),
                  label = c("a", "b", "c"))

# Different geoms, same data. 
# Exercise: turn one geom on, and comment out others. Then rotate. 
# *Don't forget to comment out the plus signs as well, when needed.

# Subtle difference between geom_path and geom_line.
# Path connects in the order of the x-axis values. 
# Line connects in the order of the data. 

df1 %>%
  ggplot(aes(x, y, label = label)) +
  #geom_point() #+
  #geom_text() # +
  #geom_area() # +
  geom_path() # +
#geom_line() # +
#geom_step() #+
#geom_polygon()


# Geom example 2. Data analysis and uncertainty. 
# When producing a point estimate for some data analysis...
# you also produce some estimate of uncertainty.
# To make a nice plot of these, you need 4 pieces of information: 
# 
# 
# There are a number of geoms for this: pointrange, errorbar, linerange, etc.
# 

df2 <- data.frame(x = c(1, 2, 3),
                  y = c(10, 4, 7),
                  ylow = c(9.5, 3.5, 6.5),
                  yhigh = c(10.5, 4.5, 7.5),
                  label = c("a", "b", "c"))


ggplot(df2, aes(x = x, y = y, ymin = ylow, ymax = yhigh)) +
  #geom_linerange() +
  geom_pointrange() 
# geom_errorbar()


## A beginning with stringr.

# Filtering with string variables.

# You can filter with stringr functions, so long as the input with filter is a... logical!
# Thus, filter works with str_detect.

# data %>%
#  filter(str_detect(variable, "pattern"))

# But, you cannot, for example subset between two patterns.
# For that, you can use base R subsetting (brackets with colon), but 
# that doesn't work with the filter/str_detect combination. 

# Instead, you'd want something with row/index number. 
# For that you'd use something like slice() from dplyr. 
# Paired with a stringr function that returns index number/row number: str_which.
# str_which is a dplyr version of which. 

# Using our base R mtcars database for most of the practice examples. 

data("mtcars")

mtcars2 <- mtcars %>%
  mutate(carmodel = rownames(mtcars))

# Now we can practice a little with stringr functions on data. 

# Note: many stringr functions operate on a single variable/column. 
# These do no stand alone as functions to pipe directly into. 
# In this case, you want to mutate and use str_... to define the new variable. 

# The following does not work.

mtcars2 %>%
  str_which(carmodel, "Toyota")

# The following does work, weirdly (with a warning), but on ALL variables. 

mtcars2 %>%
  str_replace_all("Toyota", "giraffe")

# Two ways to filter on a str_detect function. Doesn't really matter which. 

mtcars2 %>%
  mutate(toyotaz = str_detect(carmodel, "Toyota")) %>%
  filter(toyotaz) # If you don't provide a rule, filter will keep TRUEs. 

mtcars2 %>%
  filter(str_detect(carmodel, "Toyota"))


########################################################################

