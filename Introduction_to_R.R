# To download R
# https://cloud.r-project.org/

# To download RStudio IDE
# https://rstudio.com/products/rstudio/download/

# Clear environment variable and console
rm(list = ls())
cat("\014")

# Installs useful packages
# Comment out after you have installed these packages (R will re-install them regardless)
# install.packages('tidyverse')
# install.packages('data.table')
# install.packages('lubridate')
# install.packages('ggplot2')

# Loading packages into environment
library(tidyverse)
library(data.table)
library(lubridate)
library(ggplot2)

# character
# logical
# integer
# numeric
# complex

# Assignment
dog1 <- 'chihuahua'
dog2 = 'chihuahua' # differs between environment scope (for simplicity, always use <- when assigning to a variable)

# Basic Arithmetic
1 + 2 # addition
6 - 3 # subtraction
9 * 7 # multiplication
25 / 6 # division
25 %% 7 # modulus (remainder)

vec <- c(138, 124, 132, 140, 140, 146, 134, 146, 134, 140)
vec * 33

# basic comparison statements
# == equal to 
# != does not equal to
# & and (vector)
# | or (vector)
# && and (singular, first element only)
# || or (singular, first element only)
((-1:5) >= 0) & ((-1:5) <= 0)
((-1:5) >= 0) && ((-1:5) <= 0)

# In R, the simplest object is a vector.
# They can be different classes / types (e.g. character, numeric)
c(1, 7, 23.3) # Creates a vector with numbers 1, 7, and 23

# Alternatively, if you want a sequence of numbers
5:25

# Additional methods of creating vectors:
seq(5, 25, by=0.25)
rep(1:5, times=5)
rep(1:5, each=5)

# Vector Functions
sort(seq(5, 25, by=0.25), decreasing = TRUE)
rev(seq(5, 25, by=0.25))
table(rep(1:5, each=5))
unique(rep(1:5, each=5))

# Element selection within a vector
vec <- seq(5, 25, by=0.25) # Create a vector and assign it with the name `vec`

# Unlike other languages (e.g. C, Java, python), R uses the traditional mathematical notation of indices starting at 1.
vec[0] # should return numeric(0) because nothing exists at that index location
vec[3] # show me the 3rd element in the vector
vec[-1] # show me everything except the first element in the vector
vec[5:7] # show me elements 5 to 7 (inclusive on both ends)
vec[c(5, 7)] # show me elements 5 and 7 (not including element 6)
vec[vec == 10] # show me elements in the vector equal to 10
vec[vec == 26] # show me elements in the vector equal to 26 (should be empty because we only made a vector up to 25)
vec[vec < 15] # show me elements less than 15
vec[vec %in% c(1, 3, 5, 7)] # show me elements that are inside the vector c(1, 3, 5, 7)

# Basic for loop structure
for (variable in vec) {
    print(variable)
}

# Basic while loop structure
i <- 1
while (i <= length(vec)) {
    print(vec[i])
    i = i + 1
}

# Basic if-else structure
length(vec)
if (length(vec) < 25) {
    print('size of vector is less than 25')
} else if (length(vec) > 25) {
    print('size of vector is greater than 25') # we should expect this outcome
} else {
    print('size of vector is 25')
}

# Basic function structure
create_string <- function(name, age, title) {
    paste0('My name is ', name, ' and I am ', age, ' years old. I am an ', title, '.')
}
create_string('Josh', 24, 'Analytics Engineer')

# Plotting Example with GGPlot
gini <- fread('https://perso.telecom-paristech.fr/eagan/class/igr204/data/gini.csv', header = TRUE)
setnames(gini, 'GINI index', 'Country')
gini <- melt(gini, id.vars=c('Country'), variable.name="Years", value.name="GINI")
gini <- gini[!is.na(GINI)]
gini <- gini[Country %in% unique(Country)[1:5]]

ggplot(gini) +
    aes(x = as.Date(paste0(Years, '-1-1')), y = GINI, colour = Country, group = Country) +
    geom_line() +
    scale_color_hue() +
    theme_minimal()
