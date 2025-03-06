# Exercise: create a data frame with the means and standard-deviation of the first, 
# second ... tenth readings (i.e X1 ... X10).
# Hints:
#  1. Create vector for the means and standard deviations.
#  2. Combine them into a data frame.
#  3. Round the data.

n <- 10
patient_1 <- rnorm(n, mean=10, sd=1)
patient_2 <- rnorm(n, mean=12, sd=2)
patient_3 <- rnorm(n, mean=2, sd=1)
patient_4 <- rnorm(n, mean=5, sd=5)
patient_5 <- rnorm(n, mean=8, sd=3)
# t() converts a column vector to a row (transpose - see matrices).
tests <- data.frame(id=1, t(patient_1))
tests[2, ] <- data.frame(id=2, t(patient_2))
tests[3, ] <- data.frame(id=3, t(patient_3))
tests[4, ] <- data.frame(id=4, t(patient_4))
tests[5, ] <- data.frame(id=5, t(patient_5))
tests

means <- apply(tests[, 1:n+1], MARGIN=2, mean)
sds <- apply(tests[, 1:n+1], MARGIN=2, sd)
stats <- data.frame(mean=means, sd=sds)
rounded <- apply(stats, MARGIN=c(1, 2), round, 2)
rounded

# the tidyverse
library(tidyverse)
head(starwars)
# filtering the data
humans <- filter(starwars, species=='Human')
head(humans)
# selecting columns to simplify the data
small_starwars <- select(starwars, c(name, species, homeworld))
head(small_starwars)
# selecting columns and filtering the resulting dataframe
humans_smallstarwars <- starwars %>% select(c(name, species, homeworld)) %>%
                                     filter(species=='Human')
head(humans_smallstarwars)

starwars %>%
  select(c(name, species, homeworld, height, mass)) %>%
  filter(species=='Human') %>%
  summarise(n=n(), mean_h=mean(height, na.rm=TRUE), mean_m=mean(mass, na.rm=TRUE))

# arrange()
# sorts the data frame in order of the specified columns 
starwars %>%
  arrange(species, homeworld, -height)

# mutate()
# modifies the data in a column (by using an existing column name) or creates a new
# column of data
# replacing the NAs with 'none' in the hair colour column
starwars_hair <- starwars %>% mutate(hair_color=ifelse(is.na(hair_color), 
                                                       'none', hair_color))
# setting the value to 0 for the has.hair column if the entry's row has hair_color
# equal to 'none', and 1 otherwise
starwars_hair <- starwars_hair %>% mutate(has_hair=ifelse(hair_color=='none', 0, 1))
head(starwars_hair)

# group_by()
# configures a data frame so that it is grouped by the columns specified
# causes any future summary functions to be applied to the specified groups rather
# than the whole table
# use ungroup() so the data frame is not accidentally left in a grouped state
# mean height for the whole table
starwars %>% summarise(mean_h=mean(height, na.rm=TRUE))
# mean height for each species
starwars %>% group_by(species) %>% summarise(mean_h=mean(height, na.rm=TRUE)) %>% ungroup()

# Exercise: create column in the starwars data with the BMI data.
# Create a new BMI column. 
# Calculate the mean BMI for each species and for each sex. 
# You may also want to check the BMI variation by other factors. 
# If possible, ignore any species with fewer than 5 characters both on the grounds of 
#   statistical relevance and disclosure control.
# Hints:
#  1. Be careful with the calculation.
#  2. The n() function used in summarise() is the number of rows.

starwars_bmi <- starwars %>% mutate(BMI = mass / ((height / 100) ^ 2)) 
head(starwars_bmi)
starwars_bmi %>% group_by(species) %>% filter(n() > 4) %>% 
  summarise(n=n(), mean_BMI=mean(BMI, na.rm=TRUE)) %>% ungroup()
starwars_bmi %>% group_by(sex) %>% filter(n() > 4) %>% 
  summarise(n=n(), mean_BMI=mean(BMI, na.rm=TRUE)) %>% ungroup()

