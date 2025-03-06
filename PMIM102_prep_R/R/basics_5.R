# Exercise: write a condition that distinguishes between the BMI levels, underweight, 
# healthy, overweight and obese.
# Hints:
#  1. Define a variable bmi and compare it to the threshold values in a specific order.
#  2. You can use } else if () {.

bmi <- 26

bmi_test <- if (bmi < 18.5){
  print('Underweight')
} else if (bmi < 25) {
  print('Healthy')
} else if (bmi < 30 ) {
  print('Overweight')
} else if (bmi > 30) {
  print('Obese')
}

# if
# one important function of if is parameter checking
# e.g. we might want to check the values provided in the bmi_calc function

bmi_calc <- function(weight_kg, height_cm){
  bmi_calc <- weight_kg / ((height_cm / 100) ^ 2)
  return(bmi_calc)
}

bmi_calc <- function(weight_kg, height_cm){
  if (weight_kg < 10) {
    return(-1)
  }
  bmi_calc <- weight_kg / ((height_cm / 100) ^ 2)
  return(bmi_calc)
}
bmi_calc(80, 180)                       # Should be 24.6.
if (bmi_calc(8,50) < 0) print("Weight provided is too low.")

# Exercise: write a function that, given a BMI value, returns a string indicating it
# fits which of the BMI levels, underweight, healthy, overweight and obese.
# Then apply it to the starwars BMI data frame from Notebook 3.
# Hints:
#  1. Define a variable bmi and compare it to the threshold values in a specific order.
#  2. This is a bit of a trick question to illustrate a common problem - it uses 
#     conditions but not if() statements.

library(magrittr)
bmi_level_1 <- function(bmi){
  if (bmi < 18.5) level = 'underweight'
  else if (bmi < 25.0) level = 'healthy'
  else if (bmi < 30.0) level = 'overweight'
  else if (bmi < 40.0) level = 'obese'
  else level = 'very obese'
  return(level)
}
bmi_level_1(22)
bmi_level <- function(bmi){
  bmi[bmi >= 40.0]='very obese'   # Must do this first as all text entries are considered > numbers.
  bmi[bmi < 18.5]='underweight'
  bmi[bmi < 25.0]='healthy'
  bmi[bmi < 30.0]='overweight'
  bmi[bmi < 40.0]='obese'
  return(bmi)
}
bmi_level(22)
library(dplyr)
starwars_bmi <- starwars %>%
  mutate(BMI=mass/((height/100)^2))
starwars_bmi <- starwars_bmi %>%
  mutate(BMI_Level=bmi_level(BMI))
head(starwars_bmi)

# for loops
# when you know how many times you want to do something, you use a for loop
# you need to assign a series of values to be used to a variable known as the loop variable
# R will start with the first value, set the loop variable to that value and run the code
# in the associated block (the part in the curly brackets after the for statement)
# R will then repeat this for all of the values
#e.g. to create a list of the multiples of a number
multiple <- 6
for(i in 1:12){
  print(i * multiple)
}

# Exercise: create a times-table output from 1x to 12x.
# Hints:
#  1. You can use cat(n, ' ') to print values in a row with a space between (there are
#     ways to make the spacing equal but that may be better saved to later.
#  2. You can have another for loop inside a for loop as long as the inner loop's code 
#     is completely contained in the out loop.
#  3. You will have to consider how to get the code to display the rows on separate lines.

for(i in 1:12){
  for(j in 1:12){
    cat(i * j, ' ')
  }
  cat('\n')
}

# while loops
# Sometimes, you will want to run some code until all something happens - you run out 
# of data, you find the data you are looking for - and you don't know exactly when this
# will happen. In that case, you use a while loop and R will check everytime before it
# starts the loop code that the condition you specify is TRUE: if it is the loop code
# is run, if not, the loop is finished and R moves on to the code after the loop code.
# e.g. we could write a function to roll one or more dice until it throws a particular value

roll_until_n <- function(n){
  roll <- as.integer(runif(1) * 6 + 1)
  while(roll != n){
    print(roll)
    roll <- as.integer(runif(1) * 6 + 1)
  }
}
roll_until_n(6)

# Exercise: modify the dice rolling program to count and save the values.
# Do this in two steps: first and a count and print() how many rolls were required 
# after the loop completes then add the memory mechanism.
# What does the histogram look like?
# Sometimes the histogram is not very useful and sometimes there is an error. How would
# you fix these problems?
# Hints:
#  Create a vector, and add the values to the vector as you roll them and, at the end, 
#   return the vector
roll_until_n <- function(n){
  v <- c()
  roll <- as.integer(runif(1) * 6 + 1)
  while(roll != n){
    v <- c(v, roll)
    print(roll)
    roll <- as.integer(runif(1) * 6 + 1)
  }
  return(v)
}
rolls <- roll_until_n(6)
hist(rolls)

# repeat loops
# an underused form of loop which doesn't have a loop variable or a starting conditions
# but repeats the loop until you tell R to exit the loop 
# you do this with the break command
# useful for situations that don't fit neatly into the for() or while() forms 
# for example, some language have a do {} until() loop structure 
# Going back to the original dice rolling function, you'll notice that we had to roll 
# the dice twice in the code which is not very neat (if we wanted to change to a
# twenty-sided dice, we would have to change two lines of code). We could implement this 
# as a repeat loop:
roll_until_n <- function(n){
  repeat{
    roll <- as.integer(runif(1) * 6 + 1)
    if (roll == n) break
    print(roll)
  }
}
roll_until_n(6)
# repeat loops seem a lot tidier and have the flexibility that you can exit the loop 
# at different stages for different reasons if you need to. This can be useful but it 
# can also lead to difficult to understand code.

# Exercise: create a vector with all the prime numbers from 1 to 10.
# Two hints:
#  1. create a vector with all the numbers then remove anything divisible by a smaller 
#     number (not the most efficient - can you think of an improvement?).
#  2. how do you check that a number is divisible by another number ?
#  3. How long does this take to get lots of prime numbers?

n <- 10
x <- seq(1, n, 1)
s <- sqrt(n)
for(i in 2:as.integer(s)){
  x <- x[x == i | x %% i != 0]
}
print(x)
