# Functions
# A function is a unit of code that does something (performs a function) on some data. 
# You run a function by calling it by name and providing the names of the data it 
# needs as a list of arguments. We have already used several functions, class(), 
# is.na() etc. For example, in
#   print(any_number)
# print is the name of the function and any_number the argument (in this case there's 
# one argument, but there could be several).
# Another useful function for displaying output is cat(). It produces a concatentated 
# string of the parameters separated by a space, ' '). For example,
#   cat('The value of x is', x, '.')
# will produce, 'The value of x is 5 .'
x <- 5
cat('The value of x is', x, '.')
# cat is more useful than print when it comes to printing more than one thing at a
# time, but doesn't work well with more complex data structures such as lists and 
# data frames.

# Vectors
1:10
seq(1, 10, 2)
plot(sqrt(0:100))
plot(sin(seq(-pi, pi, pi/100)))

# Creating Your Own Functions
# To illustrate how a function works, we'll create one to produce the average of 
# two numbers, x and y. To create a function we use the keyword function and assign
# the function to the name we specify. We provide a list of arguments and then, 
# inside a pair of curly brackets, { and } we provide the code. When the function 
# is finished we need to provide the result which we do with the return function.
average <- function(x, y) {
  av <- (x + y) / 2
  return(av)
}
average(4,5)
# NOTE: that we didn't need to say print(average(3,4)) to get R to display the result,
# R assumes that's what we have wanted anyway. But if you are including the code in 
# a script file and running the file with source, you will need to add the print() 
# statement as R ignores all output when sourcing.

# NOTE: we can use operators in the same way if we really want to:
`+`(1, 2)

# When you type the function at the R prompt, you or the prompt are the 'caller'. 
# If you assign the output of the function to a variable, it becomes the caller and 
# the value is stored in the variable. You can use a function-call as an argument to 
# another function.
# 1. You don't need the return() call, R returns the current result to the caller 
#    of the function. This isn't necessarily a good thing and I would always 
#    recommend using return() to make things clear.
# 2. You can set a default value to an argument when you define the function using 
#    the = operator (this is the difference between = and <-).
# 3. If you don't specify the argument names when you call the function, they are 
#    assigned in the order given (in this case, the first argument is 'x' and the
#    second 'y').
# 4. If you do specify the names (again using =) then you can do so in any order 
#    (but you need to specify all the remaining names you wish to use once you have 
#    specified one).
# 5. If you don't provide an argument for which there is a default, the default 
#    will be used.

# Note that # indicates a comment and R ignores the rest of the line.

average <- function(x, y=0) {
  (x + y) / 2           # Try changing this to minus to make the values of x and y matter.
}
av <- average(3, 4)       # Set av to the average.
av                        # Print out av.
average(x=4, y=3)         # Specify the arguments explicitly.
average(y=4, x=3)         # Order doesn't matter.
average(3)                # We don't need the default argument.
average(x=3)
sum <- function(x, y) {   # Create another function so that ...
  x + y
}
average(sum(4, 5), 6)     # ... we can use a function call as a parameter.

# So we can revisit cat() now which has a default parameter, sep which is a space. 
# If we want a different separator, we just need to specify it:
a <- 4
b <- 7
cat('The average of ', a, ' and ', b, ' is ', average(a, b), '.', sep='')

# You can pass values by position, i.e. in the order they are specified, or by 
# name or a combination of the two and R will assume the unnamed parameters are 
# in the order given.
value <- 1
exponent <- 3
f <- function(const, value, exponent) {
  x <- const + value ^ exponent
}
print(f(0, 4, 3))
print(f(0, 4, exponent = 3))
print(f(const = 1, value = 7, exponent = 2))
value
x
print(f(2, 3, value = 7))
value
x
print(f(value <- 7, exponent <- 2, 4))
value
exponent
x

# Passing Parameters by Copying
# When you pass a parameter to a function in R, it is copied to the function and 
# the original variable is not changed. Note also, that this can cause the functions
# to become slow if you copy a large data object many times.
x <- 1
f <- function(value) {
  x <- value * 2
  print(x)
}
f(x)
x

# Variable Scope
# The difference in behaviour of -> and = in function parameters illustrates how 
# the scope of variables affects the outcome of programs. The scope of a variable 
# is the extent of the availability or existence of a variable.
# For example, the global variables, such as pi are available anywhere in a program.
# If you define a variable at the command line or in a script it will be a global 
# variable and available to every program you run.
# If you define a variable within a function, it is local to that function. You 
# can use it within the function but when the function completes, the variable is 
# destroyed.
# In the following code, x is global and y is local to the function f: 
# x <- 1                                               -| x exists from here on
# f <- function(value) {                                |
#     y <- 1               -| y exists from here to     |
#     return (value * y)   -| here (conceptually)       |
# }                                                     |
# f(x)                     - Actually, only when called |
# ... More code            - y no longer exists         | x does

# Changing the type of a variable
# If you need to change the type of one of the basic variable types, you can use 
# one of the following functions: 
#   as.character(x)
#   as.complex(x)
#   as.numeric(x)
#   as.integer(x)
#   as.logical(x)
# There are similar functions for more complex data types, but watch out for 
# as.Date() with capital D.

# Exercise: A function to generate LCG random numbers.
# Create a function to perform the lcg calculation: it will take four arguments 
# and return a single number, i.e. will be called as lcg(modulus, multiple, 
# constant, seed).

lcg <- function(modulus, multiple, constant, seed) {
  return((multiple * (seed + constant)) %% modulus)
}

m <- 23
a <- 6
s <- runif(1) * 100
c <- 7
s <- lcg(m, a, c, s)

