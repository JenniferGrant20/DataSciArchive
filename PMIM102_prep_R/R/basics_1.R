# Creating a simple graph
sequence <- seq(from=-10, to=10, by=0.5)
y <- sequence ^ 3
plot(x=sequence, y)

# Plotting the sines and cosines of a sequence on the same graph
x <- seq(-10, 10, 0.5)
y_sin <- sin(x)
y_cos <- cos(x)
plot(x, y_sin, type='l', col='red')
par(new=TRUE)
plot(x, y_cos, type='l', col='blue')

source('./R/functions.R')
library(crayon)
greentext("This function makes text green.")
