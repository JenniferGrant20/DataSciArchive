# R and Stats Notebook 1
# 24/01/2024
# Jennifer Grant
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Combination
# You can count the number of combinations of k items chosen from n, easily in R 
# with choose(n, k). If you want to actually generate these combinations, use 
# combn(items, k) with a list of the items and the number you want to select 
# (which gives you a matrix with the combinations in successive columns):

choose(4, 2)
combn(c(1, 2, 3, 4), 2)

# You might come across combinations when looking at the binomial distribution 
# and yes/no or other discrete variables like coins and dice. In case you're 
# wondering, permutations require a new library and slightly different output, 
# but for the sake of completeness:

install.packages("combinat")
library(combinat)
permn(c(1, 2, 3, 4))

# Quantiles
# Quantiles are a useful way to describe the distribution of data. They describe
# the proportion of data below a value (or the value below which there is a
# specified proportion of the data). So the median is the value at which half 
# the data is above / below. The 99th centile has 99 percent of the values below
# it. The quartiles are frequently quoted: the values with 25% below (lower) and 
# 75% below (upper). The inter-quartile range is often quoted to give a sense of 
# spread. The classic way to present quantiles is with a boxplot which show min 
# and max (excluding outliers), upper and lower quartiles and the mean.

par(mfrow=c(1, 2))
v <- rnorm(1000)
boxplot(v, col='orange')
w <- c(rnorm(100, mean=1, sd=1), rnorm(100, mean=2, sd=2), rpois(100, lambda=6))
boxplot(w, col='light blue', notch=TRUE)

# plotting both v and w on the same plot in different colours
boxplot(v, w, col=c('orange', 'light green'), notch=TRUE)

# Probability Distributions
# The set of all the possible values of a random variable and the associated 
# probabilities is the probability distribution of that variable. There are a 
# few very common distributions you are sure to come across as they correspond 
# to the situation for particular types of variables.

# 1. Uniform distribution - equal chance for a series of values, die, coin etc.
# 2. Binomial distribution - n independent trials with each trial having a 
#    probability, p. n=1 is a Bernoulli distribution - e.g. a single coin toss.
# 3. Poisson distribution - the number of events that have occurred in a period.
# 4. Normal (Gaussian) distribution - the fundamental distribution in statistics
#    - the result of the central-limit theorem.

# Uniform Distribution
# We can get the value for the quantile which gives us 95% with:
qunif(0.95, lower.tail=TRUE)

# Display options.
par(mfrow=c(3,2))
options(repr.plot.width = 8, repr.plot.height = 8)
# x-axis values (set like this for clear plotting).
x <- seq(1,6,0.1)
# Look at the probability density function ...
d <- dunif(x, min=0, max=6)
plot(d, pch=21, bg='orchid1', main='Probability Density Function')
# ... and the cumulative density function.
p <- punif(x, min=0, max=6)
plot(p, pch=21, bg='aquamarine', main='Cumulative Density Function')
# Now create a data frame with multiple variables and display what happens when 
# you add them together.
y <- data.frame(runif(1000))
for (i in 1:10) {
  y <- cbind(y, runif(1000))
}
# Note that the mean becomes the sum of the means and the variance the sum of 
# the variances.
hist(y[,1], col='khaki', main='Each measurement is from one variable')
hist(y[,1] + y[,2], col='palegreen', main='Each measurement is the sum of two 
     variables')
mean(y[,1])
mean(y[,1] + y[,2])
mean(y[,1] + y[,2] + y[,3])
mean(y[,1]*2)
var(y[,1])
var(y[,1] + y[,2])
var(y[,1] + y[,2] + y[,3])
var(y[,1]*2)
# Now for 4 and 10 variables - here we embed a chunk of code in the call to 
# hist():
hist(x={x <- 0; for(i in 1:4){ x <- x + y[,i]}; x}, breaks=20, 
     main='Four variables', xlab='Four variables', col='cadetblue1')
hist(x={x <- 0; for(i in 1:10){ x <- x + y[,i]}; x}, breaks=20, 
     main='Ten variables', xlab='Ten variables', col='peachpuff')
# Looking ahead we can compare this to the Normal distribution ...
y <- seq(0, 10, 0.1)
lines(y, 270*dnorm(y, mean=5, sd=1), col="red")

# How to adjust the bin sizes on the histograms
# The problem with histograms is how to organise the bins:
#   How wide should they be?
#   Where do you put samples that happen to lie exactly on the boundary between 
#   bins?
# Sturge's rule suggests nbins 1 + 3.322log(nobservations); this is the default 
# in R.
# You can specify a number using breaks= and R will try to get close or you can 
# specify the exact breaks you want with a vector or a sequence.

par(mfrow=c(3,2))
options(repr.plot.width = 8, repr.plot.height = 8)
x <- rnorm(1000) * 100
sturges <- 1 + 3.322 * log10(length(x))
sturges # <- sturges * 4
histInfo <- hist(x, col='bisque')
hist(x, breaks=sturges, col='darksalmon')
histInfo
# Now try some density histograms (i.e. frequency/total).
hist(x, freq=FALSE, main='Density Histogram', sub='default bins', 
     col='lightsteelblue')
hist(x, freq=FALSE, breaks=seq(-400, 400, 20), main='Density Histogram with 
     fixed bins', sub='-400 to 400 in 20s', col='lightsteelblue4')
hist(x, freq=FALSE, breaks=c(-800, -400,-200, -100, -50, 0, 10, 20, 40, 80, 160, 
                             320, 640), 
                             main='Density Histogram with a list of bin-sizes', 
                             sub='custom vector', col='purple')
# Now try a series of ever-increasing number of bins.
# range <- max(x) - min(x)
par(mfrow=c(3,3))
b <- c(2, 3, 5, 10, 25, 50, 100, 400, 4000)
for(i in 1:length(b)) {
  hist(x, breaks=b[i], main=b[i], col='lightgreen')
}

# The Binomial Distribution
# The binomial distribution is the distribution followed when we have each 
# measurement containing n independent trials with a probability of any trial 
# being a success of p.
# We can get the value of the binomial distribution (n given by size and p given 
# by prob at a particular value of x) with dbinom:

qbinom(0.95, size=32, prob=0.5)
dbinom(x=2, size=2, prob=0.5, log=FALSE)

# And for an entire sequence:

x <- seq(1, 10, 1)
rbinom(x, size=10, prob=0.5)

# The cumulative distribution can be accessed with pbinom:

pbinom(q=1, size=10, prob=0.5, lower.tail=TRUE)
pbinom(q=1, size=10, prob=0.5, lower.tail=FALSE)

# The value of the distribution at a particular quantile can be found with qbinom:
qbinom(p=0.95, size=10, prob=0.5)

# And we can plot the distribution and create a histogram

par(mfrow=c(4,2))
x <- seq(0, 300, 1)
for (i in 1:8) {
  x <- seq(0, 4 + 2^i, 1)
  plot(dbinom(x, size=2^i, prob=0.5), main=paste("Binomial Distribution with", 
                                                 2 ^ i, "trials per measurement", 
                                                 sep=" "))
}
par(mfrow=c(2,2))
x <- seq(0, 10, 1)
plot(dbinom(x, size=1, prob=0.5), main="Binomial Distribution")
plot(pbinom(x, size=10, prob=0.5, lower.tail=TRUE))
plot(rbinom(1000, size=10, prob=0.5))
hist(rbinom(1000, size=1, prob=0.5), breaks=50)
# Looking ahead ...
y <- seq(0, 10, 0.1)
lines(y, 1000*dnorm(y, mean=5, sd=2), col="red")

# Note the format of the data passed to the hist() function call: rbinom(1000, 
# size=10, prob=0.5)

head(rbinom(1000, size=10, prob=0.5), 40)

# Bernoulli Distribution
# Special case: a binomial with n=1 is a Bernoulli distribution and this occurs 
# quite frequently e.g. a flipping a coin

# Exercise: What happens when the number of variables increases
# Examine what happens for a coin when you have 1, then 2 then 4 then 8 throws.
# What about a 6-sided dice?

par(mfrow=c(2,3))
barplot(dbinom(x=seq(0,32,1), size=1, prob=0.5), main="Binomial Distribution", 
        col="yellow")
barplot(dbinom(x=seq(0,32,1), size=2, prob=0.5), main="Binomial Distribution", 
        col="yellow")
barplot(dbinom(x=seq(0,32,1), size=4, prob=0.5), main="Binomial Distribution", 
        col="yellow")
barplot(dbinom(x=seq(0,32,1), size=8, prob=0.5), main="Binomial Distribution", 
        col="yellow")
barplot(dbinom(x=seq(0,32,1), size=16, prob=0.5), main="Binomial Distribution", 
        col="yellow")
barplot(dbinom(x=seq(0,32,1), size=32, prob=0.5), main="Binomial Distribution", 
        col="yellow")
par(mfrow=c(1,1))
y <- seq(0, 32, 0.1)
hist(rbinom(n=10000, size=32, prob=0.5), breaks=20)
lines(y, 10000*dnorm(y, mean=16, sd=4), col="red")
#par(mfrow=c(2,3))
#colours <- c("red", "green", "orange", "purple", "blue", "black", "white", "pink")
#barplot(dbinom(x=seq(0,32,1), size=1, prob=0.17), main="Binomial Distribution", 
#        col=colours)
#barplot(dbinom(x=seq(0,32,1), size=2, prob=0.17), main="Binomial Distribution", 
#        col=colours)
#barplot(dbinom(x=seq(0,32,1), size=4, prob=0.17), main="Binomial Distribution", 
#        col=colours)
#barplot(dbinom(x=seq(0,32,1), size=8, prob=0.17), main="Binomial Distribution", 
#       col=colours)
#barplot(dbinom(x=seq(0,32,1), size=16, prob=0.17), main="Binomial Distribution",
#        col=colours)
#barplot(dbinom(x=seq(0,32,1), size=32, prob=0.17), main="Binomial Distribution", 
#       col=colours)

# The Poisson Distribution
# This is the distribution for the number of events to have occurred in a time 
# period. For example, It is specified by a single parameter, its mean, lambda.
# We can get values for specific points on a distribution

dpois(x=1, lambda=1, log=FALSE)
x <- seq(0, 10, 1)
dpois(x, lambda=1)

# As well as the distribution, cumulative distribution and random data.

par(mfrow=c(2,3))
x <- seq(0, 10, 1)
# Cumulative probability function
plot(ppois(x, lambda=5, lower.tail=TRUE), col='red')
# Probability distibution function for various lambda.
plot(dpois(x, lambda=1), pch=21, bg=1)
points(dpois(x, lambda=2), pch=21, bg=2)
points(dpois(x, lambda=5), pch=21, bg=3)
points(dpois(x, lambda=10), pch=21, bg=4)
# Sample of data.
plot(rpois(1000, lambda=5), col='green')
# Histogram of a sample of counts (note added labels).
# Values of 0, 0.5, and 1 specify that (x, y) should align with the
# left/bottom, middle and right/top of the text, respectively.
h <- hist(rpois(1000, lambda=5))
text(h$mids, h$counts, labels=h$counts, adj=c(0.5, -0.5))
# Sample of data with large lambda (and colours).
plot(rpois(1000, lambda=100), col=c(1, 2 , 3, 4, 5, 6, 7, 8, 9, 10))
# And again, looking ahead to the comparison of a large lambda and a
# Normal distribution.
y <- seq(70, 130, 1)
hist(rpois(1000, lambda=100), breaks=20)
lines(y, 5000*dnorm(y, mean=100, sd=10), col='red')
xbar <- barplot(dpois(x=seq(50,150,2), lambda=100))
#lines(x=seq(0,100,2), y=dnorm(x=seq(50,150,2), mean=81, sd=4), type='l', col="red")
#p <- dpois(x=seq(50,150,1), lambda=100)
#d <- dnorm(x=seq(50,150,1), mean=100, sd=10)
#p
#d

# As you can see, this too tends towards the normal distribution.

# The Central Limit Theorem
# The distribution these three distributions have been tending towards is the 
# Normal distribution and this tendency is known as the Central Limit Theorem.
# The i.i.d. assumption is important in the classical form of the central limit 
# theorem, which states that the probability distribution of the sum (or average) 
# of i.i.d. variables with finite variance approaches a normal distribution.

# Some Definitions
# Mode
# The most common value. There may be more than one in which case the mode is the 
# local maximum values.

v <- c(rnorm(1000, mean=0, sd=1), rnorm(800,mean=4, sd=1))
hist(v)

# Skew
# If the distribution is not symetrical and has a lengthened tail in one direction 
# it is known as skew to that direction.

v <- c(rnorm(1000, mean=0, sd=1), rpois(1000, lambda=3))
hist(v)

