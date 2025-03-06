# Complicated Variables

# Vectors, Matrices, Arrays, Lists
# R's major strength is the ability to operate efficiently on entire data structures. 
# There are four commonly used data structures (although plenty of others for specific 
# purposes, such as time-series).

# Vectors
# The basic structure in R is the vector. Even single values are vectors of length 1. 
# A vector is a group of elements of one data-type. You create a vector using the 
# c() function to combine elements.
v <- c(1, 6, 3, -5, 2)
print(v)
v <- c(v, 3)
v

# We can check the type and structure of the variable with:
class(v)
str(v)
is.vector(v)

# Now we can use some of the internal functions to do some interesting things.
sum(v)
length(v)

# We will often use built functions to perform analyses on vectors (or columns
# of data frames which work like vectors). For example, the mean() function will 
# produce the average value of all the elements in a vector (or data frame column):
mean(v)
sum(v) / length(v)

# Operators, such as +, ^, operate on each element of a vector:
v ^ 2

# If you wish to add items to a vector, you can simply refer to the index you want 
# to set, even if it doesn't exist yet and R will sort it out. Or you create a 
# vector using the c() function with the existing vector as one of the arguments. 
# Use the append() function to add data in the middle of a vector.
v[10] <- 8
v <- c(v, 9)
v

# Extra: Recycling rule for vectors
# Try adding two vectors of unequal length. R will attempt to re-use the shorter 
# vector an integer number times to match the longer vector's length. It will
# produce an error if the difference is not an exact multiple of the shorter vector's 
# length.
vector_1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
vector_2 <- c(1, 2, 3)
vector_1 + vector_2

# Exercise: The full LCG with plots.
# Create a function that uses the LCG random number function to create a vector 
# full of n random numbers. Plot the data in the vector, a histogram of the 
# frequencies of the values and then a scatter plot where each point is given by 
# (x,y) = (valuem, valuem+1) for m is 1 to n-1.

# Generator.
lcg <- function(modulus, multiple, constant, seed) {
  return((multiple * (seed + constant)) %% modulus)
}

# Vector generator.
lcg_vector <- function(modulus, a, c, seed, n) {
  l <- vector()
  for (i in 1:n) {
    s <- lcg(modulus, a, c, s)
    l[i] <- s
  }
  return(l)
}

# Number of elements in the vectors.
n <- 10000

# Bad parameters.
m <- 23
a <- 6
s <- runif(1) * 100
c <- 7
bad <- lcg_vector(m, a, c, s, n)
# Good parameters.
m <- 86436
a <- 1093
s <- runif(1) * 100
c <- 18257
good <- lcg_vector(m, a, c, s, n)

# Plot the results.
v <- lcg_vector(m, a, c, s, 100)
plot(v)
par(mfrow=c(2,2))
hist(bad, breaks=100)
plot(bad[1:length(bad)-1], bad[2:length(bad)], pch='.', cex=1, col='red')
hist(good, breaks=100)
plot(good[1:length(good)-1], good[2:length(good)], pch='.', cex=1, col='green')

# Descriptive Statistics in R
# R comes with a large number of built-in functions to perform descriptive 
# statistics such as mean() to produce the mean of the values in a vector.

# Get some random data from a normal distribution --- we'll look at this later.
x <- rnorm(n=10)
y <- rnorm(n=12)
z <- rnorm(n=8)
m <- matrix(c(x, y, z), nrow=10, ncol=3)
colMeans(m)
summary(x)
summary(x)[[2]]
rev(x)

# You will frequently end up with NA values in your data. Logically these produce 
# an NA result for calculations but this is not always what you want. To perform a 
# function ignoring the NA values you can often specify the argument, na.rm=TRUE, 
# for example:
x <- c(1, 2, 3, NA, 4, 5, 6)
mean(x)
mean(x, na.rm=TRUE)

# Tables
# R has a very useful function table() which creates summary of counts of the 
# values in the vector arguments.

n <- 10000
x <- as.integer(runif(n) * 10)
table(x)
y <- as.integer(rbinom(n=n, size=2, prob=0.5))
table(x, y)

blue_eyes <- as.logical(rbinom(n=n, size=1, prob=0.3))
blond_hair <- as.logical(rbinom(n=n, size=1, prob=0.2))

table(blue_eyes, blond_hair)

shirt <- c('red', 'red', 'yellow', 'red', 'red', 'blue', 'yellow', 'red', 'red', 
           'yellow', 'yellow', 'yellow')
episode_landing_party <- c('survived', 'killed', 'survived', 'killed', 'killed', 
                           'survived', 'killed', 'killed', 'killed', 'survived', 
                           'survived', 'survived')
shirt <- ifelse(shirt!='red', 'not-red', 'red')
table(shirt, episode_landing_party)
chisq.test(shirt, episode_landing_party, correct=TRUE)
fisher.test(shirt, episode_landing_party)

# Matrices
# A matrix is a 2-dimensional vector with row and column definitions as well as
# an indication of the order that R should use the numbers:
m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4, byrow=TRUE)
print(m)
# There are a number of built-in matrix functions:
#   Transposition --- t(M)
#   Inversion --- solve(M)
#   Multiplication --- M %*% N
#   Create an n x n identity matrix --- diag(n)
#You can extract a vector from a matrix using the usual syntax:
#   v <- m[1, ] # Get the first row v <- m[, 1] # Get the first column
# If you want to keep the matrix structure e.g. have a 1-row matrix rather than 
# a vector, use drop=FALSE:
#   v <- m[1, , drop=FALSE]

# Arrays
# An array is a multi-dimensional vector with the dimensions defined by another 
# vector:
a <- array(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), dim=c(2,3,2))
print(a)

# Lists
# If you want to mix different kinds of data in one structure or to create groups 
# of elements that are not the fundamental data types, you need to use a list.
l <- list(1, c(1.2,2.3,3.4,4.5), "Hello")
l2 <- list(l, list('a', 'b', 0))
list_functions <- list(mean, median, sin, cos, sqrt)
print(l)
print(l2)
print(list_functions)

# You can include more complex data types in lists, such as vectors, matrices, 
# arrays and other lists.
l2 <- list(l, v, m, a, list('a', 'b', 0))
print(l2)
# And, while we're here, you can even refer to functions in a list:
list_functions <- list(mean, median, sin, cos, sqrt)
print(list_functions)

# Named list items
# You can name items in a list to create a kind of dictionary.
baddies <- list(zombie=1, vampire=20, demon=10)
baddies
baddies['vampire']
names(baddies)

# Deleting list items
#You can delete a list item by setting it to NULL.
baddies['zombie'] <- NULL
baddies

# Converting a list to a vector
# Flatten a list to a vector with unlist:
unlist(baddies)

# Accessing the Individual Elements
# A vector is a series of values numbered 1 to its length. To access a single
# specific item, we can use the square bracket notation, v[index] where index is
# the number of the item we want to access. You can think of the square brackets 
# as accessing the data contained at the the value.
v <- c(2, 4, 6, 8, 10)
v[3]
m <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8), nrow=2, ncol=4, byrow=TRUE)
m[2,3]
a <- array(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), dim=c(2,3,2))
a[1,1,1]

# Lists (and data frames, as we will see) are a bit more tricky as they may contain 
# lists. So l[1] refers to the first item in the list , 1, but this is stored as a 
# list (so that all items can be treated in the same way), so you get a value 1 in 
# a container, [1]. This isn't a number but a list of numbers. But you can get the 
# number contained in this container by using the square brackets notation again on
# this list, [[1]]:
l <- list(1, c(1.2,2.3,3.4,4.5), "Hello")
class(l)
# l[1] + 3
class(l[1])
l[[1]] + 3
class(l[[1]])
print(l)
class(l[[3]])

# We can also select multiple items by giving a vector of indices:
v[c(1,3,5)]

# This leads to filtering using the function which which produces a list of indices 
# of elements which meet the specified condition.
which(v>4)
v[which(v>4)]
v[v>4]

# Applying functions to vectors, matrices and lists
# We can apply a function to all items in a data structure using apply. We need 
# to supply the matrix, the MARGIN - whether we want to use rows (=1), columns (=2) 
# or both (=c(1,2)) and the function we wish to apply. If we want we can specify 
# our own function or include the definition in the apply call.
#    apply(m, 2, function(x) length(x[x>4]))
print(m); print(dim(m)); print(is.matrix(m))
apply(m, MARGIN=1, FUN=mean)
apply(m, 2, function(x) length(x[x>2]))

# There are similar functions to produce the simplest output possible (sapply) or 
# lists (lapply). These will work on vectors, lists and data frames. We will meet 
# some other 'apply' family functions later.
sapply(v, function(x) x^2)
lv <- lapply(l, function(x) length(x))
lv
class(sapply(l, function(x) length(x)))
class(lapply(l, function(x) length(x)))

# Converting between complex strutures
# Use the as.vector(), as.list(), as.matrix(), as.data.frame() functions to convert
# from one form to another. Except where lists are concerned, where you may need to
# use a little care. To go from a list to a vector use unlist() rather than
# as.vector() to avoid problems with lists with mixed data. When a data.frame or 
# list contains different types of data and the conversion is to a single-type 
# data-type, everything gets converted to characters.

# Adding data to vectors
# One of the common features of loops is to produce a vector in a way that is 
# difficult to do with R's vector processing. For example, adding vectors is easy:
v1 <- c(1,2,3,4)
v2 <- c(5,6,7,8)
v <- v1 + v2
print(v)

# You can add a new item to the end of a vector by using rbind. But now we come up
# against some R magic which we might want to undo. R here creates the output for 
# us, but also creates a name for each row and forms that into a matrix. We can 
# fix this using unname() and as.vector(). This is typical of the kind of wrangling 
# we need to do with R. As well as rbind(), you can add data to new columns using 
# cbind() in a very similar way.

last_integer <- 1
previous_integer <- 1
f <- vector()
repeat{
  next_integer <- last_integer + previous_integer
  if (next_integer > 100) break
  f <- rbind(f, next_integer)
  previous_integer <- last_integer
  last_integer <- next_integer
}
f
f <- unname(f)
class(f)
f
f <- as.vector(f)
class(f)
f

# Some Specialised R Components
# As you saw above, there are six different types of simple data defined in R. 
# In terms of statistics, we have integers and numerics to provide continuous 
# variables or counts. We have logical variables that can define true or false. 
# Another commonly used statistical variable type is a categorical variable 
# something which can take on a number of values, possibly ordered, but not 
# necessarily often indicative of groups.
# For example, blood pressure categories
# We could have strings for each of these:
# "optimal", "normal", "high-normal", "grade-1-hypertension", "grade-2-hypertension",
# "grade-3-hypertension" and "isolated-systolic-hypertension"
# but we risk mis-spellings, capitalisation errors which we might not detect.
# R provides us with a neat way to reduce that risk and also to reduce the amount 
# of memory our data uses: factors.

# Factors
# Factors are a special form of vector used as a categorical variable or for grouping. 
# R maintains a vector as a set of values (levels) each of which has a name. So, 
# for our blood pressure factor, R will create levels:
# "optimal", "normal", "high-normal", "grade-1-hypertension", "grade-2-hypertension", 
# "grade-3-hypertension" and "isolated-systolic-hypertension"
# and it will create a look-up table and give each level an integer value starting 
# at 1:
# "optimal" = 1, "normal" = 2, "high-normal" = 3, "grade-1-hypertension" = 4,
# "grade-2-hypertension" = 5, "grade-3-hypertension" = 6 and 
# "isolated-systolic-hypertension" = 7
# A vector of this factor (i.e. a vector of blood-pressure results) would contain 
# these values, e.g. c(1, 5, 2, 3, 2, 2, ... ), which R will display by converting 
# the values to the names in the look-up table e.g. c("optimal", "grade-2-hypertension",
# "normal", "high-normal", "normal", "normal" ... ).
# You can create a factor from data. You simply need to pass the data to the factor()
# function. It will look through the data and for the look-up table accordingly. 
# However, it will order the factors alphabetically which is not always what you want,
# for example, with age range you might get '1-5' '10-15' '6-9' etc. when you'd prefer 
# '1-5', '6-9', '10-15' etc. To get the correct levels you need to specify the levels
# explicitly.
# It will also, not include any values missing from your data vector.

blood_pressure <- c("optimal", "grade-2-hypertension", "normal", "high-normal", 
                    "normal", "normal", "grade-1-hypertension")
blood_pressure
bp <- factor(blood_pressure)
print(bp)
bp <- factor(blood_pressure, levels = c("optimal", "normal", "high-normal", 
                                        "grade-1-hypertension", "grade-2-hypertension",
                                        "grade-3-hypertension", 
                                        "isolated-systolic-hypertension"))
print(bp)

# To change the text used for the levels we just need to change the levels vector 
# for the factor using levels(). This won't change the data, just the name used for 
# each entry.
levels(bp) <- c('low', 'medium', 'high', 'very high', 'huge', 'massive', 'gargantuan')
levels(bp)
print(bp)

# Exercise: Change 'very high' to a single word value, something between 'high' and 
# 'huge', in one line of code.
levels(bp) <- c('low', 'medium', 'high', 'elevated', 'huge', 'massive', 'gargantuan')

# Looking ahead to data frames: some useful functions for groups
# There are a couple of useful functions for grouping variables:
# stack() will take a named list and build a two-column data frame, the column 
#   containing the values and the second a grouping factor which were the names of the 
#   list items.
# unstack() will extract the pair of columns into n columns where n is the number of 
#   groups. Each column will be named from the name of the group.
# split() will take a pair of variables, the values and the groups and split these 
#   into a named list.

tapply(column, group, function) will apply the function to the data in column in groups according to group, for example, tapply(deaths, agegp, mean) will calculate the mean of the deaths column for each agegp.