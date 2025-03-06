# Data Frames
# It is likely that you will manipulate most of the data you work with in data-frames.
# A data-frame is a rectangular structure in which each column can have a different 
# data-type but all columns have the same length. Each column can have a name. You can
# create a data-frame manually, but most often you will import a data-frame from a file
# or by accessing a database using, for example, SQL calls.
df <- data.frame(
  name=c('Olive', 'Pinochio', 'Queenie', 'Raphael'),
  gender=c('female', 'male', 'female', 'male'), 
  height=c(1.7, 1.01, 1.3, 1.6),
  weight=c(60, 30, 50, 40),
  stringsAsFactors=FALSE)
print(df)
class(df)
df[3]
class(df[,2])
df[[3]]
class(df[[3]])
df[[3]][3]

# To refer to a single column in a data-frame, you can use one of three methods:
# 1. use '$' to refer to the column name.
# 2. use [row, col] to select a row, a column or an entry. If you leave an entry
#    blank, R will show the whole row or column (rows and columns start at 1).
# 3. use the column name in the second position rather than the column number.
df$height
df[, 3]
df[, 'height']

# And you can use functions on data frame columns:
df[, c(1,3:4)]
df
mean(df[, 3])
sapply(df[, 3:4], mean)

# Appending data to a data frame
# It regularly happens that you may wish to add data to a data frame. This is quite 
# straightforward, you just create a new row and rbind it to end of the existing 
# data frame.
newOne <- data.frame(name="Seren", gender="female", height=2.0, weight=60)
newOne
df <- rbind(df, newOne)
df

# But this is really inefficient with large data frames as the whole data frame is 
# copied every time a row is added. If you know you are going to add a lot of data,
# it would be better to allocate an empty data frame first and then just place the 
# data in the allocated structure.
# We can do this by specifying the type of data that each column will contain and 
# the number of entries.

N <- 1000
huge_df <- data.frame(name=character(N), gender=factor(N, levels=c("male", "female")), 
                      height=numeric(N), weight=numeric(N), stringsAsFactors=FALSE)
head(huge_df)
df[1,]
huge_df[1, ] <- df[1, ]
head(huge_df)

# Removing NAs
# One more useful thing is the ability to remove rows with NAs.
clean_huge_df <- na.omit(huge_df)
head(clean_huge_df)

# Working the R way
# Despite the availability of loops and control structures, R has been designed to 
# be manipulated as vectors (columns or variables in data frames) with the looping 
# processes implemented optimally under the bonnet. The normal way to work, therefore,
# is with data frames and passing an entire data frame or maybe just one column to a
# function as a parameter.

# Exercise: Create a data frame and try out some descriptive statistics
# Use the functions we have seen so far to create a number of vectors, combine them
# to form a data frame, use various analysis functions on the data frame variables,
# for example, summary(), aggregate(). How would you use apply() to generate a mean
# across the rows? How about a sum across the rows? Apply some data frame functions
# to the data frame: mean(df), cov(df) and cor(df). Create some contingency tables
# from the variables. If you have continuous variable, how might you create some 
# ordinal/categorical variables?
# try:
df <- data.frame(
  ID=c(1:4),
  gender=c('female', 'male', 'female', 'male'), 
  height_cm=c(167, 178, 152, 185),
  weight_kg=c(65, 77, 53, 82),
  stringsAsFactors=FALSE)
print(df)
summary(df)
class(df[,4])

#provided example answer:
n <- 1000
# Some random variables, mean 0.
c1 <- runif(n) - 0.5
c2 <- 2*(runif(n) - 0.5)
c3 <- runif(n) - 0.5
c4 <- 3*(runif(n) - 0.5)
c5 <- runif(n) - 0.5
c6 <- 4*(runif(n) - 0.5)
# Add them to a data frame.
df <- data.frame(col1=c1, col2=c2, col3=c3, col4=c4, col5=c5, col6=c6)

summary(df, mean)
# Run a t-test, and the same thing with the 'with' syntax.
t.test(df$col1)
with(df, t.test(col1, col2))
# Apply a mean across rows:
# df <- cbind(df, mean=apply(df[, 1:3], 1, mean))
# Apply a sum across rows:
df <- cbind(df, sum_1_6=apply(df[, 1:6], 1, sum))
# A shapiro test for normality.
library(MASS)
shapiro.test(df$sum_1_6)
# Plotting and fitting a distribution.
hist(df$sum_1_6, ylim=c(0, n/3))
x <- seq(-3, 3, 0.01)
f <- fitdistr(df$sum_1_6, densfun='normal')
lines(x, (n/2)*dnorm(x, mean=f[[1]][1], sd=f[[1]][2]), col='red')
curve((n/3)*dnorm(x, mean(df$sum_1_6), sd(df$sum_1_6)), add=TRUE, col='darkblue', lwd=2)

# Create a categorical variable from column 1.
df$cat1 <- df$col6>0.5
df$cat2 <- cut(df$col1, breaks=4)
head(df)
# aggregate(df, by=list(df$cat2), FUN=sd)
# with(df, table(cat1, cat2))


# Exercise: Repeat with some real data.
# Read the CSV file, sulphur.dioxide.csv*, and explore and manipulate that data.
sulphur.dioxide <- read.csv('data/sulphur.dioxide.csv')
head(sulphur.dioxide)


# Formulas
# Formulas are primarily used in modelling to specify to the model which predictor
# variables should effect the response variable and the kind of relationship that 
# is of interest.
# The general format is:
#   y ~ x
# to indicate that the response, y, is expected to depend on x. We will see this 
# later when we come to do linear regressions with the lm() function, e.g. 
# lm(y ~ x, data=d) will perform a linear regression on the data frame d which
# contains the variables, x and y.

# There are some additional elements that can be used in a formula:
# y ~ a + b --- multiple regression; the predictor variables, a and b will be fitted.
# y ~ - a --- exclude a (useful with '.', see below).
# y ~ x - 1 --- No intercept, regress through the origin.
# y ~ . --- With a specified data=data-frame, use all the variable in the data-frame.
# y ~ a:b --- the interaction between a and b will be fitted.
# y ~ a + b + a:b = y ~ a * b --- multiple regression with interaction between the 
#                                 variables.
# y|a --- conditioning, include y given a.
# y ~ (a + b + c) ^ 3 --- interaction terms up to three-way.
# y ~ I(a+b) --- the values 'a+b' will be fitted to the response variable rather 
#                than the separate variables.
# y ~ log(a) --- log(a) will be fitted to the response variable.
# Several other functions use the formula notation to simplify their operation. 
# For example, you can plot grouped boxplots according to a grouping variable with:
#    boxplot(y ~ x)

par(mfrow=c(1,1))
variable1 <- rnorm(10, mean=5, sd=5)
variable2 <- rnorm(10, mean=7.5, sd=5)
variable3 <- rnorm(10, mean=10, sd=1)
data <- stack(list('1'=variable1, '2'=variable2, '3'=variable3))
with(data, boxplot(values ~ ind))

variable <- data.frame(value=rnorm(1000))
variable$cut <- cut(variable$value, breaks=12)
variable$section <- rep(1:10)
variable$alternate <- rep(1:2)
with(variable, boxplot(value ~ cut))
variable$value[variable$section==4] <- variable$value[variable$section==4] + 2
with(variable, boxplot(value ~ section))
with(variable, boxplot(value ~ alternate + section))
