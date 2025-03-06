# Programs need to make decisions about whether or not they should do things
# To do this they need to be able to:
#   check a condition, and
#   control the program flow

# To check conditions we need to form a logical expression (also called a conditional 
# expression) which results in a value of TRUE or FALSE. 
# For example, if we wanted to check that the height value we have been given for a 
# BMI calculation is realistic, we might want to check that it was above 0.5m:
height_cm <- 2000
(height_cm > 50)
# and less than 3m
(height_cm > 50) & (height_cm < 3000)
# Numerical values can be used in logical and relational statements. In relational 
# statements, 0 is considered FALSE and all other values TRUE

# Relational Operators
# The following allow us to compare logical values in complex tests:
#   < --- less than
#   <= --- less than or equal to
#   > --- greater than
#   >= --- greater than or equal to
#   == --- equal to
#   != --- not equal to
#   isTRUE(x) --- test if X is TRUE

# Logical Operators
# The following allow us to modify logical values:
#  !x --- not x
#  x | y --- x or y (all elements in a complex structure)
#  x & y --- x and y (all elements in a complex structure)
#  x || y --- x or y (top element only in a complex structure; returns a single value)
#  x && y --- x and y (top element only in a complex structure; returns a single value)

# Making Decisions
# In R we can choose whether to run a piece of code using the if (conditional 
# expression) { _state else control structure which looks like:
#    if (conditional expression) { 
#      statements if TRUE 
#    } else { 
#      statements if FALSE
#    }
# R will evaluate the conditional expression, if it is TRUE (or non-zero) R will 
# execute all the instructions in the statements if TRUE section and, if FALSE or 0, 
# will instead execute all the instructions in the statements if FALSE section.

x <- 5
y <- 10
if (x < y) {
  print("x is less' than y")
} else {
  print('x isn\'t')
}
if (x == y) {
  print('They are both the same')
}
if (x | y) print('One or the other or both')

# You can combine multiple if-else statements to get a series of tests:
#   if (conditional expression) { 
#    statements if TRUE 
#   } else if (another_condition)  { 
#    statements if TRUE 
#   } else { 
#    statements if FALSE
#   }
# Note that NA values cause trouble with conditional expressions:
x <- NA
if (x == NA) print('No data')
# So we need to explicitly check for NA using a special function is.na(<value>):
if (is.na(x)) print('No data')
x <- Inf
if (is.infinite(x)) print('Infinity!')

# R has another piece of shorthand, the ifelse statement. 
# This is simply the if ... else we have seen above but in one line:
#   ifelse(condition, statement if TRUE, statement if FALSE)
# This is particularly useful for complex data processing (see mutate later on), 
# where you can assign a new variable:
#   x <- ifelse(y > z, y, z)
y <- 20
z <- 18
x <- ifelse(y > z, y, z)
x

# Repetition - Control Loops
# As well as the if ... else control structure, R has a number ways to repeat a 
# section of code for example, to iterate through data (although R often has a 
# better way to do this as we'll see later). 
# There are three kinds of loops available for three different situations:
#   for loops --- which repeat a piece of code a specified number of times.
#   while loops --- which repeat code while a condition is met each time we start a loop.
#   repeat loops --- which repeat until we break out of it (we can check a condition 
#   in the middle or at the end of a loop, or at multiple points in a loop).
# When writing while and repeat loops, be careful as it is easy to accidentally create
# an infinite loop if you fail to create the conditions for leaving. You should be 
# able to terminate your R session to break out of infinite loops, but sometimes, 
# the code executes so rapidly that R-Studio never responds and needs to be restarted
# - this is the moment you risk losing any unsaved work.

# For loops
# A for loop defines a loop variable and the values it should take , for example:
#   for (var in 1:10)
# In this case the following code section will be executed once for each value of 
# var in the range 1 to 10. The 1:10 is short-hand for a sequence which can be 
# defined using:
#   seq(from, to, step, length.out, along.with)
# in this case 1:10 is the same as seq(1, 10, 1).

for(loop in seq(1, 10, 2)) {
  die <- as.integer(runif(1) * 6) + 1
  cat(loop, '=>', die, '\n')
}
# You can force the loop to start the next iteration using next and to leave the 
# loop early using break.
for (loop in seq(1, 10, 1)){
  die <- as.integer(runif(1) * 6) + 1
  print(die)
  if (die <= 3) next
  print('Die greater than 2')
  if (die > 5) break
  print('But not a six')
}

# While loops
# While loops allow you to test a condition before you execute the loop and stop 
# executing once the condition fails (i.e. == FALSE).
#   while(condition-expression) {     # execute some code }
# In the example below, the loop will check at the start that score is less than 
# 100 and execute if so. It will continue with the first statement after the loop 
# as soon as this check shows score has reached or exceeded 100.
score <- 0
number_of_throws <- 0
while(score < 100){
  die <- as.integer(runif(1) * 6 + 1)
  score <- score + die
  number_of_throws <- number_of_throws + 1
}
cat('Reached', score, 'in', number_of_throws, 'throws.')

# Repeat loops
# A repeat loop allows you to create a loop in which you don't need a condition 
# (so it would go on forever) or where you wish to check at some point other than 
# the start. It looks like:
#   repeat {
       # execute some code
       # if you want to exit the loop, you need to 'break'
#   }  
last_integer <- 1
previous_integer <- 0
repeat{
  next_integer <- last_integer + previous_integer
  if (next_integer > 100) break
  print(next_integer)
  previous_integer <- last_integer
  last_integer <- next_integer
}
