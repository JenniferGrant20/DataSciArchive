print('Hello, me!')

any_number <- 1
any_number

class(any_number)

# variable names should be nouns describing what the variable contains
# function names should be verbs or verb-phrases
# for logicals, an is prefix can be used, e.g. is_dead

# There are six basic data types:
#   integer (specify an integer, rather than a numeric by using the 'L' suffix, e.g. v <- 123L)
#   double or numeric (v <- 123.456)
#   complex (v <- 123 + 456i)
#   logical (v <- TRUE)
#   character (v <- "Hello")
#   raw (all data are stored as byte values)

x <- 100L
class(x)
x <- 100.0
class(x)
x <- 100 + 200i
class(x)
x <- TRUE
class(x)
x <- "Hello"
class(x)
x <- raw(1)
class(x)

# There are a number of arithmetic operators in R:
#  + --- addition
#  - --- subtraction
#  * --- multiplication
#  / --- division
#  ^, ** --- exponentiation
#  %% --- modulus (x mod y)
#  %/% --- integer division

# And a few more assignment operators:
#  <- --- Left assignment
#  <<- --- Left assignment to the parent level
#  = --- Left assignment, parameter setting in a function call
#  -> --- Right assignment
#  ->> --- Right assignment to the parent level

# Comments
# Include a header on a script file describing what the file contains and how it 
#   relates to other files.
# It is a good idea to keep a note of date/time and author in this header so that 
#   we can ascertain whether it is a current version and who to ask about it.
# Document functions so that we know what they do, what parameters they take and 
#   what values they return.
# Explain why you are doing something, not how since the code explains that (although 
#   you should explain anything obscure or side-effects).
# Use comments to plan the steps of a function while you are writing it, but remove 
#   them as you implement the steps

# Expressions
# An expression is any combination of variable and operators 
# (and when we get to them, functions) that result in a value. 
# For example:
1 + 2 * 3 / 12 - 7
(1 + 2) * 3 / (12 - 7)
# operator precedence gives a different result for the above

# Statements
# A statement is a complete line of code which may include an assignment and an 
# expression or a control structure. For example an assignment statement:
x <- (1 + 2) * 3 / (12 - 7)
# consists of a variable name (x), the assignment operator (<-) and the expression 
# which generates the value to be assigned to (or stored in) the variable.