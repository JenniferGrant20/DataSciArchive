# Using R as a calculator 
3+4
1*2
9/3
3-2
3%%3
3**3
3^3
3 + 4 * 7
(3+4) * 7
answer <- 3 + 4
print(answer)
answer # Should use print() when writing in a script

sqrt(100)
log(100)
log10(100)
substr("contents", 4, 7)
toupper("contents")
runif(1)
runif(3, 1, 10)
rnorm(4, 5, 1)
seq(1, 10, 3)
seq(1:10)
?runif

# Calculate BMI
weight_kg <- 50
height_cm <- 162

bmi <- weight_kg / ((height_cm / 100)^2)
cat("The BMI is", bmi, ".n/")

# Functions
# Defining a function
#  function_name <- function(x, y) {
#    a <- first_thing(x)
#    b <- second_thing(y)
#    return(a + b)
#  }
# Using a function
#  x <- function_name(1, 2)


}
