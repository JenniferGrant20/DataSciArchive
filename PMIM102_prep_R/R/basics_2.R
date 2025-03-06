# Create 4 vectors: one with an ID value, one with a weight value (in kg), one 
# with a height value (in cm) and one with the corresponding BMI value. 
# Three hints:
#   1. BMI = weight(kg) / (height(m) ^ 2).
#   2. Use rnorm() (normally distributed random numbers) to create a set of random
#      numbers with parameters mean and sd for mean and standard deviation (you 
#      could investigate average weight and height and guess a standard deviation 
#      of, say 1/5 of those values.
#   3. You can use hist(x) to see the spread of values of a vector, x.

ID <- seq(1, 50, 1)
ID
weight_kg <- rnorm(50, 78.5, 10)
weight_kg
height_cm <- rnorm(50, 169, 10)
height_cm
BMI <- weight_kg / ((height_cm / 100) ^ 2)
BMI
hist(BMI)

# Pick an ID from the vector (you can do this manually or randomly) and create a 
# list for that ID only which contains the ID, the weight, the height and the BMI.
# To make it more interesting, convert the weight and height to character strings 
# and append the units (e.g. kg or cm).

#A hint:
#  To add the units, you can use the function paste() which works like cat() 
#  except that it doesn't print the result.

ID_23 <- list(ID=ID[23], weight=paste(weight_kg[23], 'kg'), height=paste(
  height_cm[23], 'cm'), BMI=BMI[23])
ID_23

# Creating a dataframe
df <- data.frame(id = c(1, 2, 3, 4), age = c(30, 42, 21,59), gender = c('m', 'f', 'f', 'x'))
df
# Determining its size
dim(df)
# If we wanted to view just the age column, we could:
df$age
df[, 2]
df[, 'age']

# Take your vectors (or start from scratch in a similar way) from the exercise above
# and create a data frame. Then apply the summary and describe functions to it.
# Plot the data and see what the relationships look like. It would be worth looking 
# at the hist() output as well.
# What do you notice if, instead of 100 rows, you create a data frame with 10,000? 
# What can you deduce about your data?

BMI_df <- data.frame(ID = ID, weight_kg = weight_kg, height_cm = height_cm, BMI = BMI)
BMI_df
dim(BMI_df)
library(psych)
describe(BMI_df)
hist(BMI_df$BMI)

# Creating a larger dataframe
ID_2 <- seq(1, 10000, 1)
ID_2
weight_kg_2 <- as.integer(rnorm(10000, 78.5, 10))
weight_kg_2
height_cm_2 <- as.integer(rnorm(10000, 169, 10))
height_cm_2
BMI_2 <- weight_kg_2 / ((height_cm_2 / 100) ^ 2)
BMI_2

BMI_df_2 <- data.frame(ID = ID_2, weight_kg = weight_kg_2, height_cm = height_cm_2,
                       BMI = BMI_2)
BMI_df_2
dim(BMI_df_2)
library(psych)
describe(BMI_df_2)
plot(BMI_df_2)
hist(BMI_2)