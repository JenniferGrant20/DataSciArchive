library(crayon)
blue_now <- function(){
  cat(blue(date()))
}
blue_now()

# adding return() to the function will provide the current date
library(crayon)
blue_now <- function(){
  return(blue(date()))
}
blue_now()

cat("The time now is", blue_now(), ".\n")

# suppose that we'd like to change our blue_now function to include some additional text 
# to also be coloured blue. We would do that by adding an input parameter named text.
library(crayon)
blue_now <- function(text){
  return(blue(paste(text, date())))
}
output <- blue_now('Today: ')
output
cat(output)

# We could add another parameter to indicate if we want an alarm marker.
library(crayon)
blue_now <- function(text, alarm){
  alarm_text <- ifelse(alarm, 'ALARM', '')
  return(blue(paste(text, date(), alarm_text)))
}
output <- blue_now('Today: ', TRUE)
cat(output)

# another useful feature is that we can define default values for the parameters so 
# that we don't actually have to specify them unless we want to change form that default.
library(crayon)
blue_now <- function(text='Now: ', alarm=FALSE){
  alarm_text <- ifelse(alarm, 'ALARM', '')
  return(blue(paste(text, date(), alarm_text, '\n')))
}
cat(blue_now('Today: '))
cat(blue_now())
cat(blue_now(alarm=TRUE))

# Exercise: create a function to calculate the BMI.
# The function will need to take the weight and height and return the BMI. 
# Hints:
#  1. BMI = weight(kg) / (height(m) ^ 2).
#  2. Test the function - find a website with some values and see if you match them.
#     (https://www.nhs.uk/live-well/healthy-weight/bmi-calculator/)

bmi_calc <- function(weight_kg, height_cm){
  bmi_calc <- weight_kg / ((height_cm / 100) ^ 2)
  return(bmi_calc)
}
bmi_calc(weight_kg=70, height_cm=150)   # Should be 31.1 
bmi_calc(80, 180)                       # Should be 24.6.
