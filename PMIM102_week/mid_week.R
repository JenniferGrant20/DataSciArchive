# ------------------------------------------------------------------------------
# Laboratory Sessions
# COVID-19
# ------------------------------------------------------------------------------
# January 2024
# Jennifer Grant
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(readxl)

COVID_testing = read.csv('./data/COVID_testing_data.csv')
ireland_tests <- COVID_testing %>% filter(country_code=='IE')
plot(as.integer(ireland_tests$new_cases), type='l')
ggplot(data=ireland_tests, aes(y=new_cases, x=year_week)) + geom_point()

IE_2020 = filter(ireland_tests, year_week < '2021-W01')
IE_2021 = filter(ireland_tests, year_week > '2020-W53', year_week < '2022-W01')
IE_2022 = filter(ireland_tests, year_week > '2021-W52', year_week < '2023-W01')
IE_2023 = filter(ireland_tests, year_week > '2022-W52')

IE_2020_m = mean(IE_2020$positivity_rate, na.rm = TRUE)
IE_2021_m = mean(IE_2021$positivity_rate, na.rm = TRUE)
IE_2022_m = mean(IE_2022$positivity_rate, na.rm = TRUE)
IE_2023_m = mean(IE_2023$positivity_rate, na.rm = TRUE)

IE_tests_20 = sum(IE_2020$tests_done, na.rm = TRUE)
IE_tests_21 = sum(IE_2021$tests_done, na.rm = TRUE)
IE_tests_22 = sum(IE_2022$tests_done, na.rm = TRUE)
IE_tests_23 = sum(IE_2023$tests_done, na.rm = TRUE)

ggplot(data=ireland_tests, aes(y=tests_done, x=year_week)) + 
  geom_point(color = 'blue') +
  geom_point(aes(y = new_cases), color = 'red')


tests_and_cases <- function(c) {
  
  COVID_testing = filter(COVID_testing, country == c)
  
  ggplot(data=COVID_testing, aes(y=tests_done, x=year_week)) + 
    geom_point(color = 'blue') +
    geom_point(aes(y = new_cases), color = 'red') +
    labs(title = paste("Tests Done and New Cases in", c, "from 2020-2023"), 
         y = paste("Number of tests done (blue)", "\n", "Number of new cases (red)"), 
         x = "Time") +
    scale_x_discrete(breaks = NULL)
}


tests_and_cases('Germany')
