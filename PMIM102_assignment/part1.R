library(tidyverse)
library(RPostgreSQL) 
library(moments)
library(ggpubr)

drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                 password="") #.rs.askForPassword('Password:')) 

user_practiceid <- "no_input"
user_option <- "no_input"

myprogram <- function(){
  print("Welcome to Jennifer’s program")
  print("")
  user_option <- "no_input"
  while (user_option != 'q'){
    print("Menu:")
    print("1. Ten most commonly prescribed drugs")
    print("2. Five most commonly used prescribed drug categories")
    print("3. Size of GP practice")
    print("4. Obesity and hypertension rates")
    print("q. Quit")
    print("")
    user_option <- readline("Select an analysis to run:")
    if (user_option == "1"){
      print("Option 1 selected")
    } else if (user_option == "2"){
      print("Option 2 selected")
    } else if (user_option == "3"){
      print("Option 3 selected")
    } else if (user_option == "4"){
      print("Option 4 selected")
    } else if (user_option == "q"){
      print("Thank you for using my program. Goodbye!")
    } else {
      print(paste0("Option", user_option, "is not defined. Please enter a 
                     listed number or letter."))
      print("")
      }
  }
}

myprogram()

# Code for beginning of analyses [ WORKS ]
print("Please specify a GP practice to run analyses on.")
user_practiceid <- readline("Practice ID:")


# Top 10 drugs code [ WORKS ]
option_1 <- paste0("select practiceid, bnfname as drug_name, sum(quantity) as 
                   total from gp_data_up_to_2015 where practiceid ='", 
                   user_practiceid, "'group by bnfcode, bnfname, practiceid 
                   order by total desc")
option_1_table <- dbGetQuery(con, option_1)
top_10_drugs <- option_1_table[1:10, 2:3]
print(top_10_drugs)

#select b.chemicaldesc as drug, sum(quantity) as quantity
#from gp_data_up_to_2015 a
#left join bnf b on b.bnfchemical = LEFT(a.bnfcode, 9)
#where practiceid = 'W94011'
#group by b.chemicaldesc
#order by quantity desc

# select b.chemical as drug, sum(quantity) as quantity
#from gp_data_up_to_2015 a
#left join chemsubstance b 
#on b.bnfchemical = LEFT(a.bnfcode, 9)
#where practiceid ='W94011'
#group by b.chemical
#order by quantity desc


# Top 5 drug categories code
option_2 <- paste0("SELECT sectiondesc as drug_category, total
                    FROM (SELECT o.practiceid, p.bnfchemical, o.bnfcode, p.sectiondesc, o.bnfname, o.quantity,
                          SUM(o.quantity) over (partition by p.sectiondesc) as total
                          FROM gp_data_up_to_2015 o 
                          LEFT JOIN bnf p
                          ON p.bnfchemical = LEFT(o.bnfcode, 9)
                          WHERE o.practiceid ='", user_practiceid,
                          "'GROUP BY p.sectiondesc, p.bnfchemical, o.bnfcode, o.bnfname, o.practiceid, o.quantity
                    ) X
                    group by sectiondesc, total
                    order by total desc
                    limit 5")
option_2_table <- dbGetQuery(con, option_2)
print(option_2_table)

# Size category code
print("Option 3 selected")
print("Distinguishing whether the practice is above or below average in
              number of prescriptions as a proxy for practice size. Please be
              patient as this analysis takes approximately 20 seconds.")
option_3 <- paste0("select distinct(practiceid), sum(quantity) over (partition 
                   by practiceid) as total from gp_data_up_to_2015
                   group by practiceid, quantity
                   order by total desc")
option_3_table <- dbGetQuery(con, option_3)

mean_size <- mean(option_3_table[,2])

option_3_practiceid <- paste0("select distinct(practiceid), sum(quantity) over 
                               (partition by practiceid) as total 
                               from gp_data_up_to_2015
                               where practiceid = '", user_practiceid, 
                              "'group by practiceid, quantity
                               order by total desc")
option_3_pid_table <- dbGetQuery(con, option_3_practiceid)

practice_size <- if (option_3_pid_table[, 2] > mean_size) {
  print(paste("Practice", user_practiceid, "is large"))
  print("")
} else {
  print(paste("Practice", user_practiceid, "is small"))
  print("")
}


# Hypertension and obesity rate comparison
opt4 <- paste0("select orgcode as practiceid, ratio, centile, indicator
                   from qof_achievement
                   where indicator = 'HYP001'
                   or indicator = 'OB001W'")
opt4_table <- dbGetQuery(con, opt4)

opt4_table_hyp <- opt4_table[opt4_table$indicator == 'HYP001', ]
nat_hyp_av <- round(mean(opt4_table_hyp$ratio) * 100, 1)
practice_hyp <- opt4_table_hyp[opt4_table_hyp$practiceid == user_practiceid, ]
practice_hyp <- practice_hyp$ratio
practice_hyp <- round(practice_hyp * 100, 1)

opt4_table_ob <- opt4_table[opt4_table$indicator == 'OB001W', ]
nat_ob_av <- round(mean(opt4_table_ob$ratio) * 100, 1)
practice_ob <- opt4_table_ob[opt4_table_ob$practiceid == user_practiceid, ]
practice_ob <- practice_ob$ratio
practice_ob <- round(practice_ob * 100, 1)

    # compare to other practices in same size category
# if(exists("option_3_table") == FALSE){

print("Please be patient as this analysis will take approximately 20 seconds")
  option_3 <- paste0("select distinct(practiceid), sum(quantity) over (partition 
                     by practiceid) as total from gp_data_up_to_2015
                     group by practiceid, quantity
                     order by total desc")
  option_3_table <- dbGetQuery(con, option_3)
  
  mean_size <- mean(option_3_table[,2])
  
# }

size_hyp_table <- merge(x = option_3_table, y = opt4_table_hyp, by = "practiceid", 
                        all.y = TRUE)
size_ob_table <- merge(x = option_3_table, y = opt4_table_ob, by = "practiceid", 
                       all.y = TRUE)

large_hyp_table <- na.omit(size_hyp_table[size_hyp_table$total > mean_size, ])
large_hyp_av <- round(mean(large_hyp_table$ratio) * 100, 1)
  
small_hyp_table <- na.omit(size_hyp_table[size_hyp_table$total < mean_size, ])
small_hyp_av <- round(mean(small_hyp_table$ratio) * 100, 1)

large_ob_table <- na.omit(size_ob_table[size_ob_table$total > mean_size, ])
large_ob_av <- round(mean(large_ob_table$ratio) * 100, 1)

small_ob_table <- na.omit(size_ob_table[size_ob_table$total < mean_size, ])
small_ob_av <- round(mean(small_ob_table$ratio) * 100, 1)

    # report numerical values

rates_table <- data.frame('practice_category' = c(user_practiceid, 'nationwide', 
                                                 'small', 'large'),
                          'hypertension_rate' = c(practice_hyp, nat_hyp_av, 
                                                  small_hyp_av, large_hyp_av),
                          'obesity_rate' = c(practice_ob, nat_ob_av, small_ob_av, 
                                                large_ob_av))
print(rates_table)

# report visualisations

rates_graph <- barplot(t(rates_table[c('hypertension_rate', 'obesity_rate')]), 
               beside = T,        
               names = rates_table$practice_category, 
               density = c(20, 20), 
               angle = c(0, 45), 
               col = c("red", "blue"),
               xlab = "Practice Category",
               ylab = "Percentage Rate",
               main = "Rates of Hypertension and Obesity",
               ylim = c(0, (round(max(practice_hyp, nat_hyp_av, small_hyp_av,
                                      large_hyp_av, practice_ob, nat_ob_av, 
                                      small_ob_av, large_ob_av), 0) * 1.3)),
               legend.text=c('Hypertension','Obesity'))

