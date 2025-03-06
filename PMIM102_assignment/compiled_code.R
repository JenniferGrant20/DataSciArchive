library(tidyverse)
library(RPostgreSQL) 

drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                 password="") #.rs.askForPassword('Password:')) 

user_practiceid <- "no_input"
user_option <- "no_input"

# fix print / cat

myprogram <- function(){
  print("Welcome to Jennifer’s program")
  print("")
  print("This program will allow you to run analyses on Welsh GP practice data from 2015.")
  print("")
  while (user_option != 'q'){


    print("Start Menu:")
    print("1. Start by choosing a GP practice")
    print("q. Quit")
    print("")
    user_option <- readline("Select an option:")
    
    
    if (user_option == "1"){
      print("Start selected")
      print("Please specify a GP practice to run analyses on.")
      user_practiceid <- readline("Practice ID:")
      
      id_options <- "select * from address"
      id_options_table <- dbGetQuery(con, id_options)
      
      if (any(id_options_table$practiceid == user_practiceid)){
        print("")
      } else {
        print("Invalid practice ID. Check format matches A00000 and try again.")
      }
      
      
    } else if (user_option == "q"){
      print("Thank you for using my program. Goodbye!")
      dbDisconnect(con)
      break
    } else {
      print(paste("Option", user_option, "is not defined. Please enter a listed number or letter."))
      next
    }
    
    
    while (user_option <= 4){ 
      print("Menu:")
      print("1. Ten most commonly prescribed drugs")
      print("2. Five most commonly used prescribed drug categories")
      print("3. Size of GP practice")
      print("4. Obesity and hypertension rates")
      print("b. Back to start menu")
      print("q. Quit")
      print("")
      user_option <- readline("Select an analysis to run:")
      
      
      if (user_option == "1"){
        print("Option 1 selected")
        print("Generating a table showing the top ten most commonly prescribed 
            drugs and their prescription quantities at the specified practice")
        print("")
        option_1 <- paste0("select b.chemical as drug, sum(quantity) as quantity
                           from gp_data_up_to_2015 a
                           left join chemsubstance b 
                           on b.bnfchemical = LEFT(a.bnfcode, 9)
                           where practiceid ='", user_practiceid,
                           "'group by b.chemical
                           order by quantity desc
                           limit 10")
        top_10_drugs <- dbGetQuery(con, option_1)
        print(top_10_drugs)
        print("")
        
        
      } else if (user_option == "2"){
        print("Option 2 selected")
        print("Generating a table showing the top five most commonly prescribed
            drug categories and their prescription quantities at the
            specified practice.")
        print("")
        option_2 <- paste0("SELECT sectiondesc as drug_category, total
                    FROM (SELECT o.practiceid, p.bnfchemical, o.bnfcode, 
                    p.sectiondesc, o.bnfname, o.quantity,
                    sum(o.quantity) over (partition by p.sectiondesc) as total
                    FROM gp_data_up_to_2015 o LEFT JOIN
                    bnf p
                    ON p.bnfchemical = LEFT(o.bnfcode, 9)
                    WHERE o.practiceid ='", user_practiceid,
                    "'GROUP BY p.sectiondesc, p.bnfchemical, o.bnfcode, 
                    o.bnfname, o.practiceid, o.quantity
                    ) X
                    group by sectiondesc, total
                    order by total desc
                    limit 5")
        option_2_table <- dbGetQuery(con, option_2)
        print(option_2_table)
        print("")
        
        
      } else if (user_option == "3"){
        print("Option 3 selected")
        print("")
        option_3 <- paste0("select orgcode as practiceid, sum(numerator) as 
                           quantity from qof_achievement
                           group by orgcode
                           order by quantity desc")
        option_3_table <- dbGetQuery(con, option_3)
        option_3_table <- option_3_table[-c(1), ]
        
        mean_size <- mean(option_3_table[,2])
        
        practice_size <- option_3_table[option_3_table$practiceid == 
                                        user_practiceid, ]
        
        size_result <- if (practice_size[, 2] >= mean_size) {
          print(paste("Practice", user_practiceid, "is large"))
          print("")
        } else {
          print(paste("Practice", user_practiceid, "is small"))
          print("")
        }
        
        
      } else if (user_option == "4"){
        print("Option 4 selected")
        print("")
        
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
        
        option_3 <- paste0("select orgcode as practiceid, sum(numerator) as 
                           quantity from qof_achievement
                           group by orgcode
                           order by quantity desc")
        option_3_table <- dbGetQuery(con, option_3)
        option_3_table <- option_3_table[-c(1), ]
        
        mean_size <- mean(option_3_table[,2])
        
        size_hyp_table <- merge(x = option_3_table, y = opt4_table_hyp, by = "practiceid", 
                                all.y = TRUE)
        size_ob_table <- merge(x = option_3_table, y = opt4_table_ob, by = "practiceid", 
                               all.y = TRUE)
        
        large_hyp_table <- na.omit(size_hyp_table[size_hyp_table$quantity >= mean_size, ])
        large_hyp_av <- round(mean(large_hyp_table$ratio) * 100, 1)
        
        small_hyp_table <- na.omit(size_hyp_table[size_hyp_table$quantity < mean_size, ])
        small_hyp_av <- round(mean(small_hyp_table$ratio) * 100, 1)
        
        large_ob_table <- na.omit(size_ob_table[size_ob_table$quantity >= mean_size, ])
        large_ob_av <- round(mean(large_ob_table$ratio) * 100, 1)
        
        small_ob_table <- na.omit(size_ob_table[size_ob_table$quantity < mean_size, ])
        small_ob_av <- round(mean(small_ob_table$ratio) * 100, 1)
        
        rates_table <- data.frame('practice_category' = c(user_practiceid, 
                                                          'nationwide', 
                                                          'small', 
                                                          'large'),
                                  'hypertension_rate' = c(practice_hyp, 
                                                          nat_hyp_av, 
                                                          small_hyp_av, 
                                                          large_hyp_av),
                                  'obesity_rate' = c(practice_ob, 
                                                     nat_ob_av, 
                                                     small_ob_av, 
                                                     large_ob_av))
        print("------------------")
        print("Generating a table showing the rates of hypertension and obesity 
              at the specified GP practice, across all GP practices in Wales,
              and at practices categorised as small or large.")
        print(rates_table)
        print("")
        
        rates_graph <- barplot(t(rates_table[c('hypertension_rate', 'obesity_rate')]), 
                               beside = T,        
                               names = rates_table$practice_category, 
                               density = c(20, 20), 
                               angle = c(0, 45), 
                               col = c("red", "blue"),
                               xlab = "Practice Category",
                               ylab = "Percentage Rate",
                               main = "Rates of Hypertension and Obesity",
                               ylim = c(0, (round(max(practice_hyp, nat_hyp_av, 
                                                      small_hyp_av, large_hyp_av, 
                                                      practice_ob, nat_ob_av, 
                                                      small_ob_av, large_ob_av),
                                                  0) * 1.3)),
                               legend.text=c('Hypertension','Obesity'))
        
        print("In the lower right pane, you should see a barplot displaying this
              data visually.")
        print("")
        print("Analysis complete")
        print("-----------------")
        
        
      } else if (user_option == "b"){
        print("Option b selected")
        
      } else if (user_option == "q"){
        print("Thank you for using my program. Goodbye!")
        dbDisconnect(con)
        break
      } else {
        print(paste("Option", user_option, "is not defined. Please enter a listed number or letter."))
        print("")
      }
      }
    
  }
}

myprogram()
