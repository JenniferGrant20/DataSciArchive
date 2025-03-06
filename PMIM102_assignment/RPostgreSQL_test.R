library(RPostgreSQL) 
drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                
                 password="") #.rs.askForPassword('Password:')) 
tables <- dbListTables(con) 
print(tables)

# Test (need to figure out how to pass variable to character string in a way that
# works with RPostgreSQL). The top version where user_practiceid is replaced by an
# example practiceid works but

statement <- "select practiceid, bnfcode, bnfname, sum(quantity) as total_quantity 
              from gp_data_up_to_2015 where practiceid = 'W94011' group by bnfcode, 
              bnfname, practiceid order by total_quantity desc"
option_1_table <- dbGetQuery(con, statement)

user_practiceid <- "W94011"
print(user_practiceid)
statement <- paste("select practiceid, bnfcode, bnfname, sum(quantity) as total_quantity from gp_data_up_to_2015 where practiceid ='", user_practiceid, "'group by bnfcode, bnfname, practiceid order by total_quantity desc", sep = " ")
print(statement)
option_1_table <- dbGetQuery(con, statement)

# Test
statement <- paste0("select practiceid, bnfcode, bnfname, sum(quantity) as total_quantity 
                   from gp_data_up_to_2015 where practiceid ='", user_practiceid, "'group 
                   by bnfcode, bnfname, practiceid order by total_quantity desc")
print(statement)

# dbBind version
query <- dbSendQuery(con, "select practiceid, bnfcode, bnfname, sum(quantity) as total_quantity 
                          from gp_data_up_to_2015 where practiceid = ?
                          group by bnfcode, bnfname, practiceid 
                          order by total_quantity desc")
dbBind(query, list('user_practiceid'))
dbFetch(query)
dbClearResult(query)

# sprintf version
statement_half1 <- "select practiceid, bnfcode, bnfname, sum(quantity) as total_quantity from gp_data_up_to_2015 where practiceid ='"
statement_half2 <- "'group by bnfcode, bnfname, practiceid order by total_quantity desc"
statement <- sprintf("% s %s % s", statement_half1, user_practiceid, statement_half2)
