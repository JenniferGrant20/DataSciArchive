library(RPostgreSQL) 
drv <- dbDriver('PostgreSQL') 
con <- dbConnect(drv, dbname='gp_practice_data', host='localhost',                  
                 port=5432, user='postgres',                  
                 password="") #.rs.askForPassword('Password:')) 
tables <- dbListTables(con) 
print(tables)

df_address <- dbGetQuery(con, "select * from address")
