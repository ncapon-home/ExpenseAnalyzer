## Loads all necessary data from DB 
library(magrittr)
library(ggplot2)
library(data.table)
library(rJava)
library(RJDBC)
library("RPostgreSQL")
drv <- dbDriver("PostgreSQL")
loading_status <- TRUE


############################################
#  Establish a connection with Advisory DB
###########################################
getAdvisoryConnection <- function(){
  print(paste0(Sys.time(), '- Opening connection with ADVISORY DB'))
  con <- dbConnect(drv, dbname = "advisory",
                   host = "pre-bdbadvisory01glc.bank.swissquote.ch", port = 5432, # bdbadvisory01glc - PROD, pre-bdbadvisory01glc - PRE-prod
                   user = "ncapon_pw",
                   password = 'Welcome_123')
  return(con)
}

############################################
# get a valid blacklist for a given date
###########################################
getBlackListData <- function(current_timestamp, query_type=0){
  PS_connection <- getAdvisoryConnection() # create connection to Advisory DB (on Postgres)
  print(paste0(Sys.time(), ' - Loading data from DB'))
  tryCatch( {
    query = switch(as.character(query_type),
           '0' = paste0("select * from blacklisted_isins"),
           '1' = paste0("select * from blacklisted_isins where timestamp '", current_timestamp, "' between date_in and date_end;"),
           '2' = paste0("select * from blacklisted_isins where timestamp'", current_timestamp, "' > date_end;")
    )
    print(query)
    rez <- dbGetQuery(PS_connection, query)%>%as.data.table();
    print(paste0( nrow(rez), ' rows are loaded.' ))
  },
  error = function(e){
    message("Error when loading the blacklist content.")
    message(e)
    rez <- data.table()
  })
  RPostgreSQL::dbDisconnect(PS_connection) # Closing connections
  print(paste0(Sys.time(), ' - Closing DB connections.'))
  return(rez)
}



############################################
# Get data on stock prices
###########################################
getStockPrices <- function(isin_list, max_date, lookback_months=12){
  PS_connection <- getAdvisoryConnection() # create connection to Advisory DB (on Postgres)
  print(paste0(Sys.time(), ' - Loading data from DB'))
  tryCatch( {
    min_date = max_date - 12*30
    query <- paste0("SELECT  isin_code||'_'||stock_exchange||'_'||currency as stock_key, close_date, close_mid as close from prices_histo ",
                    "WHERE close_date BETWEEN '", min_date, "' AND '", max_date, "' ")
    query = paste0(query, " and isin_code  in  ('",  paste(isin_list, collapse="', '") , "')" )
    print(query)
    rez <- dbGetQuery(PS_connection, query)%>%as.data.table();
    print(paste0(nrow(rez), ' rows are loaded.' ))
  },
  error = function(e){
    message("Error when loading prices of selected isins.")
    message(e)
    rez <- data.table()
  })
  RPostgreSQL::dbDisconnect(PS_connection) # Closing connections
  print(paste0(Sys.time(), ' - Closing DB connections.'))
  return(rez)
}



################################
## Update the existing record in the BL
################################
updateBlacklistRecord <- function(isin_code, new_end_date, comment=''){
  
  con <- getAdvisoryConnection() # create connection to Advisory DB (on Postgres)
  print(paste0(Sys.time(), ' - Attempting to update BLACKLISTED_ISINS'))
  
  tryCatch({
    query <- paste0( "UPDATE blacklisted_isins SET  date_end = '", new_end_date , 
                     "', username_op ='", UserName, "', blacklist_reason = '", comment, "' WHERE isin_code='", isin_code, "'")
    print(query)
    dbExecute(con,  query)
    print("Successfully updated BLACKLISTED_ISINS.")
  },
  error = function(e){
    message("Error when trying to update BLACKLISTED_ISINS.")
    message(e)
  })
  
  RPostgreSQL::dbDisconnect(con) # Closing connections
  print(paste0(Sys.time(), ' - Closing DB connections.'))
}

################################
## Create a new record in the BL
################################
addIsinToBlacklistDB <- function(isin_code, date_in, date_end, comment=''){
  
  con <- getAdvisoryConnection() # create connection to Advisory DB (on Postgres)
  print(paste0(Sys.time(), ' - Attempting to insert a new record into BLACKLISTED_ISINS'))
  
  tryCatch({
    query <- paste0( "INSERT INTO blacklisted_isins (isin_code, date_in, date_end, username_op, blacklist_reason) ",
                     "VALUES  ('", isin_code, "', '", date_in, "', '", date_end, "', '", UserName, "', '", comment, "') ")
    print(query)
    dbExecute(con,  query)
    print("Successfully inserted a row in table BLACKLISTED_ISINS.")
  },
  error = function(e){
    message("Error when trying to insert a new record into BLACKLISTED_ISINS.")
    message(e)
  })
  
  RPostgreSQL::dbDisconnect(con) # Closing connections
  print(paste0(Sys.time(), ' - Closing DB connections.'))
}


