# TODO: enable loading only the delta.
# TODO: introduce an avanced NLP based categoriation that uses string similarities

#############################
# Get list of application users
#############################
GetUsers <- function(export_path = paste0(path_data, 'users_export/'),
                     search_pattern = "user_") {
  return(list.files(
    path = export_path,
    pattern = search_pattern,
    full.names = FALSE,
    recursive = FALSE
  )
  )
}

###############################
# Load transaction categories
###############################
GetTransactionCategiries <- function(){
  rez <- xlsx::read.xlsx(paste0(path_data, 'metadata/', 
                                transaction_categories_file), sheetIndex = 1)%>%as.data.table()%>%
        .[, last_modif_date:=as.Date(last_modif_date)]
  return (rez)
}


###############################
# Load transaction categories
###############################
SaveTransactionCategories <- function(dt){
   xlsx::write.xlsx(df, file= paste0(path_data, 'metadata/', transaction_categories_file), sheetIndex = 1)
}


#############################
# Load existing file with previously compiled user transactions
#############################
LoadCompiledTransactions <- function(recompile_file = F,
                                     compiled_transactions_file = 'historical_transactions.csv'){
  # TODO: implement loading delta only (by comparing with archived copies) and keeping the historical categorized transactions
  
  file_exists = file.exists(paste0(path_data, compiled_transactions_file))
  
  if (!recompile_file & file_exists){
    
    print(paste0('Loading existing file: ', compiled_transactions_file))
    transactions_dt <-  fread(paste0(path_data, compiled_transactions_file))
    
  } else {
    
    if (file_exists){
     
      file_name <- strsplit(compiled_transactions_file, "\\.")[[1]][[1]]
      archive_dir <- paste0(path_data, 'archives/')
      
      # move the file to archives
      file.copy(from = paste0(path_data, compiled_transactions_file),
                to = paste0(path_data, 'archives/')  )
      
      new_file_name <-  paste0(file_name, '_', format(Sys.time(), "%Y%m%d_%H%M%S"), '.csv')
      file.rename(from=file.path(archive_dir, compiled_transactions_file),
                  to=file.path(archive_dir, new_file_name))

    }
    
    transactions_dt <- CompileUserTransactions(compiled_transactions_file)
    SaveTransactions(transactions_dt, file_name = compiled_transactions_file)
    
  }
  
  # Randomly fill transaction amounts for DEMO purposes:
  if (RANDOM_FILL){
    nb_income_items <- nrow(transactions_dt[ transaction_type == INCOME_ITEM_LABEL])
    transactions_dt[ transaction_type == INCOME_ITEM_LABEL, transaction_amount:=runif(nb_income_items, min=8000, max=12000)]
    
    nb_expense_items <- nrow(transactions_dt[ transaction_type == EXPENSE_ITEM_LABEL])
    transactions_dt[ transaction_type == EXPENSE_ITEM_LABEL, transaction_amount:=runif(nb_expense_items, min=20, max=500)]
  }
  
  transactions_dt[, transaction_amount:=round(transaction_amount, 2)]
  return(transactions_dt)
}


###############################
# Load transaction categories
###############################
SaveTransactions <- function(dt, file_name){
  write.csv(dt, paste0(path_data, file_name), row.names = FALSE, fileEncoding = 'UTF-8')
}

#############################
# Load raw data from a specified user directory
#############################
CompileUserTransactions <- function(compiled_transactions_file = 'historical_transactions.csv'){

  print('Compiling users transactions ...')
  transactions_dt = data.table()
  
  for (user_j in users_list){
    files_to_load = list.files(path = paste0(path_data, 'users_export/', user_j),
                               pattern = NULL, full.names = TRUE)
    selected_cols = users_exports_settings[[user_j]][['headers']]
    
    for (file_k in files_to_load){
      print(paste0('Loading file: ', file_k))
      export_tmp <- fread(file_k)%>%select(., as.character(selected_cols))%>%
        setnames(names(selected_cols))
      export_tmp[, 'user':= user_j]
      
      cols_to_process = users_exports_settings[[user_j]][['cols_with_thousands_sep']]
      sep = users_exports_settings[[user_j]][['thousands_sep']]
      export_tmp[, (cols_to_process) := lapply(.SD, function(x) as.numeric(gsub( sep, "", x))), .SDcols = cols_to_process]
      transactions_dt <- rbind(transactions_dt, export_tmp)
    }
  }
  
  # remove entries with intermediary computations of banking fees:
  transactions_dt[, 'transaction_date':= sapply(transaction_date, function(x) {
               x = strsplit(as.character(x), '\\.')[[1]];
              return(paste0(x[3],'-', x[2],'-', x[1]))})] 
  
  
  transactions_dt <- transactions_dt[!(is.na(expense) & is.na(income))]%>%unique()%>%
                  setorder('user', 'transaction_date')

  # Checking for duplicated rows:
  transactions_dt[ , sanity_check:= .N, by = .(user, transaction_num)]
  if (nrow(transactions_dt[sanity_check > 1])) {
      print('Found duplicated entried:')
    transactions_dt[sanity_check > 1]%>%print()
  }
  transactions_dt[, sanity_check:=NULL]
  
  # TODO: aggrerate expences / income if a transaction was split (not clear how to treat account balance in this case!)
  # for (col_j in c('expense', 'income')){
  #   users_exports[, paste0(col_j):= sum(get(col_j), na.rm = T), by = .(user, transaction_num)]
  # }

  # Label each transaction as income or expense:
  transactions_dt[, 'transaction_type':= ifelse(!is.na(income), INCOME_ITEM_LABEL, EXPENSE_ITEM_LABEL)]
  transactions_dt[transaction_type == EXPENSE_ITEM_LABEL, transaction_amount:= expense]
  transactions_dt[transaction_type == INCOME_ITEM_LABEL, transaction_amount:= income]
  transactions_dt[, c('expense', 'income'):=NULL]
  transactions_dt[, transaction_date:= as.Date(transaction_date)]
  
  # Categorize transactions:
  trans_cat <- GetTransactionCategiries()
  transactions_dt <- CategorizeTransactons(transactions_dt, trans_cat)
  
  return(transactions_dt)
}


# ##################################
# # Correct transactions disguised as Income
# ##################################
# CorrectTransactonDirection <- function(dt){
#   if (length(disguised_expenses) > 0){
#     dt[tolower(default_description) %like% lapply(disguised_expenses, tolower),
#                  ':='(expense = income, income = NA)]
#     nb_rows_affected <- dt[tolower(default_description) %like% lapply(disguised_expenses, tolower)]%>%nrow()
#     print(paste0('Nb. of expense transactions disguised as income: ', nb_rows_affected, '. Modifications are made.'))
#   }
#   
#   if (length(disguised_income) > 0){
#     
#     dt[tolower(default_description) %like% lapply(disguised_income, tolower),
#                     ':='(expense = income, income = NA)]
#     nb_rows_affected <- dt[tolower(default_description) %like% lapply(disguised_income, tolower)]%>%nrow()
#     print(paste0('Nb. of income transactions disguised as expense: ', nb_rows_affected, '. Modifications are made.'))
#     
#   }
#   return(dt)
# }





