#################################
# Helper to get a formatted table
#################################
GetFormattedTable <- function(dt, 
                              columns_mapping=NULL,
                              paging = F,
                              filter_position = "none",
                              searching = T,
                              scrollX = T,
                              scrollY = T,
                              lenght_menue = c(25, 50, -1),
                              options_params = 'Bfrptip',
                              show_rownames = F,
                              formatting_params = NULL){
  
  # TODO: implement parameterized highlight option
  
  if (! is.null(columns_mapping)){
    dt <- dt%>%setnames(columns_mapping, names(columns_mapping))
  }
  
  rez <- DT::datatable(dt, 
                       selection = list(mode = 'single'),
                       extensions = "Buttons",
                       filter = list(position = filter_position, clear = FALSE),
                       rownames= show_rownames,
                       options = list(paging = paging, 
                                      searching = searching, 
                                      scrollX = scrollX, 
                                      scrollY = scrollY, 
                                      lengthMenu = lenght_menue,
                                      dom = options_params,
                                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),  escape = F)
  
  # Apply color formatting if needed:
  if (!is.null(formatting_params)){
    rez <- ApplyConditionalColoring(rez, formatting_params)
  }
  return(rez)
}

############################
# Apply conditional coloring
# ############################
ApplyConditionalColoring <- function(rez, formatting_params){
  # Note: formatting_params should be a list  with the following columns:
  # target_cols: e.g., 1:2
  # target_dim: e.g., 'col', 'row'
  # style_type: e.g, 'equal', 'interval'
  # value_range: e.g., c(0, 1)
  # color_schema: e.g., c('gray', 'yellow')
  # 
  if (formatting_params$style_type == 'equal'){
    
    rez <- rez%>%formatStyle( formatting_params$target_cols,
                              target = formatting_params$target_dim,
                              backgroundColor = styleEqual(formatting_params$value_range, 
                                                           formatting_params$color_schema))} 
  else{
    rez <- rez%>%formatStyle( formatting_params$target_cols,
                              target = formatting_params$target_dim,
                              backgroundColor = styleInterval(formatting_params$value_range, 
                                                              formatting_params$color_schema))
    
  }
  
  return(rez)
}

##################################
# Categorize transactions
##################################
CategorizeTransactons <- function(transactions_dt, trans_cat){
  # """
  # Categorization of the transactions is run in 2 steps:
  # 1) the hard classification is applied based on the key words
  # 2) the metadata of non-classified transactions are compared to classified transactions and the 
  #    similarity score is computed. These transactions are signaled to the users for the confirmation.
  # """
  nb_trans = nrow(transactions_dt)
  print(paste0('Categorizing ', nb_trans, ' transactions ...'))
  pb = txtProgressBar(min = 0, max = nb_trans, initial = 0) 
  transactions_dt[, 'category_id':= '']
  
  for (row_i in 1:nb_trans){
    
    margin_fields <- c('search_field', 'target_string')
    trans_details <- subset(transactions_dt[row_i, ], select = transaction_cat_fields)%>%transpose()%>%setnames('target_string')%>%data.table()
    trans_details <- data.table(c(transaction_cat_fields), trans_details)%>%setnames(margin_fields)
    
    
    for (categ in setdiff(trans_cat$category_id, 'others')){
      
      cat_details <- subset(trans_cat[category_id == categ], select = transaction_cat_fields)%>%transpose()%>%setnames('key_terms')%>%data.table()
      cat_details <- data.table(search_field = transaction_cat_fields,  cat_details)
      
      trans_details[, paste0(categ):= apply(trans_details[,.(search_field)], 1, function(x){
        DetectPattern(x, trans_details, cat_details)[[1]]})]
      
      # early stopping if the category is found
      if (any(trans_details[, get(categ)])){
        break
      }
    }
    
    dat =  subset(trans_details, select =  c(setdiff(colnames(trans_details), margin_fields ) ) ); remove(trans_details) 
    
    transaction_label <- names(dat)[which(dat == T, arr.ind = TRUE)[, 2]]%>%as.character()
    
    transactions_dt[row_i, 'category_id' := ifelse(length(transaction_label) == 0, '', unique(transaction_label))]
    
    setTxtProgressBar(pb, row_i)
  }

  transactions_dt[category_id == '', category_id:= UNKNOWN_CATEGORY_ID]
  transactions_dt[, cat_mode:= AUTOMATIC_CAT_LABEL]
  transactions_dt[, cat_date:= Sys.Date()]
  return(transactions_dt)
}


#######################
# Detect pattern
#######################
DetectPattern <- function(target_search_field, trans_details, cat_details){
  
  matched_key_terms <- c()
  match_found <- F
  
  # preprocess key terms to remove white leading and tailing white spaces and uneless symbols
  key_terms <- cat_details[search_field == target_search_field, 'key_terms']$key_terms%>%as.character()
  target_string <- trans_details[search_field == target_search_field, 'target_string']$target_string%>%as.character
  
  if (!is.na(key_terms) &  target_string != ''){
    
    key_term_list <- strsplit(key_terms, ',')[[1]]%>%tolower()
    key_term_list <- lapply(key_term_list, function(x){  sub('\"', '', x)})%>%unlist()
    key_term_list <- lapply(key_term_list, function(x){  sub('\"', '', x)})%>%unlist()
    key_term_list <- lapply(key_term_list, function(x){  trimws(x, which = c("both"))})%>%as.character()
    
    scan_list <- lapply(key_term_list, function(x){str_detect(tolower(target_string), coll(x))})%>%unlist()
    
    if (any(scan_list)){
      match_found <- T
      matched_key_terms <- key_term_list[which(scan_list)]
    }
  }
  
  return(list(match_found, matched_key_terms))
}

#########################
# Get Transactions of a given user
#########################
FilterTransactionsByUser <- function(transactions_dt, selected_user='all'){
  # filter transactions by user if needed
  if (tolower(selected_user)  == 'all'){
    return(transactions_dt)
  } else{
    return(transactions_dt[transactions_dt$user == selected_user])
  }
}


#########################
# Get expenses (outflows) only
#########################
FilterExpenses <- function(dt){
  dt = dt[transaction_type == EXPENSE_ITEM_LABEL]
  return(dt)
}

#########################
# Get revenuews (inflows) only
#########################
FilterRevenues <- function(dt){
  dt = dt[transaction_type == INCOME_ITEM_LABEL]
  return(dt)
}


#########################
# Compute Expense Structure
#########################
GetFinancialStructure <- function(date_be,
                                  date_end,
                                  selected_user='all',
                                  transaction_type = 'all',
                                  dt){
  
  dt_aggr <- dt[transaction_date >= date_be & transaction_date <= date_end]%>%
             FilterTransactionsByUser(selected_user)
  
  # filter by transaction type (expense / income/all)
  if (tolower(transaction_type) == tolower(EXPENSE_ITEM_LABEL)){
    dt_aggr <-  FilterExpenses(dt_aggr)
  } else if (tolower(transaction_type) == tolower(INCOME_ITEM_LABEL)){
    dt_aggr <-  FilterRevenues(dt_aggr)
  }  

  dt_aggr <- dt_aggr%>%
             .[, aggr_amount:= sum(transaction_amount, na.rm = T), by = .(transaction_type, category_id) ]%>%
             subset(select=c('transaction_type', 'category_id', 'aggr_amount'))%>%unique()%>%
             .[, percent:= round(aggr_amount / sum(aggr_amount, na.rm = T) * 100, 2), by = .(transaction_type)]

  return(dt_aggr)
}

#########################
# Get detailed expenses by category
#########################
GetDetailedExpensesByCategory <- function(selected_category='food_base',
                                          selected_user = ''){
  
  dt <- FilterTransactionsByUser(selected_user='all', transactions_dt)%>%
        .[category_id == selected_category]
  return(dt)
}

#########################
# Get cumulative expense by category
#########################
GetTimeAggregatedData <- function(date_be,
                                  date_end,
                                  dt,
                                  selected_user = 'all',
                                  frequency = 'monthly',
                                  selected_categories = c(),
                                  transaction_type = 'expense') {

    dt_aggr <- dt[transaction_date >= date_be & transaction_date <= date_end] %>%
    FilterTransactionsByUser(selected_user)
  
  if (transaction_type == EXPENSE_ITEM_LABEL) {
    dt_aggr <-  FilterExpenses(dt_aggr)
  } else {
    dt_aggr <-  FilterRevenues(dt_aggr)
  }
  
  dt_aggr <- dt_aggr %>%
    .[, ':='(
      year_d =  format(as.Date(transaction_date), '%Y'),
      month_d = format(as.Date(transaction_date), '%m'),
      week_d = format(as.Date(transaction_date), '%V')
    )]
  
  print(dt_aggr)
  
  if (frequency == 'monthly') {
    dt_aggr[, time_label := as.integer(paste0(year_d, month_d))]
  } else if (frequency == 'yearly') {
    dt_aggr[, time_label := as.integer(year_d)]
  } else {
    dt_aggr[, time_label := as.integer(paste0(year_d, week_d))]
  }
  
  dt_aggr[, c('year_d', 'month_d', 'week_d') := NULL]
  dt_aggr <-
    dt_aggr[, aggr_amount := sum(transaction_amount, na.rm = T), by = .(transaction_type, category_id, time_label)] %>%
    subset(select = c('time_label', 'category_id', 'aggr_amount')) %>%
    unique()
  
  
  margins <-
    data.table(expand.grid(unique(dt_aggr$time_label),  trans_cat$category_id))
  margins <-
    margins %>% setnames(c('time_label', 'category_id')) %>% setorder('time_label', 'category_id')
  
  dt_aggr <-
    merge(margins,
          dt_aggr,
          by = c('time_label', 'category_id'),
          all.x = T)
  dt_aggr[is.na(aggr_amount), aggr_amount := 0]
  

  # create cumulative amount:
  dt_aggr <- setorder(dt_aggr, 'time_label') %>%
    .[, cum_amount := cumsum(aggr_amount), by = .(category_id)]
  
  return(dt_aggr)
}

# # # tests
# financial_breakdown <- GetFinancialStructure(
#                 date_be = '2022-06-01',
#                 date_end = '2022-08-20',
#                 selected_user='all',
#                 group_list = c('category_id'),
#                 transaction_type = 'all',
#                 transactions_dt)
# 
# test <- GetTimeAggregatedData(transactions_dt,
#                                   selected_user = 'all',
#                                   frequency = 'weekly',
#                                   selected_categories=c(),
#                                   transaction_type = 'expense')
# 
# plt <- PlotTimeDynamics(test,
#                         target_col = 'aggr_amount',
#                         transaction_type = 'expense')
# grid.arrange(plt)

# # TODO
#  - create a line plot for selected expenses ( without aggregation)
#  - create a barchart plot

