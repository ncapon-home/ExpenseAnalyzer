# https://mastering-shiny.org/reactivity-objects.html
########################
rm(list = ls())
library(ggplot2)
library(gridExtra)
library(data.table)
library(magrittr)
library(zoo)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(highcharter)
library(glue)
library(RColorBrewer)


path_main  = "C:/Users/Natacha/ShinyApps/ExpenseAnalyzer/"  
RECOMPILE_TRANSACTIONS = F
RANDOM_FILL = T

path_configs =  paste0(path_main, 'configs/')
path_data =  paste0(path_main, 'data/')
path_code =  paste0(path_main, 'src/')
path_helpers =  paste0(path_code, 'helpers/')
setwd(path_main)

source(paste0(path_configs, "global_configs.R") )
source(paste0(path_helpers, "helpers_dataloaders.R") )
source(paste0(path_helpers, "helpers_computations.R") )
source(paste0(path_helpers, "helpers_graphs.R") )

# Get the list of users:
users_list = GetUsers(export_path=paste0(path_data, 'users_export/'),
                      search_pattern="user_")

# Get transaction categories:
trans_cat <- GetTransactionCategiries()

# Compile all exported data:
transactions_dt <- LoadCompiledTransactions(recompile_file = RECOMPILE_TRANSACTIONS,
                                            compiled_transactions_file = HIST_TRANSACTIONS_FILE_NAME)

MIN_TRANS_DATE <- min(transactions_dt$transaction_date)
MAX_TRANS_DATE <- max(transactions_dt$transaction_date)

# Mapping to manage column name labels
COLUMNS_NAMES <- c("Category ID" = "category_id",
                   "Transaction category" = "category_label",
                   "Description" = "category_description",
                   "Bank trace" = "default_description",
                   "User trace" = "custom_description",
                   "Vendor details" = "vendor_details",
                   "Transaction type" = "transaction_type", 
                   "Last modified on" = "last_modif_date",

                   "Transaction date" = "transaction_date",
                   "Transaction ID" = "transaction_num", 
                   "Account balance, CHF" = "account_balance",
                   "User" = "user",
                   "Amount, CHF" = "transaction_amount",
                   "Categorization method" = "cat_mode",
                   "Categorization date" = "cat_date",
                   
                   "Amount, CHF" = 'aggr_amount',
                   '%' = 'percent')

total_label = 'Total:'

contrast_palette_clrs  <- colorRampPalette(c("lightyellow", "salmon"))
 