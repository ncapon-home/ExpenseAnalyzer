#######################
# Global configurations
#######################

UNKNOWN_CATEGORY_ID = 'others'
EXPENSE_ITEM_LABEL = 'expense'
INCOME_ITEM_LABEL = 'income'
AUTOMATIC_CAT_LABEL = 'automatic'
MANUAL_CAT_LABEL = 'manual'

transaction_categories_file <- "transactions_categories.xlsx"
event_categories_file <- "events_history.xlsx"
HIST_TRANSACTIONS_FILE_NAME <- 'historical_transactions.csv'

transaction_cat_fields <- c('default_description', 'custom_description', 'vendor_details')

# define the format of headers of input files as they can be loaded from different sources
users_exports_settings <- c()
users_exports_settings[['user_1']] = list(headers =   c('transaction_date' = 'Date de transaction',
                                        'transaction_num' = 'N° de transaction', 
                                        'default_description' = 'Description 1', # automatic description by bank
                                        'custom_description' = 'Description 2', # manual description made by the user
                                        'vendor_details' = 'Description 3', # name and address of vendors / beneficiaries / creditors
                                        'income' = 'Crédit',
                                        'expense' = 'Débit',
                                        'account_balance' = 'Solde'),
                                       cols_with_thousands_sep = c('account_balance', 'income', 'expense'), # specify columns with thousands separator (the latter will be removed)
                                       thousands_sep = "'",
                                       cols_with_decimal_sep = c(),
                                       decimal_sep = ".")

# Key words to identify expenses disguised as income
disguised_expenses <- c('Crédit e-banking')

# Key words to identify income disguised as expenses
disguised_income <- c()