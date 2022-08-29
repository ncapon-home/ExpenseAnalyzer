library(shiny)
library(shinydashboard)
library(shinyjs)

# useful resources:
#  DT options: https://rstudio.github.io/DT/options.html, https://datatables.net/reference/option/dom
#  DT styling: https://rstudio.github.io/DT/010-style.html
#  Highcharts package: https://www.highcharts.com/blog/tutorials/highcharts-for-r-users/, 
#  https://rpubs.com/techanswers88/piechart,
#   https://towardsdatascience.com/exploring-highcharts-in-r-f754143efda7
#   Highcharter Cookbook: https://www.tmbish.me/lab/highcharter-cookbook/


# TODO: recreate the following structure
# Page 1: Update transaction categories (priority 3)
# Page 2: Browse transactions and save category updates (priority 2)
# Page 3: Analyze the expense structure and dynamics for selected users / dates (priority 1)
# Page 4: expense / revenue simulator (priority 4)
# Page 5: enter holidays ( to highlight holiday periods on the graphs)
# Page 6 (Optional): Tax simulator (last priority)

navbarPage(
  shinyjs::useShinyjs(),


  # tabPanel('Special events register',    
  #          column(12,
  #                 div(style = "background-color:#ebf1fa; border: solid 2px #232323; width:100%; border-color: #337ab7;",  
  #                     div(style = "text-align:center; padding:10px; margin-bottom: 10px;
  #                             background-color: #a5bccf;", h3("Special events and holidays") ),
  #                     div(style = "padding:10px;", DT::dataTableOutput("tab_special_events"))
  #                 ), # end div
  #                 br()
  #                 )
  # ),
  
  tabPanel('Transactions',
           tags$style("
                 .checkbox { /* checkbox is a div class*/
                     line-height: 30px;
                     margin-bottom: 20px; /*set the margin, so boxes don't overlap*/
                    # background: #f0f0f0;
                  }  
               
               input[type='checkbox']{ /* style for checkboxes */
                  width: 25px; /*Desired width - or transform: scale(2); */
                  height: 25px; /*Desired height*/
                  line-height: 30px;
                  background:#337ab7;
                  
               }
 

               .checkbox span { 
                    margin-left: 20px;  /*set the margin, so boxes don't overlap labels*/
                    line-height: 30px; 
                    font-weight:bold; 
                    font-size: 16px; 
               }
               .box.bg-yellow { background-color: #FFFF00 !important; color: #000000 !important; }
               
                  ",
                      HTML("hr {border-top: 1px solid #337ab7 ;}  ## hr-lines
                        .box.box-solid.box-primary>.box-header {  ## box N 1
                                    color:#fff;
                                    background: #337ab7}
                        .box.box-solid.box-primary{
                              border:2px solid #337ab7;
                              }"
                      )
           ),

           fluidRow(
             column(12, checkboxInput('enable_manual_categororisation',  'Enable manual categorization', value = F))
           ),
           fluidRow(
             column(12,
                    div(style = "background-color:#ebf1fa; border: solid 2px #232323; width:100%; border-color: #337ab7;",  
                        div(style = "text-align:center; padding:10px; margin-bottom: 10px;
                                background-color: #a5bccf;", 
                            h3(paste0("Historical transactions from ", as.character(MIN_TRANS_DATE), " to ", as.character(MAX_TRANS_DATE))) ),
                        div(style = "padding:10px;", DT::dataTableOutput("tab_historical_transactions")),
                        # div(style = "text-align:center; margin-top: -10px;", h3(paste0("*) Stocks highlighted in yellow will be 
                        #                                                              whitelisted within the next ", ReleaseAlertDays, " days.")))
                    ),
  
             ) # end column
           ) # end row
  ), # End Panel 'Transactions'
  
  
  tabPanel('Transaction categories',  
           tags$style(
             ".navbar-nav li a {
              font-size: 20px;
              font-weight: bold;
            }
            "),
           tags$head(
             tags$link(
               rel = "stylesheet",
               href = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.css"
             ),
             tags$script(
               src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-contextmenu/2.8.0/jquery.contextMenu.min.js"
             )
           ),
           column(12,
                  div(style = "background-color:#ebf1fa; border: solid 2px #232323; width:100%; border-color: #337ab7;",  
                      div(style = "text-align:center; padding:10px; margin-bottom: 10px;
                              background-color: #a5bccf;", h3("Transaction categories") ),
                      div(style = "padding:10px;", DT::dataTableOutput("tab_trans_cat")),
                      # div(style = "text-align:center; margin-top: -10px;", h3(paste0("*) Stocks highlighted in yellow will be 
                      #                                                              whitelisted within the next ", ReleaseAlertDays, " days.")))
                  ) # end 1st box
                  
                  # div(style = "background-color:#ebf1fa; border: solid 2px #232323; width:100%; border-color: #337ab7; margin-top: 25px;",  
                  #     div(style = "text-align:center; padding:10px; background-color: #a5bccf; margin-bottom: 10px;", h3("Modify the selected record") ),
                  #     br(),
                  #     
                  #     fluidRow(  
                  #       column(1,  div(style = "padding-left:10px; text-align:center;", disabled(textInput("category_id", "Category ID", value="")))),
                  #       column(1,  div(style = "text-align:center;", textInput("categoy_label", "Category", value=''))),
                  #       column(2,  div(style = "text-align:center;", textInput("category_description", "Description", value=''))),
                  #       column(2,  div(style = "text-align:center;", textInput("default_description", "Default trace - key words", value=''))),
                  #       column(2,  div(style = "text-align:center;", textInput("custom_description", "Custom trace - key words", value=''))),
                  #       column(2,  div(style = "text-align:center;", textInput("vendor_details", "Vendor details", value=''))),
                  #       column(1,  div(style = "text-align:center;", disabled(dateInput("last_modif_date", "Last modified", value='')))),
                  #       # column(4, div(style = "padding-right:10px;", selectInput(inputId = "comments_BL", label = "Select a reason", choices = c())))
                  #     ), # end fluidRow
                  #     
                  #     fluidRow( div(style = "text-align:center;",   
                  #                   actionButton("modify_cat_record", "Save modifications",  
                  #                                style="color: #fff; background-color: #5f778a  ; border-color: #2e6da4; margin-top:20px;",
                  #                                width = "250px") 
                  #     ),
                  #     br()
                  #    ) #end div
                  # )
           ) #end column
           
  ),
  
  
  tabPanel('Analysis of revenues & expenses',
           fluidRow(
             column(3,
                    dateRangeInput(inputId = "analysis_range",   ## time period for simulations
                                   label = "Historical period:",
                                   start =  MIN_TRANS_DATE, 
                                   end = MAX_TRANS_DATE,
                                   min =  MIN_TRANS_DATE, 
                                   max = MAX_TRANS_DATE)
                    
             ),
             column(3,
                    selectInput("selected_user", "Select a user:",  c(users_list, 'all'), selected = "all"),
                    
             ),
             
           ), # end row

           fluidRow(
             column(12,
                    div(style = "background-color:#ebf1fa; border: solid 2px #232323; width:100%; border-color: #337ab7;",  
                        div(style = "text-align:center; padding:10px; margin-bottom: 10px;
                                background-color: #a5bccf;", h3('Breakdown by category')),
                        
                        fluidRow(
                          column(12,
                            div(style = "text-align:left; padding:20px;",
                                   selectInput("selected_trans_type", "Transaction type to analyze:",
                                               c(EXPENSE_ITEM_LABEL, INCOME_ITEM_LABEL), selected = EXPENSE_ITEM_LABEL),
                                   
                            )
                          ),
                        ),
                        
                        fluidRow(
                          column(6,
                                 highchartOutput("plot_financial_structure", width = "100%", height = "600px")
                                 
                                 
                          ), # end column
                          
                          column(6,
                                 div(style = "background-color:white; border:solid 1px #232323; width: 95%;
                             margin-left:10px;   padding: 20px; margin-top: 10px; margin-bottom: 10px;     border-color: #337ab7",

                                     DT::dataTableOutput("tab_summary"),
                                     
                                 )
                          ), # end column
                        ), # end row
                        
                        
                        br(), hr(),
                        div(style = "padding:10px;", htmlOutput('header_category_details')),
                        div(style = "padding:10px;", DT::dataTableOutput("tab_category_details")),
                        
                    ), # end box
                    
             ) # end column
           ), # end row
           
           fluidRow(
             column(12,
                    div(style = "background-color:#ebf1fa; border: solid 2px #232323; width:100%; border-color: #337ab7;",  
                        div(style = "text-align:center; padding:10px; margin-bottom: 10px;
                                background-color: #a5bccf;", h3('Account dynamics')),
                        
                        fluidRow(
                          column(3,
                                 div(style = "text-align:left; padding:20px;",
                                     selectInput("selected_trans_type2", "Transaction type to analyze:",
                                                 c(EXPENSE_ITEM_LABEL, INCOME_ITEM_LABEL), selected = EXPENSE_ITEM_LABEL),
                                     
                                 )
                          ),
                          column(3,
                                 div(style = "text-align:left; padding:20px;",
                                     selectInput("selected_time_frequency", "Select time frequency:",
                                                 c('Weekly' = 'weekly',
                                                   'Monthly' = 'monthly',
                                                   'Yearly' = 'yearly'), selected = 'weekly'),
                                     
                                 )
                          ),
                          
                          column(3,
                                 div(style = "text-align:left; padding:20px;",
                                     selectInput("selected_measure_type", "Select variable:",
                                                 c('Amount, CHF' = 'aggr_amount',
                                                   'Cumulative amount, CHF' = 'cum_amount'), selected = 'cum_amount'),
                                     
                                 )
                          ),
                        ),
                        
                        fluidRow(
                          column(12,
                                 plotOutput("plot_account_dynamics", width = "100%", height = "600px")
                                 
                                 
                          )
                        ) # end row

                    ), # end box
                    
             ) # end column
           ) # end row
           
  ) # End Panel 'Account dynamics'

)