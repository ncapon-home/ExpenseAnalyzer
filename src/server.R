server <- function(input, output, session) {
  new_transaction_category <- reactiveVal()
  updated_trans_index <-  reactiveVal()
  selected_category_id <- reactiveVal()
  categorization_updated <- reactiveVal(0)

  # shinyjs::runjs("$('#comments_BL').attr('maxlength', 200)")
  # shinyjs::runjs("$('#comments_WL').attr('maxlength', 200)")
  
  # Initial data:
  # rez_BL_init <- refreshtData("blacklist", init_blacklisted_isins, Sys.time() %m-% days(LagDays))
  # rez_WL_init <- refreshtData("whitelist", init_blacklisted_isins, Sys.time() %m-% days(LagDays))
  
  datasets <- reactiveValues(trans_cat=trans_cat,
                             transactions_dt=transactions_dt)
 
  # financial breakdown of expenses / income streams:
  financial_breakdown  <- eventReactive(  list(input$analysis_range, 
                                                input$selected_user, 
                                                input$selected_trans_type,
                                                categorization_updated()), { 
                                                  
    print(datasets$transactions_dt[transaction_num == 'AS01681HJ0035898'])   
                                                  
    rez <- GetFinancialStructure(
      date_be =  input$analysis_range[1],
      date_end = input$analysis_range[2],
      selected_user=input$selected_user,
      transaction_type = input$selected_trans_type,
      datasets$transactions_dt)%>%
      merge(datasets$trans_cat[,.(category_id, category_label)], by = 'category_id', all.x = T)

    return(rez)
    })   
  
  # content of the summary table:
  summary_tab <- eventReactive(financial_breakdown(), {
    tmp  <- copy(financial_breakdown())%>%
      .[,.(category_label, category_id, aggr_amount, percent)]%>%setorder(-'aggr_amount')%>%
      rbind(data.table('category_label' = total_label,
                       'category_id' = '',
                       'aggr_amount' = sum(financial_breakdown()$aggr_amount),
                       'percent' = sum(financial_breakdown()$percent)))
  })
  
  # time-framed aggregation
  time_frequency_aggregates <- eventReactive(
    list(
      input$analysis_range,
      input$selected_user,
      input$selected_trans_type2,
      input$selected_time_frequency,
      datasets$transactions_dt,
      categorization_updated()
    ),
    {
       rez <- GetTimeAggregatedData(
         date_be =  input$analysis_range[1],
         date_end = input$analysis_range[2],
         dt = datasets$transactions_dt,
         selected_user = input$selected_user,
         frequency = input$selected_time_frequency,
         selected_categories = c(),
         transaction_type = input$selected_trans_type2
       )
       print(rez)
      return(rez)
    }
  )
  
  ######################### TRANSACTION CATEGORIES TAB ############################
  
  ##########################
  ### Display  the metadata of actual categories
  ##########################
  output$tab_trans_cat <- DT::renderDataTable({
    validate(need(datasets$trans_cat, message = "Dataframe not found"))  

    cols_to_rename <- intersect(COLUMNS_NAMES, colnames(datasets$trans_cat))
    columns_mapping <- COLUMNS_NAMES[which(sapply(COLUMNS_NAMES, function(x)  x %in% cols_to_rename )) ]
    
    GetFormattedTable(copy(datasets$trans_cat), 
                      columns_mapping=columns_mapping,
                      paging = F,
                      filter_position = 'top',
                      searching = T,
                      scrollX = T,
                      scrollY = T,
                      lenght_menue = c(10, 20, -1),
                      options_params = 'frpti',
                      show_rownames = F)
      
  })
  
  
  ##########################
  ### Display  historical transactions
  ##########################

  GetTransactions <- function(tmp){
    DT::renderDataTable({
      
      cols_to_rename <- intersect(COLUMNS_NAMES, colnames(tmp))
      columns_mapping <- COLUMNS_NAMES[which(sapply(COLUMNS_NAMES, function(x)  x %in% cols_to_rename )) ]
      
      formatting_params <- list()
      formatting_params[['target_cols']] = names(COLUMNS_NAMES[COLUMNS_NAMES == 'category_id'])
      formatting_params[['target_dim']] = 'row'
      formatting_params[['style_type']] = 'equal'
      formatting_params[['value_range']] = c(UNKNOWN_CATEGORY_ID)
      formatting_params[['color_schema']] = c('lightyellow')
      
      GetFormattedTable(tmp,
                        columns_mapping=columns_mapping,
                        paging = F,
                        filter_position = 'top',
                        searching = T,
                        scrollX = T,
                        scrollY = T,
                        lenght_menue = c(20, 40, -1),
                        options_params = 'frpti',
                        show_rownames = F,
                        formatting_params = formatting_params)
      
    })
  }
  
  output$tab_historical_transactions <-  GetTransactions(copy(datasets$transactions_dt)[, - c('account_balance')])
  
  #####################################
  # Call a pop-up window to enable a manual categorization of transactions
  ######################################
  observeEvent(input$tab_historical_transactions_cell_clicked, {
    
    if (input$enable_manual_categororisation){

        if (!is.null(input$tab_historical_transactions_cell_clicked$row)  ) {

          row <- input$tab_historical_transactions_cell_clicked$row
          col <- input$tab_historical_transactions_cell_clicked$col

          
          selected_trans <- datasets$transactions_dt[row, ] 
          updated_trans_index(row) # update global variable
          available_categories <- sort(datasets$trans_cat[transaction_type  == selected_trans$transaction_type]$category_id)

          showModal(  modalDialog( size = "m",
                                   title =   paste0("Assign category to transaction: ",  selected_trans$transaction_num), 
                                   easyClose = T,

                                   selectInput('new_transaction_category', 'New transaction category:', 
                                               choices = available_categories,
                                               selected = selected_trans$category_id),
                                   footer=tagList(
                                     actionButton('submit', 'Submit'),
                                     modalButton('cancel')
                                   )
          ))

        }
    }
    
  })
  
  
  ##############################################
  # Change transaction category
  ##############################################
  observeEvent(input$submit, {
    removeModal()
    new_transaction_category(input$new_transaction_category)
    
    datasets$transactions_dt[updated_trans_index(), ':=' ("category_id" = new_transaction_category(),
                                                          "cat_mode" = MANUAL_CAT_LABEL,
                                                          "cat_date" = Sys.Date())]
    # Recompute aggregated values
    categorization_updated(categorization_updated() + 1)

    # Force the DT output to refresh:
    output$tab_historical_transactions <-  GetTransactions(copy(datasets$transactions_dt)[, - c('account_balance')])
    
    # Save transactions
    SaveTransactions(dt = datasets$transactions_dt, file_name = HIST_TRANSACTIONS_FILE_NAME)
    
    selected_category_id(NULL)
  })

  
  ################## TAB: ACCOUNT DYNAMICS #######################

  # Pie Chart
  output$plot_financial_structure <- renderHighchart({
    
    if ( nrow(financial_breakdown()) > 0){
      pie_chart <-
        highcharter::hchart(financial_breakdown(),
                            type = 'pie', 
                            hcaes(x = category_id, label=category_label, y = percent)) %>%
        hc_title(
          text = '',
          align =  'center',
          style = list(fontWeight = 'bold', fontSize = '16px')
        ) %>%
        hc_tooltip(enabled = T) %>%
        hc_subtitle(
          text = 'In %',
          align = 'center',
          style = list(fontWeight = 'bold')
        ) %>%
        hc_add_theme(hc_theme_ffx()) %>%
        hc_credits(enabled = T, text = '') %>% #hc_tooltip(enabled = T)%>%
        hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, format =
                                                      "{point.label}:{point.y}%")))
    }
  })
  
  # Summary table with the breakdown
  output$tab_summary <- DT::renderDataTable({
    
    validate(need(list(summary_tab(), datasets$trans_cat), message = "Datasets not found"))  
    
    cols_to_rename <- intersect(COLUMNS_NAMES, colnames(summary_tab()))
    columns_mapping <- COLUMNS_NAMES[which(sapply(COLUMNS_NAMES, function(x)  x %in% cols_to_rename )) ]
    
    formatting_params <- list()
    formatting_params[['target_cols']] = names(COLUMNS_NAMES[COLUMNS_NAMES == 'category_label'])
    formatting_params[['target_dim']] = 'row'
    formatting_params[['style_type']] = 'equal'
    formatting_params[['value_range']] = c(total_label)
    formatting_params[['color_schema']] = c('lightblue')
    
    GetFormattedTable(copy(summary_tab()),
                      columns_mapping=columns_mapping,
                      paging = F,
                      filter_position = 'none',
                      searching = F,
                      scrollX = F,
                      scrollY = F,
                      lenght_menue = c(-1),
                      options_params = 't',
                      show_rownames = F,
                      formatting_params = formatting_params)
    
  })
  
  # update selected category
  observeEvent(input$tab_summary_cell_clicked, { 
    
    if (!is.null(input$tab_summary_cell_clicked$row)  ) {
      
      row <- input$tab_summary_cell_clicked$row
      col <- input$tab_summary_cell_clicked$col
      
      if (row < nrow(summary_tab())){ # exlude the last row from considerations
        selected_category_id(summary_tab()[row, ]$category_id)
      }
      
    }
      
  })
  
  
  # Summary table with the breakdown
  output$header_category_details <- renderUI(  {
    if  ( !is.null(selected_category_id()) ){
      h3(paste0(' Details for category: ', datasets$trans_cat[category_id == selected_category_id()]$category_label)) 
    } else{
      h3('')
    }
  })
  
  output$tab_category_details <- DT::renderDataTable({
    if  ( !is.null(selected_category_id()) ){

      tmp_tab <- datasets$transactions_dt[category_id == selected_category_id()]%>%setorder(-'transaction_date')
      tmp_tab <- tmp_tab[, - c('account_balance')]
      cols_to_rename <- intersect(COLUMNS_NAMES, colnames(tmp_tab))
      columns_mapping <- COLUMNS_NAMES[which(sapply(COLUMNS_NAMES, function(x)  x %in% cols_to_rename )) ]
      
      brks <- c(0, 50, 100, 200, 500, 1000)
      color_pal <- contrast_palette_clrs(length(brks) + 1)
      formatting_params <- list()
      formatting_params[['target_cols']] = names(COLUMNS_NAMES[COLUMNS_NAMES == 'aggr_amount'])
      formatting_params[['target_dim']] = 'row'
      formatting_params[['style_type']] = 'interval'
      formatting_params[['value_range']] = brks
      formatting_params[['color_schema']] = color_pal
      
      GetFormattedTable(tmp_tab,
                        columns_mapping=columns_mapping,
                        paging = T,
                        filter_position = 'none',
                        searching = T,
                        scrollX = T,
                        scrollY = F,
                        lenght_menue = c(10, 20, -1),
                        options_params = 't',
                        show_rownames = F,
                        formatting_params = formatting_params)
    } else{ DT::datatable(data.table())}
    
  })
  
  # Plot account dynamics
  output$plot_account_dynamics <- renderPlot({
    PlotTimeDynamics(time_frequency_aggregates(),
                     target_col = input$selected_measure_type,
                     transaction_type = input$selected_trans_type2)
  })
 

}