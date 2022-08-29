########################
# Produce area plot
########################
PlotTimeDynamics <- function(dt_aggr, 
                                target_col = 'aggr_amount',
                                transaction_type = 'income'){
  
  if (transaction_type == INCOME_ITEM_LABEL){
    
    y_label = "Revenues, in CHF"
    legend_name = "Revenue categories:"
  } else {
    y_label = "Expenses, in CHF"
    legend_name = "Expense categories:"
  }
  
  empty_category <- copy(dt_aggr)[, total:= sum(aggr_amount, na.rm = T), by = .(category_id)]%>%
    .[,.(category_id, total)]%>%unique()%>%.[total == 0]

  
  plt <- ggplot(data = dt_aggr[!category_id %in% empty_category$category_id] , 
                aes( x = time_label,
                     y =  get(target_col), 
                     fill = factor(category_id) ) ) + 
    geom_area() +  labs(  x = paste("Timeline"),   
                          y = y_label, 
                          title =  "") + 
    scale_fill_hue(name = legend_name) + theme(legend.position="right")
  
  return(plt)
}


# GetBarCHarts <- function(transactions_dt, selected_frequency = 'weekly'){
#   expenses <- GetTimeAggregatedData(transactions_dt,
#                                     selected_user = 'all',
#                                     frequency = selected_frequency,
#                                     selected_categories=c(),
#                                     transaction_type = EXPENSE_ITEM_LABEL)%>%
#     .[, transaction_type:=EXPENSE_ITEM_LABEL]
#   
#   revenues <- GetTimeAggregatedData(transactions_dt,
#                                     selected_user = 'all',
#                                     frequency = selected_frequency,
#                                     selected_categories=c(),
#                                     transaction_type = INCOME_ITEM_LABEL)%>%
#     .[, transaction_type:=INCOME_ITEM_LABEL]
#   
#   dt <- rbind(expenses, revenues)
#   
#   dev.off()
#   # https://statisticsglobe.com/draw-stacked-bars-within-grouped-barplot-r
#   empty_category <- copy(dt)[, total:= sum(aggr_amount, na.rm = T), by = .(category_id)]%>%
#     .[,.(category_id, total)]%>%unique()%>%.[total == 0]
#   ggplot(dt[!category_id %in% empty_category$category_id],                         # Draw barplot with grouping & stacking
#          aes(x = transaction_type,
#              y = aggr_amount ,
#              fill = category_id)) + 
#     geom_bar(stat = "identity",
#              position = "stack") +
#     facet_grid(~ time_label)
#   
# }
# 


# 
# Experiments with Pie Charts
# financial_breakdown <- GetFinancialStructure(
#                 date_be = '2022-06-01',
#                 date_end = '2022-08-20',
#                 selected_user='all',
#                 transaction_type = 'expense',
#                 transactions_dt)
# 
# pie_chart <- financial_breakdown %>% 
#              highcharter::hchart(type = 'pie', hcaes(category_id, percent)) %>% 
#              hc_title(text = "Expenses structure",
#                      align =  'center',
#                      style = list(fontWeight = 'bold', fontSize = '16px')) %>% 
#              hc_tooltip(enabled = T) %>% 
#              hc_subtitle(text = 'In %',
#                         align = 'center',
#                         style = list(fontWeight = 'bold')) %>% 
#              hc_add_theme(hc_theme_ffx()) %>% 
#              hc_credits(enabled = T, text = '')%>% hc_tooltip(enabled = T)

 
