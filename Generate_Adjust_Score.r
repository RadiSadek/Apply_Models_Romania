
#######################################################
######## Apply logistic models and policy rule ########
#######################################################

# Function to apply scoring model 
gen_apply_score <- function(empty_fields,threshold_empty,
                            df,scoring_df,products,df_Log_Flexcredit_App,
                            period,all_df){
  
  # Apply model coefficients according to type of credit 
  if (empty_fields>=threshold_empty){
    
    scoring_df$score <- "NULL"
    scoring_df$color <- 2
    
  } else {
    scoring_df <- gen_app_flex(df,scoring_df,products,df_Log_Flexcredit_App,
                               period,all_df)
  }
  return(scoring_df)
  
}


