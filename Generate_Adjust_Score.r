
#######################################################
######## Apply logistic models and policy rule ########
#######################################################

# Function to apply scoring model 
gen_apply_score <- function(empty_fields,threshold_empty,
                            df,scoring_df,products,df_Log_Flexcredit_App,
                            df_Log_Flexcredit_Beh,period,all_df,flag_beh){
  
  # Apply model coefficients according to type of credit 
  if (empty_fields>=threshold_empty){
    
    scoring_df$score <- "NULL"
    scoring_df$color <- 2
    
  } else if(flag_beh==1){
    scoring_df <- gen_beh_flex(df,scoring_df,products,df_Log_Flexcredit_Beh,
                               period,all_df)
  } else {
    scoring_df <- gen_app_flex(df,scoring_df,products,df_Log_Flexcredit_App,
                               period,all_df)
  }
  return(scoring_df)
  
}

# Function to apply policy rules
gen_apply_policy <- function(scoring_df,flag_beh,all_df,db_name,application_id,
                             prev_amount,all_id,products,crit){
  
  if(flag_beh==1 & crit==0){
    scoring_df <- gen_restrict_rep(scoring_df,prev_amount,products,all_id,
                                   all_df,db_name,application_id,crit)
  } else if(flag_beh==1 & crit==1){
    scoring_df <- gen_restrict_rep2(scoring_df,prev_amount,products,all_id,
                                   all_df,db_name,application_id,crit)
  } else {
    scoring_df <- gen_restrict_app(scoring_df,all_df)
  }
}

