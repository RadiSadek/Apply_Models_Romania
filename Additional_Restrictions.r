
#############################################################
######## Apply restrictions on final scoring dataframe ######
#############################################################

# Function to apply restrictions for application scorecard
gen_restrict_app <- function(scoring_df,all_df){
  
  low_amounts <- c(2000,1500,1200,1000,800)
  high_amounts <- c(2500,2000,1500,1200,1000)
  
  if(is.na(all_df$ccr_max_delay)){
    amounts <- high_amounts
  } else if(all_df$ccr_max_delay<=60){
    amounts <- high_amounts
  } else {
    amounts <- low_amounts
  }
  
  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>amounts[1],1,
    ifelse(scoring_df$score %in% c("Good 3") & scoring_df$amount>amounts[2],1,      
    ifelse(scoring_df$score %in% c("Good 2") & scoring_df$amount>amounts[3],1, 
    ifelse(scoring_df$score %in% c("Good 1") & scoring_df$amount>amounts[4],1,
    ifelse(scoring_df$score %in% c("Indeterminate") & 
          scoring_df$amount>amounts[5],1,scoring_df$color))))))

  return(scoring_df)
}

# Function to apply restrictions for repeat scorecard
gen_restrict_rep <- function(scoring_df,prev_amount,products,all_id,
                            all_df,db_name,application_id){
  
  # Check if has Good 1 at least somewhere in table
  criteria <- length(names(table(scoring_df$score))
    [names(table(scoring_df$score)) %in% 
          c("Good 1","Good 2","Good 3","Good 4")])
  scoring_df$color <- ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(criteria==0 & scoring_df$amount>prev_amount$amount,1,
           scoring_df$color))
  
  # Check if installment ratio is OK
  if(!("installment_amount" %in% names(scoring_df))){
    scoring_df <- merge(scoring_df,products[,c("amount","installments",
       "installment_amount")],
       by.x = c("amount","period"),by.y = c("amount","installments"),
       all.x = TRUE)
  }
  allowed_installment <- gen_installment_ratio(db_name,all_id,all_df,
                                               application_id)
  for(i in 1:nrow(scoring_df)){
    if(scoring_df$installment_amount[i]>allowed_installment){
      scoring_df$color[i] <- 1
    }
  }
  
  # Limit based on maximum amount differential
  for(i in 1:nrow(all_id)){
    all_id$amount[i] <- suppressWarnings(fetch(dbSendQuery(con,
     gen_big_sql_query(db_name,all_id$id[i])), n=-1))$amount
  }
  max_prev_amount <- max(all_id$amount[all_id$status %in% c(9:12,15,16) & 
                                       all_id$id!=application_id])
  scoring_df$color <- ifelse(scoring_df$score %in% c("Good 4") & 
      scoring_df$amount>(max_prev_amount+1500),1,
      ifelse(scoring_df$score %in% c("Good 1","Good 2","Good 3",
      "Indeterminate") & scoring_df$amount>(max_prev_amount+1000),1,
      scoring_df$color))
  
  return(scoring_df)
}

# Function to apply restrictions to refinances
gen_restrict_beh_refinance <- function(db_name,all_df,all_id,
    scoring_df,flag_active,application_id,flag_credirect,flag_cashpoint){
  
  return(scoring_df)
}

