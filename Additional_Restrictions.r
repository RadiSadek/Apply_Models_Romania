
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
  
  # Apply score 
  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>amounts[1],1,
    ifelse(scoring_df$score %in% c("Good 3") & scoring_df$amount>amounts[2],1,      
    ifelse(scoring_df$score %in% c("Good 2") & scoring_df$amount>amounts[3],1, 
    ifelse(scoring_df$score %in% c("Good 1") & scoring_df$amount>amounts[4],1,
    ifelse(scoring_df$score %in% c("Indeterminate") & 
          scoring_df$amount>amounts[5],1,scoring_df$color))))))
  is_online <- ifelse(!is.na(all_df$product_id) & all_df$product_id==14,1,0)
  
  # Remove Indterminate
  if(is_online==1){
    scoring_df$color <- 
       ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
       ifelse(scoring_df$score %in% c("Indeterminate","Good 1"),1,
              scoring_df$color))
  }
  return(scoring_df)
}

# Function to apply restrictions for repeat scorecard
gen_restrict_rep <- function(scoring_df,prev_amount,products,all_id,
                            all_df,db_name,application_id,crit){
  
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
                                               application_id,crit)
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
  if(crit==0){
    max_prev_amount <- max(all_id$amount[all_id$status %in% c(9:12,15,16) & 
                                           all_id$id!=application_id])
  } else {
    max_prev_amount <- max(all_id$amount[all_id$status %in% c(9:12,15,16)])
  }
  
  # Define allowed amounts according to whether the app is online or not 
  allowed_step_higher <- c(1500,2000)
  allowed_step_lower <- c(1000,1700)
  if(all_df$product_id==14){
    allowed_step <- allowed_step_lower
  } else {
    allowed_step <- allowed_step_higher
  }
  
  # Apply hard max step 
  scoring_df$color <- ifelse(scoring_df$score %in% c("Good 4") & 
      scoring_df$amount>(max_prev_amount+allowed_step[2]),1,
      ifelse(scoring_df$score %in% c("Good 1","Good 2","Good 3",
      "Indeterminate") & scoring_df$amount>(max_prev_amount+allowed_step[1]),1,
      scoring_df$color))
  
  return(scoring_df)
}

# Function to apply restrictions to refinances
gen_restrict_beh_refinance <- function(db_name,all_df,all_id,
    scoring_df,flag_active,application_id,flag_credirect,flag_cashpoint){
  
  return(scoring_df)
}


# Function to apply restrictions to repeat scorecard (for PO terminated credits)
gen_restrict_rep2 <- function(scoring_df,prev_amount,products,all_id,
                             all_df,db_name,application_id,crit){
  
  max_dpd <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_plan_main_select_query(db_name,application_id)),n=-1))$max_delay
  
  installments_paid <- nrow(suppressWarnings(fetch(dbSendQuery(con, 
     gen_passed_installments_query(db_name,application_id,
     substring(all_id$deactivated_at[all_id$id==application_id],1,10))),n=-1)))
  
  # Define allowed amounts according to whether 
  allowed_amounts_higher <- c(1000,1200,2000)
  allowed_amounts_lower <- c(1000,1000,1500)
  if(all_df$product_id==14){
    allowed_amounts <- allowed_amounts_lower
  } else {
    allowed_amounts <- allowed_amounts_higher
  }
  
  # Apply repeat conditions
  if(is.na(max_dpd) | is.na(installments_paid)){
    max_amount <- NA
    max_step <- 0
  } else {
    if(all_df$period==3){
      if(max_dpd>=91){
        max_amount <- allowed_amounts[1]
        max_step <- NA} 
      else if(max_dpd>=61) {
        max_amount <- max(0.8 * prev_amount$amount,allowed_amounts[2])
        max_step <- NA} 
      else {
        max_amount <- NA
        if(installments_paid==1){
          max_step <- 0
        } else {
          if(max_dpd<=14){
            max_step <- allowed_amounts[3]
          } else if(max_dpd<=30){
            max_step <- allowed_amounts[3]
          } else {
            max_step <- allowed_amounts[2]}}}
    } else {
      if(max_dpd>=91){
        max_amount <- allowed_amounts[1]
        max_step <- NA} 
      else if(max_dpd>=61) {
        max_amount <- max(0.8 * prev_amount$amount,allowed_amounts[2])
        max_step <- NA} 
      else {
        max_amount <- NA
        if(installments_paid<6){
          max_step <- 0
        } else {
          if(max_dpd<=14){
            max_step <- allowed_amounts[3]
          } else if(max_dpd<=30){
            max_step <- allowed_amounts[3]
          } else {
            max_step <- allowed_amounts[2]}}} 
      }
  }
  
  # Apply additional criteria to max step
  max_step <- ifelse(!(is.na(max_step)) & max_step>allowed_amounts[2] & 
    !(is.na(all_df$ccr_criteria_last_6m)) & 
    all_df$ccr_criteria_last_6m>0,allowed_amounts[2],max_step)
  
  # Apply criteria
  scoring_df$allowed_amount <- ifelse(is.na(max_step),max_amount,
    ifelse(!is.na(max_amount),max_amount,prev_amount$amount + max_step))
  scoring_df$color <- ifelse(scoring_df$score!="NULL" & 
        scoring_df$amount>scoring_df$allowed_amount,1,scoring_df$color)
  
  # Apply additional criteria to installments
  if(all_df$period==1){
    scoring_df$color <- ifelse(scoring_df$score!="NULL" & 
        scoring_df$period>45,1,scoring_df$color)
                        
  } else {
    scoring_df$color <- ifelse(scoring_df$score!="NULL" & 
        scoring_df$period>16,1,scoring_df$color)
  }

  return(scoring_df)
}

