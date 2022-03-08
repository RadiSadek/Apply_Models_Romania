
####################################################################
######## Functions to compute variables for repeat customers  ######
####################################################################


# Function to compute variables for repeat customers
gen_other_rep <- function(nrow_all_id,all_id,all_df,
                          data_plan_main_select_def,application_id){
  
  if (nrow_all_id>1){
    all_id <- all_id[order(all_id$created_at),]
    all_id$credits_cum[1] <- 0
    all_id$days_diff_last_credit <- NA
    all_df$max_delay <- ifelse(!(is.na(data_plan_main_select_def)), 
        data_plan_main_select_def[1], 40)
    for (i in 2:nrow(all_id)){
      all_id$credits_cum[i] <- 1 + all_id$credits_cum[i-1]
      all_id$days_diff_last_credit[i] <- difftime(all_id$created_at[i], 
        all_id$deactivated_at[i-1], units=c("days"))
    }
    all_id$days_diff_last_credit <- ifelse(all_id$days_diff_last_credit<0, NA, 
        all_id$days_diff_last_credit)
    all_id$days_diff_last_credit <- round(all_id$days_diff_last_credit, 0)
    all_id <- subset(all_id, all_id$id==application_id)
    all_id <- all_id[,c("credits_cum","days_diff_last_credit")]
    all_df <- cbind(all_df, all_id)
  } else {
    all_df$credits_cum <- 0
    all_df$days_diff_last_credit <- NA
    all_df$max_delay <- NA
  }
  return(all_df)
}


# Function to get number of unique payment days of previous credit
gen_prev_paid_days <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  result <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_all_payments_query(var$id[nrow(var)-1],db_name)), n=-1))
  return(length(unique(result$created_at)))
}

# Function to select id of all previous credits
gen_select_relevant_ids <- function(all_id_max_delay,nrow_all_id_max_delay){
  var <- unique(all_id_max_delay$id)[1]
  if(nrow_all_id_max_delay>1){
    for(i in 2:length(unique(all_id_max_delay$id))){
      var <- paste(var,unique(all_id_max_delay$id)[i], sep=",")}
  }
  return(var)
}

# Function to select ids only of relevant credits for max delay
gen_select_relevant_ids_max_delay <- function(db_name,all_actives_past,
                                              all_id_max_delay){
  
  data_plan_main_actives_past <- suppressWarnings(fetch(dbSendQuery(con, 
      gen_plan_main_actives_past_query(db_name,all_actives_past)), n=-1))
  data_plan_main_actives_past$date_diff <- difftime(Sys.time(), 
      data_plan_main_actives_past$pay_day, units=c("days"))
  agg_passed_installments <- as.data.frame(aggregate(
    data_plan_main_actives_past$date_diff, 
    by=list(data_plan_main_actives_past$loan_id), FUN=max))
  agg_passed_installments <- subset(agg_passed_installments, 
    agg_passed_installments$x<30)
  all_id_max_delay <- all_id_max_delay[!(all_id_max_delay$id %in% 
    agg_passed_installments$Group.1),]
  
  return(all_id_max_delay)
}

# Function to order by credits for repeat customers
gen_variables_for_rep <- function(all_id){
  list_ids <- unique(all_id$id)[1]
  for(i in 2:length(unique(all_id$id))){
    list_ids <- paste(list_ids, unique(all_id$id)[i], sep=",")
  }
  all_id$date <- ifelse(is.na(all_id$activated_at), 
                        all_id$created_at, all_id$activated_at)
  all_id <- all_id[ , -which(names(all_id) %in% c("signed_at","created_at"))]
  all_id <- all_id[order(all_id$date),]
  
  return(all_id)
  
}

# Function to get total amount (with taxes) of previous credit
gen_last_total_amount <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(total_amount <- suppressWarnings(fetch(dbSendQuery(con, 
    gen_last_cred_installments_query(var$id[nrow(var)-1],db_name)), n=-1)))
}

# Get maximum previous installment amount
gen_prev_max_installment <- function(db_name,input,all_df,application_id){
  
  input <- input[order(input$activated_at),]
  input <- input[input$id!=application_id,]
  all_df$period <- suppressWarnings(fetch(dbSendQuery
    (con,gen_products_query_desc(db_name,all_df[1,])), n=-1))$period
  
  for(i in 1:nrow(input)){
    
    input$installment_amount[i] <- mean(suppressWarnings(fetch(dbSendQuery(con,
      gen_max_pmt_main_query(db_name,input$id[i])),n=-1))$installment_amount)
    
    input$period[i] <- suppressWarnings(fetch(dbSendQuery
      (con,gen_products_query_desc(db_name,input[i,])), n=-1))$period
    
    if(input$period[i]!=all_df$period){
      input$installment_amount[i] <- gen_correct_max_installment_po(
        input$period[i],all_df$period,input$installment_amount[i])
    }
  }
  prev_installment_amount <- max(input$installment_amount)
  return(prev_installment_amount)
}


# Function to get amount of previous credit
gen_last_prev_amount <- function(all_id){
  var <- gen_variables_for_rep(all_id)
  return(prev_amount <- suppressWarnings(fetch(dbSendQuery(con, 
     gen_prev_amount_query(db_name,var)), n=-1)))
}

# Function to compute installment ratio 
gen_installment_ratio <- function(db_name,all_id,all_df,application_id){
  
  # Join DPD of past credits
  all_id_here <- all_id[all_id$status %in% c(9:12,15),]
  all_id_here <- all_id_here[all_id_here$id!=application_id,]
  for (i in 1:nrow(all_id_here)){
    all_id_here$max_delay[i] <- suppressWarnings(fetch(dbSendQuery(
      con,gen_plan_main_select_query(db_name,all_id_here$id[i])), 
      n=-1))$max_delay
  }
  
  # Subset into active and terminated
  all_id_local <- subset(all_id_here,all_id_here$status %in% c(10,11,12))
  all_id_local2 <- subset(all_id_here,all_id_here$status %in% c(9))
  all_id_local_activ_not_ok <- subset(all_id_local2,
          all_id_local2$max_delay>60)
  
  if(nrow(all_id_local)>0 | nrow(all_id_local2)>0){
    
    # Get DPD of terminated credits
    all_id_local_tot <- all_id_local
    all_id_local_ok <- subset(all_id_local_tot,
                              all_id_local_tot$max_delay<=60)
    all_id_local_not_ok <- subset(all_id_local_tot,
                                  all_id_local_tot$max_delay>60)
    
    # Compute optimized previous installment amount
    final_prev_installment_amount <-
      ifelse(nrow(all_id_local_activ_not_ok)>0,0.6*
             gen_prev_max_installment(db_name,all_id_local2,all_df,
                                      application_id),
      ifelse(nrow(all_id_local_ok)>0 & nrow(all_id_local_not_ok)==0,
             1.3*gen_prev_max_installment(db_name,rbind(
             all_id_local_ok,all_id_local2),all_df,application_id),
      ifelse(nrow(all_id_local_ok)>0 & nrow(all_id_local_not_ok)>0,
             1.1*gen_prev_max_installment(db_name,rbind(
             all_id_local_ok,all_id_local2),all_df,application_id),
      ifelse(nrow(all_id_local2)>0,
             1.1*gen_prev_max_installment(db_name,all_id_local2,all_df,
                                          application_id),
             1*gen_prev_max_installment(db_name,all_id_local_not_ok,all_df,
                                        application_id)))))                          
    
  } else {
    final_prev_installment_amount <- Inf
  }
  return(final_prev_installment_amount)
}

# Compute flag if has active or hidden active
gen_flag_if_curr_active <- function(all_id,application_id){
  
  all_id_here <- all_id[all_id$id!=application_id,]
  all_id_local_active <- all_id_here[all_id_here$status %in% c(9),]
  all_id_local_term <- all_id_here[all_id_here$status %in% c(10:12),]
  if(nrow(all_id_local_term)>0){
    all_id_local_term <- all_id_local_term[rev(order(
      all_id_local_term$deactivated_at)),]
    all_id_local_term$time_to_now <- round(difftime(
      as.Date(substring(Sys.time(),1,10)),
      as.Date(substring(all_id_local_term$deactivated_at,1,10)),
      units=c("days")),
      2)
  }
  
  return(cbind(ifelse(nrow(all_id_local_active)>0,1,0),
    ifelse(nrow(all_id_local_term[all_id_local_term$time_to_now<1,])>0,1,0)))
}



