
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



