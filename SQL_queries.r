
##################################
######## Define SQL queries ######
##################################

# Define big query which reads from credits_applications
gen_big_sql_query <- function(db_name,application_id){
big_sql_query <- paste("SELECT 
",db_name,".clients.ownership, 
",db_name,".clients.on_address,
",db_name,".clients.household_total,
",db_name,".clients.education,
",db_name,".personal_data.cnp,
",db_name,".client_employer.work_experience,
",db_name,".loans.id,
",db_name,".loans.amount,
",db_name,".loans.master_client_id,
",db_name,".loans.installments,
",db_name,".loans.created_at,
",db_name,".loans.product_id,
",db_name,".loans.office_id
FROM ",db_name,".loans
LEFT JOIN ",db_name,".clients
ON ",db_name,".loans.master_client_id = ",db_name,
".clients.id
LEFT JOIN ",db_name,".client_employer
ON ",db_name,".loans.master_client_id = ",db_name,
".client_employer.client_id
LEFT JOIN ",db_name,".personal_data
ON ",db_name,".loans.master_client_id = ",db_name,
".personal_data.personable_id
WHERE loans.id =", application_id, 
" AND personal_data.personable_type = 'App\\\\Models\\\\Clients\\\\Client'",
sep="")
return(big_sql_query)
}

# Define query for products periods and amounts
gen_products_query <- function(db_name,all_df){
  return(paste("SELECT * FROM ", db_name, ".product_amounts_and_installments
               WHERE product_id IN (",
               all_df$product_id, ")", sep=""))
}

# Define query for products 
gen_products_query_desc <- function(db_name,all_df){
  return(paste("SELECT id, period FROM ", db_name, ".products 
               WHERE id=", all_df$product_id, sep=""))
}

# Define query for income
gen_income_sql_query <- function(db_name,all_df){
  return(paste("SELECT SUM(AMOUNT) AS total_income
  FROM ",db_name,".client_incomes
  WHERE incomeable_type='App\\\\Models\\\\Clients\\\\Client'
  AND incomeable_id=",all_df$master_client_id, sep=""))
}

# Define query for getting all credits for client 
gen_all_credits_query <- function(db_name,all_df){
  return(paste("SELECT id, master_client_id, status, created_at, 
  activated_at, amount, finished_at AS deactivated_at, 
  installments, product_id FROM ",db_name,".loans 
  WHERE master_client_id=",all_df$master_client_id, sep =""))
}

# Define query to get the pay days of previous actives credits
gen_plan_main_actives_past_query <- function(db_name,all_actives_past){
  return(paste("SELECT loan_id, pay_day
  FROM ",db_name,".loan_repayment_schedule WHERE loan_id in(", 
               all_actives_past$id," )", sep=""))
}

# Define query to get the maximum of delay days from previous credits
gen_plan_main_select_query <- function(db_name,list_ids_max_delay){
  return(paste("SELECT MAX(days_delay) AS max_delay FROM ",db_name, 
  ".loan_repayment_schedule WHERE loan_id in(",list_ids_max_delay," 
  )", sep=""))
}

# Define query to get all payments of previous credit 
gen_all_payments_query <- function(var,db){		
  return(paste("SELECT transactionable_id, amount, created_at
  FROM ",db,".transactions WHERE type = 1 AND 
  transactionable_type = 'App\\\\Models\\\\Loans\\\\Loan' AND 
  transactionable_id = ",var,sep =""))		
}

# Define query to get the last amount of previous credit
gen_last_cred_installments_query <- function(var,db){
  return(paste("SELECT installments
  FROM ",db,".loans 
  WHERE id=", var, sep =""))
}

# Get CCR data
gen_ccr_data <- function(var,db){
  return(paste("SELECT loan_id, created_at, content
  FROM ",db,".client_ccr_reports
  WHERE loan_id IN ", var, sep =""))
}

# Define query to get total amount of current application amount
gen_prev_amount_query <- function(db_name,all_id){
  return(paste("SELECT amount FROM ",db_name,
    ".loans WHERE id=", 
    all_id$id[nrow(all_id)-1], sep=""))
}

# Define query to get max installment amount per application id
gen_max_pmt_main_query <- function(db_name,id){
  return(paste(
  "SELECT penalty + interest + principal AS installment_amount 
  FROM ",db_name,".loan_repayment_schedule
  WHERE deleted_at IS NULL AND loan_id=",id,sep=""))
}

# Define number of passed installments 
gen_passed_installments_query <- function(db_name,id,deactivated){
  return(paste(
    "SELECT pay_day, pay_day 
  FROM ",db_name,".loan_repayment_schedule
  WHERE loan_id=",id," AND pay_day <= '",deactivated,"'",sep=""))
}
