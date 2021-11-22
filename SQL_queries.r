
##################################
######## Define SQL queries ######
##################################

# Define big query which reads from credits_applications
gen_big_sql_query <- function(db_name,application_id){
big_sql_query <- paste("SELECT 
",db_name,".clients.ownership, 
",db_name,".clients.on_address,
",db_name,".clients.education,
",db_name,".personal_data.cnp,
",db_name,".client_employer.work_experience,
",db_name,".loans.id,
",db_name,".loans.amount,
",db_name,".loans.master_client_id,
",db_name,".loans.amount,
",db_name,".loans.installments,
",db_name,".loans.created_at,
",db_name,".loans.product_id
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


