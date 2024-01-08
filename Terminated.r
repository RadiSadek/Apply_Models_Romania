
gen_terminated_fct <- function(con,client_id,product_id,last_id,
                               flag_limit_offer){
  
  
#########################################
####### Define reading parameters #######
#########################################
  
# Defines the directory where custom .env file is located
load_dot_env(file = here('.env'))
  

# Defines the directory where the RScript is located
base_dir <- Sys.getenv("RSCRIPTS_PATH", unset = "", names = FALSE)  
  


#################################
####### Load source files #######
#################################
  
# Load other r files
source(file.path(base_dir,"Additional_Restrictions.r"))
source(file.path(base_dir,"Behavioral_Variables.r"))
source(file.path(base_dir,"CKR_variables.r"))
source(file.path(base_dir,"Cutoffs.r"))
source(file.path(base_dir,"Empty_Fields.r"))
source(file.path(base_dir,"Generate_Adjust_Score.r"))
source(file.path(base_dir,"Normal_Variables.r"))
source(file.path(base_dir,"Logistic_App_Flexcredit.r"))
source(file.path(base_dir,"Logistic_Beh_Flexcredit.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Useful_Functions.r"))

  

########################
####### Settings #######
########################
  
# Load predefined libraries
rdata <- file.path(base_dir, "rdata","flexcredit_beh_coeffs.rdata")
load(rdata)



####################################
### Read database and build data ###
####################################
  
# Get client id 
clients <- client_id


# Get last application_id of terminated or active credit		
application_id <- last_id


# Read credits applications
all_df <- suppressWarnings(fetch(dbSendQuery(con, 
              gen_big_sql_query(db_name,application_id)), n=-1))
curr_amount <- all_df$amount
  
  
# Apply some checks to main credit dataframe
if(!is.na(product_id)){
  all_df$product_id <- product_id
}
if(nrow(all_df)>1){
  all_df <- all_df[!duplicated(all_df$application_id),]
}
  
  
# Read product's periods and amounts
products  <- suppressWarnings(fetch(dbSendQuery(con, 
   gen_products_query(db_name,all_df)), n=-1))
products_desc <- suppressWarnings(fetch(dbSendQuery(con, 
   gen_products_query_desc(db_name,all_df)), n=-1))


# Recorrect amount to get highest possible amount	
all_df$amount <- max(products$amount)

  
# Get closets to product amount and installments 
all_df$installments <- products$installments[
   which.min(abs(products$installments - all_df$installments))]
all_df$amount <- products$amount[
   which.min(abs(products$amount - all_df$amount))]


# # Get period 
all_df <- merge(all_df,products_desc[,c("id","period")],
    by.x = "product_id",by.y = "id",all.x = TRUE)
  
  
# Check all credits of client
all_credits <- suppressWarnings(fetch(dbSendQuery(con, 
  gen_all_credits_query(db_name,all_df)), n=-1))
all_credits_raw <- all_credits
if(nrow(all_credits)>0){
  all_credits <- subset(all_credits,all_credits$id==application_id | 
    (!is.na(all_credits$activated_at) & 
    all_credits$activated_at<=all_df$created_at))
}
  
  
# Read all previous active or terminated credits of client
all_id <- subset(all_credits, 
  all_credits$id==application_id | all_credits$status %in% c(9:12,15)
) 
  
  
# Compute flag repeats
flag_beh <- ifelse(nrow(all_id)>0,1,0)
  
  
# Select for max delay if past AND active: must have at least 30 days of passed
all_id_max_delay <- all_id
all_actives_past <- subset(all_id,all_id$status==9)
if(nrow(all_actives_past)>0){
  all_id_max_delay <- gen_select_relevant_ids_max_delay(db_name,
        all_actives_past,all_id_max_delay)
}
  
  
# Get correct max days of delay (of relevant previous credits)
all_id_max_delay <- all_id_max_delay[!duplicated(all_id_max_delay$id),]
nrow_all_id_max_delay <- nrow(all_id_max_delay)
if (nrow_all_id_max_delay>=1){
  list_ids_max_delay <- gen_select_relevant_ids(all_id_max_delay,
      nrow_all_id_max_delay)
  data_plan_main_select <- suppressWarnings(fetch(dbSendQuery(con, 
      gen_plan_main_select_query(db_name,list_ids_max_delay)), n=-1))
} 
  
  
# Generate variables for payments of previous credits
nrow_all_id <- nrow(all_id)
all_id <- all_id[order(all_id$activated_at),]
if(nrow_all_id>=1){
  prev_paid_days <- gen_prev_paid_days(rbind(all_id,all_id[nrow(all_id),]))
  installments <- gen_last_total_amount(rbind(all_id,all_id[nrow(all_id),]))
  prev_amount <- gen_last_prev_amount(rbind(all_id,all_id[nrow(all_id),]))
}
  
  
# Compute and generate variables specific for behavioral model
data_plan_main_select_def <- ifelse(exists("data_plan_main_select"),
                                    data_plan_main_select,NA)
all_df <- gen_other_rep(nrow_all_id,all_id,all_df,
                        data_plan_main_select_def,application_id)
all_df$max_delay <- ifelse(!(is.na(data_plan_main_select_def)), 	
   data_plan_main_select_def[1],10)	
all_df$credits_cum <- nrow(all_id)	
all_df$days_diff_last_credit <- round(as.numeric(difftime(Sys.time(),	
    all_id$deactivated_at[nrow(all_id)],units = c("days"))))	

  
# Compute ratio of number of payments
all_df$ratio_nb_payments_prev <- ifelse(flag_beh==1,prev_paid_days/	
    installments$installments,NA)
  
  
# Get CCR(CKR) values
all_df <- gen_ckr_variables(db_name,all_df,flag_beh,application_id,
                            all_credits_raw)
  
  
  
############################################
### Compute and rework additional fields ###
############################################
  
# Compute flag if has current active
if(flag_beh==1){
  flag_active <- gen_flag_if_curr_active(all_id,application_id)
} else {
  flag_active <- cbind(NA,NA)
}
  
  
# Set period variable (monthly, twice weekly, weekly)
period <- products_desc$period
  
  
# Compute and generate general variables
all_df <- gen_norm_var(period,all_df,products,1)
  
  
# Compute income variables
all_df$total_income <- suppressWarnings(fetch(dbSendQuery(con, 
   gen_income_sql_query (db_name,all_df)), n=-1))$total_income
  
  
# Read relevant product amounts (not superior to amount of application)
products <- subset(products, products$amount<=all_df$amount)
  
  
# Prepare final dataframe
scoring_df <- gen_final_df(products,application_id,all_df)
  
  
# Make back-up dataframe
df <- all_df
  
  
# Correct empty and missing value fields (to standard format)
df <- gen_null_to_na(df)
  
  
# Get if empty field threshold is passed
empty_fields <- gen_empty_fields(df)
threshold_empty <- 4
  
  
# Readjust fields
df <- gen_norm_var2(df)
  
  
  
############################################################
### Apply model coefficients according to type of credit ###
############################################################
  
scoring_df <- gen_apply_score(
  empty_fields,threshold_empty,
  df,scoring_df,products,df_Log_Flexcredit_App,df_Log_Flexcredit_Beh,
  period,all_df,flag_beh)
  
  
# Build column PD
if(!("pd" %in% names(scoring_df))){
  scoring_df$pd <- NA
}
  
  
######################################
### Generate final output settings ###
######################################
  
# Generate scoring dataframe
scoring_df$created_at <- Sys.time()
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "pd","created_at")]
  
  
# Readjust scoring table by applying policy rules
scoring_df <- gen_apply_policy(scoring_df,flag_beh,all_df,db_name,
    application_id,prev_amount,all_id,products,1)


# Subset scoring dataframe according to criteria
correct_scoring_df <- subset(scoring_df,scoring_df$color!=1 &
      scoring_df$score %in% c("Indeterminate","Good 1",
                              "Good 2","Good 3","Good 4"))


# Get highest amount
get_max_amount <- suppressWarnings(max(correct_scoring_df$amount))


# Get maximum installment
scoring_df <- merge(scoring_df,products[,c("amount","installments",
   "installment_amount")],by.x = c("amount","period"),
   by.y = c("amount","installments"),all.x = TRUE)
if(is.infinite(get_max_amount)){
  get_max_installment <- -Inf	
} else {	
  get_max_installment <- max(scoring_df$installment_amount[
    scoring_df$color!=1 & scoring_df$amount==get_max_amount])	
}


# Get score of highest amount
if(get_max_amount>-Inf){
  sub <- subset(scoring_df,scoring_df$color!=1 &
                  scoring_df$installment_amount==get_max_installment & 
                  scoring_df$amount==get_max_amount)
  get_score <- 
    ifelse(nrow(subset(sub,sub$score=="Good 4"))>0,
      "Good 4",
    ifelse(nrow(subset(sub,sub$score=="Good 3"))>0,
      "Good 3",
    ifelse(nrow(subset(sub,sub$score=="Good 2"))>0,
      "Good 2",
    ifelse(nrow(subset(sub,sub$score=="Good 1"))>0,
     "Good 1",
    ifelse(nrow(subset(sub,sub$score=="Indeterminate"))>0,
    "Indeterminate",
     NA)))))
} else {
  get_score <- NA
}


# Make final list and return result
final_list <- list(get_max_amount,get_max_installment,get_score,
                   all_df$max_delay)
return(final_list)

}
