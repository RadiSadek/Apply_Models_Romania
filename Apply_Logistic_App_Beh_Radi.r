

################################################################################
#               Joint script for Application and Behavioral scoring            #
#      Apply Logistic Regression on all products (Flexcredit Romania)          #
#                          Version 1.0 (2021/10/06)                            #
################################################################################



########################
### Initial settings ###
########################

# Library
suppressMessages(suppressWarnings(library(RMySQL)))
suppressMessages(suppressWarnings(library(here)))
suppressMessages(suppressWarnings(library(dotenv)))
suppressMessages(suppressWarnings(require("reshape")))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))


# Database
db_user <- "root"
db_password <- "123456"
db_name <- "romania"
db_host <- "127.0.0.1"
df_port <- 3306
con <- dbConnect(MySQL(), user=db_user, password=db_password, 
                 dbname=db_name, host=db_host, port = df_port)


# Define work directory
main_dir <- "C:\\Projects\\Flexcredit_Romania\\Apply_Scoring\\"


# Read argument of ID
args <- commandArgs(trailingOnly = TRUE)
application_id <- args[1]
#application_id <- 39568
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(main_dir)


# Load other r files
source(paste(main_dir,"Apply_Models_Romania\\Additional_Restrictions.r", 
  sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Behavioral_Variables.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\CKR_variables.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Cutoffs.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Empty_Fields.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Generate_Adjust_Score.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Normal_Variables.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Logistic_App_Flexcredit.r", 
  sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Logistic_Beh_Flexcredit.r", 
  sep=""))
source(paste(main_dir,"Apply_Models_Romania\\SQL_queries.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Useful_Functions.r", sep=""))


# Load predefined libraries
load("rdata\\flexcredit_app.rdata")
load("rdata\\flexcredit_beh.rdata")



####################################
### Read database and build data ###
####################################

# Read credits applications
all_df <- suppressWarnings(fetch(dbSendQuery(con, 
              gen_big_sql_query(db_name,application_id)), n=-1))


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


# Get closets to product amount and installments 
all_df$installments <- products$installments[
   which.min(abs(products$installments - all_df$installments))]
all_df$amount <- products$amount[
   which.min(abs(products$amount - all_df$amount))]


# Check all credits of client
all_credits <- suppressWarnings(fetch(dbSendQuery(con, 
  gen_all_credits_query(db_name,all_df)), n=-1))
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
flag_beh <- ifelse(nrow(all_id[all_id$id!=application_id,])>0,1,0)


# Select for max delay if past AND active: must have at least 30 days of passed
all_id_max_delay <- all_id[all_id$id != application_id,]
all_actives_past <- subset(all_id, 
        all_id$id!=application_id & all_id$status==9)
if(nrow(all_actives_past)>0){
  all_id_max_delay <- gen_select_relevant_ids_max_delay(db_name,
        all_actives_past,all_id_max_delay)
}


# Get correct max days of delay (of relevant previous credits)
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
if(nrow_all_id>1){
  prev_paid_days <- gen_prev_paid_days(all_id)
  installments <- gen_last_total_amount(all_id)
  prev_amount <- gen_last_prev_amount(all_id)
}


# Compute and generate variables specific for behavioral model
data_plan_main_select_def <- ifelse(exists("data_plan_main_select"),
                                    data_plan_main_select,NA)
all_df <- gen_other_rep(nrow_all_id,all_id,all_df,
                        data_plan_main_select_def,application_id)


# Compute ratio of number of payments
all_df$ratio_nb_payments_prev <- ifelse(flag_beh==1,prev_paid_days/	
    installments$installments,NA)


# Get CCR(CKR) values
all_df <- gen_ckr_variables(db_name,all_df,flag_beh,application_id)



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
    application_id,prev_amount,all_id,products)


# Create column for table display
scoring_df <- gen_final_table_display(scoring_df)


# Create output dataframe
final <- as.data.frame(cbind(scoring_df$loan_id[1],
 scoring_df$score[scoring_df$amount== unique(scoring_df$amount)
   [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
                    & 
 scoring_df$installments==unique(scoring_df$installments)
   [which.min(abs(all_df$installments - unique(scoring_df$installments)))]],
 scoring_df$display_score[scoring_df$amount== unique(scoring_df$amount)
   [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
                  & 
 scoring_df$installments==unique(scoring_df$installments)
 [which.min(abs(all_df$installments - unique(scoring_df$installments)))]]))
names(final) <- c("id","score","display_score")
final$pd <- scoring_df$pd[scoring_df$amount== unique(scoring_df$amount)
    [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
                          & 
             scoring_df$installments==unique(scoring_df$installments)
    [which.min(abs(all_df$installments - unique(scoring_df$installments)))]]
final$flag_beh <- flag_beh
final$ownership <- all_df$ownership
final$on_address <- all_df$on_address
final$education <- all_df$education
final$experience <- all_df$work_experience
final$total_income <- all_df$total_income
final$gender <- all_df$gender
final$age <- all_df$age
final$household_total <- all_df$household_total
final$ratio_nb_payments_prev <- round(all_df$ratio_nb_payments_prev,3)
final$max_delay <- all_df$max_delay
final$credits_cum <- all_df$credits_cum
final$days_diff_last_credit <- all_df$days_diff_last_credit
final$ccr_max_delay <- all_df$ccr_max_delay
final$ccr_monthly_payed <- all_df$ccr_monthly_payed
if(flag_beh==1){
  final$prev_amount <- prev_amount$amount
} else {
  final$prev_amount <- NA
}
final$amount <- all_df$amount
final$highest_amount <- suppressWarnings(
  max(scoring_df$amount[scoring_df$display_score %in% c("Yes")]))
final$office_id <- all_df$office_id

# Read and write
final_exists <- read.xlsx(paste(main_dir,
  "\\Monitoring\\Files\\Scored_Credits.xlsx", sep=""))
final <- rbind(final_exists, final)
write.xlsx(final, paste(main_dir,"\\Monitoring\\Files\\Scored_Credits.xlsx", 
  sep=""))



#######
# END #
#######

