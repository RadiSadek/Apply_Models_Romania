

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
suppressMessages(suppressWarnings(require(reshape)))
suppressMessages(suppressWarnings(library(openxlsx)))
suppressMessages(suppressWarnings(require(jsonlite)))


# Defines the directory where custom .env file is located
load_dot_env(file = here('.env'))


#########################
### Command arguments ###
#########################

args <- commandArgs(trailingOnly = TRUE)
application_id <- args[1]
product_id <- args[2]


#######################
### Manual settings ###
#######################

# Defines the directory where the RScript is located
base_dir <- Sys.getenv("SCORING_PATH", unset = "", names = FALSE)



#####################
####### MySQL #######
#####################

db_host <- Sys.getenv("DB_HOST", 
                      unset = "localhost", 
                      names = FALSE)
db_port <- strtoi(Sys.getenv("DB_PORT", 
                             unset = "3306", 
                             names = FALSE))
db_name <- Sys.getenv("DB_DATABASE", 
                      unset = "citycash", 
                      names = FALSE)
db_username <- Sys.getenv("DB_USERNAME", 
                          unset = "root", 
                          names = FALSE)
db_password <- Sys.getenv("DB_PASSWORD", 
                          unset = "secret", 
                          names = FALSE)
con <- dbConnect(MySQL(), user=db_username, 
                 password=db_password, dbname=db_name, 
                 host=db_host, port = db_port)
sqlMode <- paste("SET sql_mode=''", sep ="")
suppressWarnings(fetch(dbSendQuery(con, sqlMode), 
                       n=-1))


#################################
####### Load source files #######
#################################

# Load other r files
source(file.path(base_dir,"Behavioral_Variables.r"))
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
rdata <- file.path(base_dir, "rdata","flexcredit_app.rdata")
rdata <- file.path(base_dir, "rdata","flexcredit_beh.rdata")
load(rdata)



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
}


# Compute and generate variables specific for behavioral model
data_plan_main_select_def <- ifelse(exists("data_plan_main_select"),
                                    data_plan_main_select,NA)
all_df <- gen_other_rep(nrow_all_id,all_id,all_df,
                        data_plan_main_select_def,application_id)


# Compute ratio of number of payments
all_df$ratio_nb_payments_prev <- ifelse(flag_beh==1,prev_paid_days/	
    installments$installments,NA)



############################################
### Compute and rework additional fields ###
############################################

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


# Create column for table display
scoring_df <- gen_final_table_display(scoring_df)


# Update table credits applications scoring
write_sql_query <- paste("
  DELETE FROM ",db_name,".loan_scoring WHERE loan_id=",
  application_id, sep="")
suppressMessages(dbSendQuery(con,write_sql_query))
suppressMessages(dbWriteTable(con, name = "loan_scoring", 
  value = scoring_df,
  field.types = c(loan_id="numeric", amount="integer", 
  installments="integer", score="character(20)",color="integer", 
  display_score="character(20)",pd="numeric",created_at="datetime"),
  row.names = F, append = T))



#######
# END #
#######

