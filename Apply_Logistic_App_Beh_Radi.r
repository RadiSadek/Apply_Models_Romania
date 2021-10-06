

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
#application_id <- args[1]
application_id <- 1
product_id <- NA


# Set working directory for input (R data for logistic regression) and output #
setwd(main_dir)


# Load other r files
source(paste(main_dir,"Apply_Models_Romania\\Normal_Variables.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Logistic_App_Flexcredit.r", 
  sep=""))
source(paste(main_dir,"Apply_Models_Romania\\SQL_queries.r", sep=""))
source(paste(main_dir,"Apply_Models_Romania\\Useful_Functions.r", sep=""))


# Load predefined libraries
load("rdata\\flexcredit_app.rdata")




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
scoring_df <- gen_final_df(products,application_id)


# Make back-up dataframe
df <- all_df


# Correct empty and missing value fields (to standard format)
df <- gen_null_to_na(df)


# Get if empty field threshold is passed
empty_fields <- gen_empty_fields(flag_beh,flag_credirect,df)
threshold_empty <- 4


# Readjust fields
df <- gen_norm_var2(df)


# Compute flag if has current active
if(flag_beh_company==1){
  flag_active <- gen_flag_if_curr_active(all_id,application_id)
} else {
  flag_active <- cbind(NA,NA)
}



############################################################
### Apply model coefficients according to type of credit ###
############################################################

scoring_df <- gen_apply_score(
  empty_fields,threshold_empty,flag_exclusion,
  flag_varnat,flag_is_dead,flag_credit_next_salary,flag_credirect,
  flag_beh,all_df,scoring_df,df,products,df_Log_beh_CityCash,
  df_Log_CityCash_App,df_Log_beh_Credirect,df_Log_Credirect_App_installments,
  df_Log_Credirect_App_payday,period,all_id,prev_amount,amount_tab,
  t_income,disposable_income_adj,flag_new_credirect_old_city,api_df,0)


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


# Readjust score when applicable
scoring_df <- gen_apply_policy(scoring_df,flag_credirect,flag_cession,
   flag_bad_ckr_citycash,all_df,all_id,flag_beh,prev_amount,products,
   application_id,flag_new_credirect_old_city,flag_credit_next_salary,
   flag_beh_company,flag_cashpoint)


# Reselect columns 
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "pd","created_at")]


# Reselect columns 
scoring_df <- scoring_df[,c("application_id","amount","period","score","color",
                            "pd","created_at")]


# Create column for table display
scoring_df <- gen_final_table_display(scoring_df)


# Create output dataframe
final <- as.data.frame(cbind(scoring_df$application_id[1],
 scoring_df$score[scoring_df$amount== unique(scoring_df$amount)
   [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
                    & 
 scoring_df$period==unique(scoring_df$period)
   [which.min(abs(all_df$installments - unique(scoring_df$period)))]],
 scoring_df$display_score[scoring_df$amount== unique(scoring_df$amount)
   [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
                  & 
 scoring_df$period==unique(scoring_df$period)
 [which.min(abs(all_df$installments - unique(scoring_df$period)))]]))
names(final) <- c("id","score","display_score")
final$highest_score <- 
  ifelse(length(names(table(scoring_df$score))
      [names(table(scoring_df$score)) %in% c("Good 4")])!=0,"Good 4",
  ifelse(length(names(table(scoring_df$score))
              [names(table(scoring_df$score)) %in% c("Good 3")])!=0,"Good 3",
  ifelse(length(names(table(scoring_df$score))
              [names(table(scoring_df$score)) %in% c("Good 2")])!=0,"Good 2",
  ifelse(length(names(table(scoring_df$score))
              [names(table(scoring_df$score)) %in% c("Good 1")])!=0,"Good 1",
  ifelse(length(names(table(scoring_df$score))
              [names(table(scoring_df$score)) %in% c("Indeterminate")])!=0,
         "Indeterminate",
  ifelse(length(names(table(scoring_df$score))
        [names(table(scoring_df$score)) %in% c("Bad")])!=0,"Bad","NULL"))))))

final$PD <- scoring_df$pd[scoring_df$amount== unique(scoring_df$amount)
  [which.min(abs(all_df$amount - unique(scoring_df$amount)))]
      & 
  scoring_df$period==unique(scoring_df$period)
  [which.min(abs(all_df$installments - unique(scoring_df$period)))]]
final$highest_amount <- max(scoring_df$amount)
final$flag_beh <- flag_beh
final$flag_beh_company <- flag_beh_company
final$flag_credirect <- flag_credirect
final$flag_next_salary <- flag_credit_next_salary
final$flag_exclusion <- flag_exclusion
final$flag_bad_ckr_citycash <- flag_bad_ckr_citycash
final$flag_fraud <- fraud_flag
final$flag_new_credirect_old_city <- flag_new_credirect_old_city
final$flag_varnat <- flag_varnat
final$flag_cession <- flag_cession
final$flag_active <- flag_active[1,1]
final$flag_active_hidden <- flag_active[1,2]
final$flag_risky_address <- flag_risky_address$flag_risky_address
final$lat <- flag_risky_address$lat
final$lon <- flag_risky_address$lon
final$type <- flag_risky_address$hierarchy
final$precision <- flag_risky_address$location_precision
final$status_active_total <- all_df$status_active_total
final$status_finished_total <- all_df$status_finished_total
final$outs_overdue_ratio_total <- all_df$outs_overdue_ratio_total
final$source_entity_count_total <- all_df$source_entity_count_total
final$office <- all_df$office_id


# Read and write
final_exists <- read.xlsx(paste(main_dir,
  "\\Monitoring\\Files\\Scored_Credits.xlsx", sep=""))
final <- rbind(final_exists, final)
write.xlsx(final, paste(main_dir,"\\Monitoring\\Files\\Scored_Credits.xlsx", 
  sep=""))



#######
# END #
#######

