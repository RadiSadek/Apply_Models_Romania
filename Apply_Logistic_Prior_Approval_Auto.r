

################################################################################
#         Script for generating  daily offers for terminated credits           #
#      Apply Logistic Regression on all products (CityCash and Credirect)      #
#                          Version 1.0 (2020/06/23)                            #
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



#######################
### Manual settings ###
#######################

# Defines the directory where the RScript is located
base_dir <- Sys.getenv("RSCRIPTS_PATH", unset = "", names = FALSE)

# Define product id
product_id <- NA



#####################
####### MySQL #######
#####################

db_host <- Sys.getenv("SCORING_DB_HOST", 
                      unset = "localhost", 
                      names = FALSE)
db_port <- strtoi(Sys.getenv("SCORING_DB_PORT", 
                             unset = "3306", 
                             names = FALSE))
db_name <- Sys.getenv("SCORING_DB_DATABASE", 
                      unset = "citycash", 
                      names = FALSE)
db_username <- Sys.getenv("SCORING_DB_USERNAME", 
                          unset = "root", 
                          names = FALSE)
db_password <- Sys.getenv("SCORING_DB_PASSWORD", 
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
source(file.path(base_dir,"Terminated.r"))
source(file.path(base_dir,"SQL_queries.r"))
source(file.path(base_dir,"Useful_Functions.r"))



########################
####### Settings #######
########################

# Load predefined libraries
rdata <- file.path(base_dir, "rdata","flexcredit_beh_coeffs.rdata")
load(rdata)

# Define product id
product_id <- NA


#############################
### Read current po table ###
#############################

# Read current table
get_po_sql <- paste("SELECT * FROM ",db_name,
".clients_prior_approval_applications",sep="")
po <- suppressWarnings(fetch(dbSendQuery(con,get_po_sql), n=-1))
po_raw <- po

# Read current database
if(nrow(po)==0){
  id_max <- 1
} else {
  id_max <- max(po$id)+1
}


###################################################
### Generate data of potential credits to offer ###
###################################################

# Get date of previous day
prev_day <- Sys.Date() - 1

# Read all credits
get_actives_sql <- paste("
SELECT id, master_client_id, amount, activated_at, office_id, 
finished_at, status, product_id
FROM ",db_name,".loans WHERE activated_at IS NOT NULL",sep="")
all_credits <- suppressWarnings(fetch(dbSendQuery(con,get_actives_sql), n=-1))
all_credits_raw <- subset(all_credits,all_credits$status %in% c(10:12))

# Read credit applications
select <- subset(all_credits,!is.na(all_credits$finished_at) & 
    substring(all_credits$finished_at,1,10)==prev_day)
select <- subset(select,select$status %in% c(10:12))



#####################################################
### Apply selection criteria for credits to offer ###
#####################################################

# Remove those who have active credit of corresponding company
all_credits$activated_at_day <- substring(all_credits$activated_at,1,10)
actives <- subset(all_credits,is.na(all_credits$finished_at))
actives <- subset(actives,actives$activated_at_day>=prev_day)
select <- select[!(select$master_client_id %in% actives$master_client_id),]

# Correct for older product_id 
select$product_id <- ifelse(select$product_id %in% c(1,11),12,
    ifelse(select$product_id %in% c(2,10),13,select$product_id))

# Products not to be included in the offers
select <- subset(select,select$product_id %in% c(12,13,9,14))



#####################
### Compute score ###
#####################

if(nrow(select)>0){

# Compute and append score
select <- select[!duplicated(select$master_client_id),]
select$max_amount <- NA
select$max_installment_amount <- NA
select$score_max_amount <- NA
select$max_delay <- NA
for(i in 1:nrow(select)){
  suppressWarnings(tryCatch({
    client_id <- select$master_client_id[i]
    last_id <- select$id[i]
    calc <- gen_terminated_fct(con,client_id,product_id,last_id,0)
    select$max_amount[i] <- calc[[1]]
    select$max_installment_amount[i] <- calc[[2]]
    select$score_max_amount[i] <- calc[[3]]
    select$max_delay[i] <- as.numeric(calc[[4]])
  }, error=function(e){}))
}



##################################
### Reapply selection criteria ###
##################################

# Select based on score and DPD
select <- subset(select,select$max_amount>-Inf & select$max_amount<Inf)
select <- subset(select,select$max_delay<=360)



#############################################
### Work on final dataset and write in DB ###
#############################################

if(nrow(select)>0){

# Create final dataframe for writing in DB
offers <- select
offers$id <- seq(id_max,id_max+nrow(offers)-1,1)
offers$created_at <- Sys.time()
offers$loan_id <- NA
offers$updated_at <- NA
offers$deleted_at <- NA
offers$credit_amount_updated <- NA
offers$consultant_id <- NA
offers$installment_amount_updated <- NA
offers <- offers[,c("id","loan_id","master_client_id","consultant_id",
  "max_amount", "credit_amount_updated","max_installment_amount",
  "installment_amount_updated","office_id","product_id",
  "created_at","updated_at","deleted_at")]
names(offers)[names(offers)=="max_amount"] <- "credit_amount"
names(offers)[names(offers)=="max_installment_amount"] <- "installment_amount"
names(offers)[names(offers)=="master_client_id"] <- "client_id"
offers[is.na(offers)] <- "NULL"


# Recorrect obsolete product ids
offers$product_id <- ifelse(offers$product_id %in% c(1,11),12,
  ifelse(offers$product_id %in% c(2,10),13,offers$product_id))


# Make result ready for SQL query
string_sql <- gen_sql_string_po_terminated(offers,1)
if(nrow(offers)>1){
  for(i in 2:nrow(offers)){
    string_sql <- paste(string_sql,gen_sql_string_po_terminated(offers,i),
                        sep=",")
  }
}


# Output real offers
update_prior_query <- paste("INSERT INTO ",db_name,
".clients_prior_approval_applications VALUES ",
string_sql,";", sep="")


# Write in database
if(nrow(offers)>0){
  suppressMessages(suppressWarnings(dbSendQuery(con,update_prior_query)))
}}}



######################################################
### Check for special cases and delete immediately ###
######################################################

# Remove obsolete offers
suppressMessages(suppressWarnings(dbSendQuery(con,paste(
"DELETE FROM ",db_name,".clients_prior_approval_applications 
WHERE credit_amount = 0",sep=""))))

# Read special cases (deceased and gdrk marketing clients) 
get_special_sql <- paste("
SELECT id
FROM ",db_name,".clients
WHERE dead_at IS NOT NULL",sep="")
special <- suppressWarnings(fetch(dbSendQuery(con,get_special_sql), n=-1))

# Remove special cases if has offer
po_get_special_query <- paste(
  "SELECT id, client_id
  FROM ",db_name,".clients_prior_approval_applications
  WHERE deleted_at IS NULL",sep="")
po_special <- suppressWarnings(fetch(dbSendQuery(con,po_get_special_query), 
  n=-1))
po_special <- po_special[po_special$client_id %in% special$id,]

if(nrow(po_special)>0){
  po_special_query <- paste("UPDATE ",db_name,
   ".clients_prior_approval_applications SET updated_at = '",
   substring(Sys.time(),1,19),"', deleted_at = '",
   paste(substring(Sys.time(),1,10),"04:00:00",sep=),"'
   WHERE id IN",gen_string_po_terminated(po_special), sep="")
  suppressMessages(suppressWarnings(dbSendQuery(con,po_special_query)))
}



###########
### End ###
###########

