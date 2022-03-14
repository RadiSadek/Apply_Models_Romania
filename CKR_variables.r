
#####################################################
######## Functions to compute CKR variables  ########
#####################################################

# Function to set correctly CKR variables
gen_ckr_variables <- function(db_name,all_df,flag_beh,application_id){
 
  ccr <- suppressWarnings(fetch(dbSendQuery(con, 
           gen_ccr_data(application_id,db_name)), n=-1))
  ccr <- ccr[rev(order(ccr$created_at)),]
  ccr <- ccr[1,]

  if(is.na(ccr$content)){
    max_days_delay <- NA
    max_month_pay <- NA
  } else {
    json <- as.data.frame(do.call(rbind,lapply(ccr$content,
       function(j) as.list(unlist(fromJSON(j))))))
    max_days_delay <- gen_get_ccr_data_max("max_crt_delay_days",json)
    max_month_pay <- gen_get_ccr_data_max("maximum_monthly_payed",json)
  }

  all_df$ccr_max_delay <- max_days_delay
  all_df$ccr_monthly_payed <- max_month_pay
  return(all_df)
}

# Function to get the maximum of bank/financial of CCR reports
gen_get_ccr_data_max <- function(input,json){
  
  here <- json[ , grepl(input,names(json))]
  here <- here[ , grepl("#text",names(here))][1,]
  if(ncol(here)==2){
    result <- max(as.numeric(here[1,1]),as.numeric(here[1,2]))
  } else if(ncol(here)==1){
    result <- as.numeric(here[1,1])
  } else if(ncol(here)==0){
    max_days_delay <- NA
  } else {
    result <- -999
  }
}

