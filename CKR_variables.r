
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
  } else {
    json <- as.data.frame(do.call(rbind,lapply(ccr$content, 
         function(j) as.list(unlist(fromJSON(j))))))
    json_here <- json[ , grepl( "max_crt_delay_days", names(json))]
    rm(json)
    here <- json_here[ , grepl( "#text", names(json_here))][1,]
    
    if(ncol(here)==2){
      max_days_delay <- max(as.numeric(here[1,1]),as.numeric(here[1,2]))
    } else if(ncol(here)==1){
      max_days_delay <- as.numeric(here[1,1])
    } else if(ncol(here)==0){
      max_days_delay <- NA
    } else {
      max_days_delay <- -999
    }
  }

  all_df$ccr_max_delay <- max_days_delay
  return(all_df)
}



