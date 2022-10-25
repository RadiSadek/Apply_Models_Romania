
#####################################################
######## Functions to compute CKR variables  ########
#####################################################

# Function to set correctly CKR variables
gen_ckr_variables <- function(db_name,all_df,flag_beh,application_id,
                              all_credits_raw){
  if(nrow(all_credits_raw)==1){
    list_ids <- paste("(",all_credits_raw$id[1],")",sep="")
  } else {
    list_ids <- gen_string_po_terminated(all_credits_raw)
  }
  ccr <- suppressWarnings(fetch(dbSendQuery(con, 
           gen_ccr_data(list_ids,db_name)), n=-1))
  ccr <- ccr[rev(order(ccr$created_at)),]
  ccr <- ccr[1,]

  if(is.na(ccr$content)){
    max_days_delay <- NA
    max_month_pay <- NA
    criteria_last6m <- NA
  } else {
    json <- as.data.frame(do.call(rbind,lapply(ccr$content,
       function(j) as.list(unlist(fromJSON(j))))))
    max_days_delay <- gen_get_ccr_data_max("max_crt_delay_days",json)
    max_month_pay <- gen_get_ccr_data_max("maximum_monthly_payed",json)
    criteria_last6m <- suppressWarnings(gen_get_index_last_6m(
      "credit_reports.credit_report.delays_history.yr",json,ccr))
  }

  all_df$ccr_max_delay <- max_days_delay
  all_df$ccr_monthly_payed <- max_month_pay
  all_df$ccr_criteria_last_6m <- criteria_last6m
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

# Function to get index of last 6 months in delays history
gen_get_index_last_6m <- function(input,json,ccr){
  
  here <- json[ , grepl(input,names(json))]
  
  # Transform json file into dataframe 
  datafr <- cbind(row.names(as.data.frame(t(here))),(as.data.frame(t(here))))
  rownames(datafr) <- NULL
  names(datafr) <- c("col","value")
  
  # Get last 6 months 
  cur_year <- as.numeric(substring(ccr$created_at,1,4))
  cur_month <- as.numeric(substring(ccr$created_at,6,7))
  all_months <- c(seq(1,12,1),seq(1,12,1))
  last_6m <- all_months[c(
    (which(!is.na(match(all_months,cur_month)))[[2]]-5)):
    (which(!is.na(match(all_months,cur_month)))[[2]])]
  
  # Compute certain variables to identify the necessary values
  value <- subset(datafr,
    grepl("credit_reports.credit_report.delays_history.yr.@value",
          datafr$col))
  value$col2 <- gsub("credit_reports.credit_report.delays_history.yr.@value", 
          "", value$col)
  max_id <- as.numeric(max(as.numeric(value$col2))) - 1
  
  if(last_6m[1]<last_6m[6]){
    
    values <- datafr$value[datafr$col==paste("credit_reports.credit_report.",
      "delays_history.yr.month.@value",last_6m[1],
      ifelse(max_id==0,"","."),ifelse(max_id==0,"",max_id),sep="")]
    for(i in 2:length(last_6m)){
    values <- rbind(values,
      datafr$value[datafr$col==paste("credit_reports.credit_report.",
     "delays_history.yr.month.@value",last_6m[i],
     ifelse(max_id==0,"","."),ifelse(max_id==0,"",max_id),sep="")])
    }
  } else {

    max_id2 <- max_id - 1 
    for(i in 1:(length(last_6m)-1)){
      if(last_6m[i]>last_6m[i+1]){break}
    }
    when_change <- i
    
   values <- datafr$value[datafr$col==paste("credit_reports.credit_report.",
      "delays_history.yr.month.@value",last_6m[1],
      ifelse(max_id2==0,"","."),ifelse(max_id2==0,"",max_id2),sep="")]
    for(i in 2:length(last_6m)){
      if(i<=when_change){
        values <- rbind(values,
           datafr$value[datafr$col==paste("credit_reports.credit_report.",
           "delays_history.yr.month.@value",last_6m[i],
           ifelse(max_id2==0,"","."),ifelse(max_id2==0,"",max_id2),sep="")])
      } else {
        values <- rbind(values,
           datafr$value[datafr$col==paste("credit_reports.credit_report.",
           "delays_history.yr.month.@value",last_6m[i],
           ifelse(max_id==0,"","."),ifelse(max_id==0,"",max_id),sep="")])
      }
    }
  }
  
  values <- as.character(values)
  return(length(values[values %in% c("6","G")]))

}


