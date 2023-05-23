
##########################################################################
## Apply correction to prior approval credits (terminated - refinanced) ##
##########################################################################

# Function to apply correction depending if terminated or refinance
gen_correction_po_fct <- function(con,db_name,all_df,all_id,
      scoring_df,products,period,application_id){
  
  # Read offers of terminated or refinance
  po <- suppressWarnings(fetch(dbSendQuery(con, 
    gen_po_terminated_query(db_name,all_df$master_client_id)), n=-1))
  po <- subset(po,is.na(po$deleted_at))
  po <- subset(po,po$credit_amount>0)

  # Rearrange if offer is still valid
  if(nrow(po)>0){
    po <- po[rev(order(po$created_at)),]
    po <- po[1,]
    time_to_now <- difftime(Sys.time(),po$deleted_at,units = c("days"))
    if(is.na(time_to_now) | (!is.na(time_to_now) & time_to_now<7)){
      if(!(any(names(scoring_df) == 'installment_amount'))){
        installments <- suppressWarnings(fetch(dbSendQuery(con, 
           gen_installment_amount_query(db_name,all_df$product_id)), n=-1))
        scoring_df <- merge(scoring_df,installments,
           by.x = c("amount","period"),by.y = c("amount","installments"),
           all.x = TRUE)
      }
      scoring_df$color <- 
        ifelse(scoring_df$amount>po$credit_amount & 
            scoring_df$installment_amount>po$installment_amount & 
            scoring_df$score!="NULL",1,scoring_df$color)
      scoring_df <- scoring_df[,-which(names(scoring_df) %in% 
            c("installment_amount"))]
    }
  }
  return(scoring_df)
}

