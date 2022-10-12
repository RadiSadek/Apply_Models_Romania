
###################################################
######## Define some extra useful functions  ######
###################################################

# Define function to get apply cutoffs
gen_group_scores <- function(var,flag_beh,office){
  
  flag_office <- flag_bad_office(office)
  
  if(flag_beh==1){
    if(flag_office==1){
      cutoffs <- cu_beh_flexcredit_good
    } else if(flag_office==2){
      cutoffs <- cu_beh_flexcredit
    } else if(flag_office==3){
      cutoffs <- cu_beh_flexcredit_bad
    } else if(flag_office==4){
      cutoffs <- cu_beh_flexcredit_worst
    } else {
      cutoffs <- cu_beh_flexcredit
    }
  } else {
    if(flag_office==1){
      cutoffs <- cu_app_flexcredit_good
    } else if(flag_office==2){
      cutoffs <- cu_app_flexcredit
    } else if(flag_office==3){
      cutoffs <- cu_app_flexcredit_bad
    } else if(flag_office==4){
      cutoffs <- cu_app_flexcredit_worst
    } else {
      cutoffs <- cu_app_flexcredit
    }
  }
  if (var>cutoffs[1]){output="Bad"} 
  else if (var>cutoffs[2]) {output="Indeterminate"} 
  else if (var>cutoffs[3]) {output="Good 1"} 
  else if (var>cutoffs[4]) {output="Good 2"} 
  else if (var>cutoffs[5]) {output="Good 3"} 
  else {output="Good 4"}
  return (output)
}

# Define function to prepare final data frame to aggregate scoring
gen_final_df <- function(products,application_id,all_df){
  
  # Read table with number of payments/number of for city cash
table_flex <- table(products$installments, products$amount)
  for (i in 1:nrow(table_flex)){
    for (j in 1:ncol(table_flex)){
      if (table_flex[i,j]>0){
        table_flex[i,j] <- row.names(table_flex)[i]
      } else {
        table_flex[i,j] <- NA}}
  }
  
  # Make dataframe of all possible amounts/installments
  vect_flex_installment <- sort(as.numeric(unique(unlist(table_flex))))
  vect_flex_amount <- colnames(table_flex, do.NULL = TRUE, 
                                   prefix = "col")
  PD_flex <- matrix("", ncol = length(vect_flex_installment), 
                        nrow = length(vect_flex_amount))
  colnames(PD_flex) <- vect_flex_installment
  rownames(PD_flex) <- vect_flex_amount
  melted <- as.data.frame(melt(t(PD_flex)))
  names(melted) <- c("period","amount","value")
  melted$value <- as.numeric(melted$value)
  
  # Remove unneccessary rows from melted dataframe
  for(i in 1:nrow(melted)){
    c1 <- as.character(melted$period[i]) 
    c2 <- as.character(melted$amount[i])
    melted$value[i] <- ifelse(is.na(table_flex[c1,c2]),0,1)
  }
  scoring_df <- subset(melted, melted$value==1)[,1:2]
  names(scoring_df) <- c("period","amount")
  scoring_df$application_id <- application_id
  return(scoring_df)
}


# Correct maximum installment amount of PO 
gen_correct_max_installment_po <- function(period_po,period,installment_amount){
  if(period_po==3 & period==1){
    result <- installment_amount*7/30
  } else if (period_po==3 & period==2){
    result <- installment_amount*14/30
  } else if (period_po==2 & period==3){
    result <- installment_amount*30/14
  } else if (period_po==2 & period==1){
    result <- installment_amount*7/14
  } else if (period_po==1 & period==3){
    result <- installment_amount*30/7
  } else if (period_po==1 & period==2){
    result <- installment_amount*14/7
  }
  return(result)
}

# Function to create column for scoring table for display
gen_final_table_display <- function(scoring_df){
  scoring_df$display_score <- 
   ifelse(scoring_df$color %in% c(1),"No",
   ifelse(scoring_df$score %in% c("NULL"),"NULL","Yes"))
 
   scoring_df$color <- ifelse(scoring_df$display_score=="No",1,
   ifelse(scoring_df$display_score=="NULL",2, 6))
  
  scoring_df$color <- ifelse(scoring_df$score %in% 
    c("Indeterminate","Good 1","Good 2","Good 3","Good 4") & 
    scoring_df$display_score=="No",2,scoring_df$color)
  
  names(scoring_df)[names(scoring_df) == 'application_id'] <- 'loan_id'
  names(scoring_df)[names(scoring_df) == 'period'] <- 'installments'
  return(scoring_df)
}

# Gen flag bad office
flag_bad_office <- function(var_off){
  return(
    ifelse(
     var_off %in% c("34","1","18","42","47","2","44","39"), 1,
    ifelse(
     var_off %in% c("17","4","49","35","28","22","48","10","33","5",
                    "15","41","37","11","25","29","20","13","8","19","27"), 2,
    ifelse(
     var_off %in% c("43","7","38","31","40","32","30","36","12","3","45"), 3,
    ifelse(
     var_off %in% c("14","23","16","24","51","21","26","52"), 4, 2
    )))))
}

# Function to make string for DB update of PO terminated (delete offer)
gen_string_po_terminated <- function(input){
  string_sql_update <- input$id[1]
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      string_sql_update <- paste(string_sql_update,input$id[i],
                                 sep=",")}}
  return(paste("(",string_sql_update,")",sep=""))
}


# Define sql string query for writing in DB for PO terminated
gen_sql_string_po_terminated <- function(input,inc){
  return(paste("(",input$id[inc],",",
    input$office_id[inc],",",input$client_id[inc],",",
    input$group[inc],",",input$product_id[inc],",",
    input$loan_id[inc],",",input$credit_amount[inc],",",
    input$installment_amount[inc],",",input$credit_amount_updated[inc],",",
    input$installment_amount_updated[inc],",",input$hide_until_date[inc],",",
    input$consultant_id[inc],",'",input$created_at[inc],"',",
    input$updated_at[inc],",",input$deleted_at[inc],")",
         sep=""))
}

# Function  to get last credit per company 
gen_if_credit_after_po_terminated <- function(input,table_po,name){
  result <- as.data.frame(aggregate(
    input$id[input$master_client_id %in% table_po$client_id],
    by=list(input$master_client_id[input$master_client_id %in% 
    table_po$client_id]),FUN=max))
  names(result) <- c("master_client_id",name)
  result <- merge(result,input[,c("id","activated_at")],by.x = name,by.y = "id",
                  all.x = TRUE)
}

# Function to make string for DB update of PO terminated (update offer)
gen_string_delete_po_terminated <- function(input,var,var_name,db_name){
  iterate_string <- paste("WHEN id = ",input$id[1]," THEN ",var[1],sep="")
  if(nrow(input)>1){
    for(i in 2:nrow(input)){
      iterate_string <- paste(iterate_string,
      paste("WHEN id = ",input$id[i]," THEN ",var[i],sep=""))}
  }
  return(paste("UPDATE ",db_name,".clients_prior_approval_applications SET ",
     var_name," = CASE ",iterate_string," ELSE ",var_name," END;",sep=""))
}
