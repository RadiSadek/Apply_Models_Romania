
###################################################
######## Define some extra useful functions  ######
###################################################

# Define function to get apply cutoffs
gen_group_scores <- function(var){
   
   cutoffs <- cu_app_flexcredit
  if (var>cutoffs[1]){output="Bad"} 
  else if (var>cutoffs[2]) {output="Indeterminate"} 
  else if (var>cutoffs[3]) {output="Good 1"} 
  else if (var>cutoffs[4]) {output="Good 2"} 
  else if (var>cutoffs[5]) {output="Good 3"} 
  else {output="Good 4"}
  return (output)
}

# Define function to prepare final data frame to aggregate scoring
gen_final_df <- function(products,application_id){
  
  suppressMessages(suppressWarnings(require("reshape")))
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
  names(scoring_df)[names(scoring_df) == 'application_id'] <- 'loan_id'
  names(scoring_df)[names(scoring_df) == 'period'] <- 'installments'
  return(scoring_df)
}




