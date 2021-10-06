
########################################
######## Define normal functions  ######
########################################

# Function to generate certain fields
gen_norm_var <- function(period,all_df,products,criteria_age){
  
  all_df$maturity <- ifelse(period==1, all_df$installments*7/30,
     ifelse(period==2, all_df$installments*14/30, all_df$installments))
  all_df$gender <- ifelse(substring(all_df$cnp,1,1) %in% c(0,2,4,6,8), 1, 0)
  all_df$dob <- 
    ifelse(substring(all_df$cnp,1,1) %in% c(1,2),
      paste("19",substring(all_df$cnp,2,3),"-",substring(all_df$cnp,4,5),"-",
                 substring(all_df$cnp,6,7),sep=""),
    ifelse(substring(all_df$cnp,1,1) %in% c(5,6),
      paste("20",substring(all_df$cnp,2,3),"-",substring(all_df$cnp,4,5),"-",
                 substring(all_df$cnp,6,7),sep=""),NA))
  all_df$age <- ifelse(is.na(all_df$dob), 18, 
      floor(as.numeric(difftime(all_df$created_at, all_df$dob, 
      units=c("days"))/365.242)))
  
  return(all_df)
}

# Function to regenerate certain functions
gen_norm_var2 <- function(df){
  df$maturity <- ifelse(df$maturity>=20,20,df$maturity)
  df$age <- ifelse(df$age>90,89,
       ifelse(df$age<18,18, df$age))
  return(df)
}




