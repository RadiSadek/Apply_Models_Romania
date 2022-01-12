
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_flex <- function(df,scoring_df,products,df_Log_Flexcredit_App,
                             period,all_df){
  
  # Cut and bin
  df$ownership <- ifelse(is.na(df$ownership),"1",
     ifelse(df$ownership %in% c(1),"1_2",
     ifelse(df$ownership %in% c(2),"1_2","3_4")))
  df$ownership <- as.factor(df$ownership)
  
  df$on_address <- ifelse(is.na(df$on_address),"more_60",
     ifelse(df$on_address<=60,"0_60","more_60"))
  df$on_address <- as.factor(df$on_address)
  
  df$education <- ifelse(is.na(df$education), "1_2_3",
     ifelse(df$education==4,"4","1_2_3"))   
  df$education <- as.factor(df$education)
  
  df$work_experience <- ifelse(is.na(df$work_experience),"more_25",
     ifelse(df$work_experience<=6,"0_6",
     ifelse(df$work_experience<=24,"7_24","more_25")))
  df$work_experience <- as.factor(df$work_experience)
  
  df$total_income <- ifelse(
    is.na(df$total_income),"2400_3500",
    ifelse(df$total_income<=2400,"less_2400",
    ifelse(df$total_income<=3500,"2400_3500",
    ifelse(df$total_income<=4500,"3500_4500",
    ifelse(df$total_income<=5500,"4500_5500","more_5500")))))
  df$total_income <- as.factor(df$total_income)
  
  df$gender <- as.factor(df$gender)
  
  df$age <- 
    ifelse(df$age<=33,"less_33",
    ifelse(df$age<=51,"34_51",
    ifelse(df$age<=57,"51_57","more_58")))
  df$age <- as.factor(df$age)
  
  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    # Apply logistic model to each amount and installment
    apply_logit <- predict(df_Log_Flexcredit_App, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],0)
    scoring_df$pd[i] <- round(apply_logit,3)
    scoring_df$color[i] <- 0
    scoring_df$color[i] <- ifelse(scoring_df$color[i]==1 | 
              scoring_df$score[i]=="Bad", 1, 
              ifelse(scoring_df$score[i]=="Indeterminate", 2,
              ifelse(scoring_df$score[i]=="Good 1", 3,
              ifelse(scoring_df$score[i]=="Good 2", 4,
              ifelse(scoring_df$score[i]=="Good 3", 5,
              ifelse(scoring_df$score[i]=="Good 4", 6, NA))))))
  }
  return(scoring_df)
}


