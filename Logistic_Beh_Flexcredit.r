
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_beh_flex <- function(df,scoring_df,products,df_Log_Flexcredit_Beh,
                             period,all_df){
  
  # Cut and bin
  df$ownership <- ifelse(is.na(df$ownership),"1_3",
     ifelse(df$ownership  %in% c(1,3),"1_3",df$ownership))
  df$ownership <- as.factor(df$ownership)
  
  df$education <- ifelse(is.na(df$education), "1_2",
     ifelse(df$education %in% c(3,4),"3_4","1_2"))   
  df$education <- as.factor(df$education)
  
  df$gender <- as.factor(df$gender)
  
  df$household_total <- ifelse(is.na(df$household_total), "1_2_3",
     ifelse(df$household_total %in% c(1,2,3),"1_2_3","more_4"))
  df$household_total <- as.factor(df$household_total)
  
  df$age <- ifelse(df$age<=40,"less_40",
     ifelse(df$age<=62,"41_62","more_63"))
  df$age <- as.factor(df$age)
  
  df$work_experience <- ifelse(is.na(df$work_experience),"more_12",
     ifelse(df$work_experience<=12,"0_12","more_12"))
  df$work_experience <- as.factor(df$work_experience)
  
  df$total_income <- ifelse(is.na(df$total_income),"4000_6500",
     ifelse(df$total_income<=4000,"0_4000",
     ifelse(df$total_income<=6500,"4000_6500","more_6500")))
  df$total_income <- as.factor(df$total_income)
  
  df$ratio_nb_payments_prev <- 
    ifelse(is.na(df$ratio_nb_payments_prev),"0_0.45",
    ifelse(df$ratio_nb_payments_prev<=0.45,"0_0.45",
    ifelse(df$ratio_nb_payments_prev<=0.7,"0.45_0.7","more_0.7")))
  df$ratio_nb_payments_prev <- as.factor(df$ratio_nb_payments_prev)
  
  df$max_delay <- ifelse(is.na(df$max_delay),"9_12",
     ifelse(df$max_delay<=8,"0_8",
     ifelse(df$max_delay<=12,"9_12",
     ifelse(df$max_delay<=34,"13_34","more_34"))))
  df$max_delay <- as.factor(df$max_delay)
  
  df$credits_cum <- 
    ifelse(is.na(df$credits_cum),"NA",
    ifelse(df$credits_cum %in% c(0,1),"0_1",
    ifelse(df$credits_cum==2,"2",
    ifelse(df$credits_cum==3,"3","more_4"))))
  df$credits_cum <- as.factor(df$credits_cum)
  
  df$days_diff_last_credit <- 
    ifelse(is.na(df$days_diff_last_credit),"0_4",
    ifelse(df$days_diff_last_credit<=4,"0_4","more_5"))
  df$days_diff_last_credit <- as.factor(df$days_diff_last_credit)
  
  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    # Apply logistic model to each amount and installment
    apply_logit <- predict(df_Log_Flexcredit_Beh, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],1)
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


