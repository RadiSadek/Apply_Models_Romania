
##############################################################################
######## Functions to apply logisit regression on application City Cash ######
##############################################################################

gen_app_flex <- function(df,scoring_df,products,df_Log_Flexcredit_App,
                             period,all_df){
  
  # Cut and bin
  df$ownership <- ifelse(is.na(df$ownership),"1",
      ifelse(df$ownership %in% c(1),"1",
      ifelse(df$ownership %in% c(2),"2","3_4")))
  df$ownership <- as.factor(df$ownership)
  
  df$education <- ifelse(is.na(df$education), "1",
      ifelse(df$education %in% c(3,4),"3_4",
      ifelse(df$education %in% c(1,2),"1_2","1_2")))
  df$education <- as.factor(df$education)
  
  df$gender <- as.factor(df$gender)
  
  df$age <- 
    ifelse(df$age<=30,"less_30",
    ifelse(df$age<=34,"31_34",
    ifelse(df$age<=52,"35_52",
    ifelse(df$age<=59,"53_59",
    ifelse(df$age<=66,"60_66","67_more")))))
  df$age <- as.factor(df$age)
  
  df$on_address <- ifelse(is.na(df$on_address),"more_36",
    ifelse(df$on_address<=36,"0_36","more_36"))
  df$on_address <- as.factor(df$on_address)
  
  df$total_income <- ifelse(
    is.na(df$total_income),"less_4500",
    ifelse(df$total_income<=4500,"less_4500","4500_more"))
  df$total_income <- as.factor(df$total_income)
  
  df$ccr_monthly_payed_cut <- 
    ifelse(is.na(df$ccr_monthly_payed),"350_1100",
    ifelse(df$ccr_monthly_payed<=350,"0_350",
    ifelse(df$ccr_monthly_payed<=1100,"350_1100",
    ifelse(df$ccr_monthly_payed<=1800,"1100_1800",
    ifelse(df$ccr_monthly_payed<=2800,"1800_2800","more_2800")))))
  df$ccr_monthly_payed <- as.factor(df$ccr_monthly_payed_cut)
  
  # Apply logisic regression
  for(i in 1:nrow(scoring_df)){
    
    # Apply logistic model to each amount and installment
    apply_logit <- predict(df_Log_Flexcredit_App, newdata=df, type="response")
    scoring_df$score[i] <- apply_logit
    scoring_df$score[i] <- gen_group_scores(scoring_df$score[i],0,
                                            all_df$office_id)
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


