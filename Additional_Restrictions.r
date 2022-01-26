
#############################################################
######## Apply restrictions on final scoring dataframe ######
#############################################################

# Function to apply restrictions for application scorecard
gen_restrict_app <- function(scoring_df,all_df){
  
  low_amounts <- c(2000,1500,1200,1000,800)
  high_amounts <- c(2500,2000,1500,1200,1000)
  
  if(is.na(all_df$ccr_max_delay)){
    amounts <- high_amounts
  } else if(all_df$ccr_max_delay<=60){
    amounts <- high_amounts
  } else {
    amounts <- low_amounts
  }
  
  scoring_df$color <- 
    ifelse(scoring_df$score %in% c("NULL"),scoring_df$color,
    ifelse(scoring_df$score %in% c("Good 4") & scoring_df$amount>amounts[1],1,
    ifelse(scoring_df$score %in% c("Good 3") & scoring_df$amount>amounts[2],1,      
    ifelse(scoring_df$score %in% c("Good 2") & scoring_df$amount>amounts[3],1, 
    ifelse(scoring_df$score %in% c("Good 1") & scoring_df$amount>amounts[4],1,
    ifelse(scoring_df$score %in% c("Indeterminate") & 
          scoring_df$amount>amounts[5],1,scoring_df$color))))))

  return(scoring_df)
}

# Function to apply restrictions for repeat scorecard
gen_restrict_rep <- function(scoring_df,all_df){
  
  return(scoring_df)
}