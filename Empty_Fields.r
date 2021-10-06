
###################################################
######## Functions to deal with missing data ######
###################################################

# Transform all NULLs to NAs
gen_null_to_na <- function(df){
  if (df$on_address=="NULL" | is.na(df$on_address)) {
    df$on_address <- as.numeric(NA)}
  if (df$ownership=="NULL" | is.na(df$ownership)) {
    df$ownership <- as.numeric(NA)}
  if (df$education=="NULL" | is.na(df$education)) {
    df$education <- as.numeric(NA)}
  if (df$work_experience=="NULL" | is.na(df$work_experience)) {
    df$work_experience <- as.numeric(NA)}
  if (df$age=="NULL" | is.na(df$age)) {
    df$age <- as.numeric(NA)}
  if (df$total_income=="NULL" | is.na(df$total_income)) {
    df$total_income <- as.numeric(NA)}
  if (df$gender=="NULL" | is.na(df$gender)) {
    df$gender <- as.numeric(NA)}
  return(df)
}

# Function to count number of empty fields for each model
gen_empty_fields <- function(df){
  empty_fields <- sum(is.na(df[,
        c("on_address","ownership","education","work_experience",
          "age","total_income","gender")]))
  return(empty_fields)
}
