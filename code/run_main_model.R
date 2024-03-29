library(rstanarm)

df <- read.csv('data/incidence.csv')

make_lags <- function(df) {
  df$over <- df$weekly_incidence >= 100
  
  for(i in 1:7){
    df[, sprintf("over_lag_%d",i)] = lag(df$over, n=i)
  }
  
  return(df) 
}

df <- make_lags(df)

entry_condition <- function(row) {
  return(row$over_lag_3 & row$over_lag_4 & row$over_lag_5)
}

exit_condition <- function(row) {
  return(!(row$over_lag_3 | row$over_lag_4 | row$over_lag_5 | row$over_lag_6 | row$over_lag_7))
}

brake_applies <- function(df, i) {
  if(i<=5){
    return(FALSE)
  }
  
  in_cond <- df$entry_condition[i]
  out_cond <- df$exit_condition[i]
  last_period_brake <- df$brake_applies[i-1]
  
  return(in_cond | (last_period_brake & (!out_cond)))
}

df$entry_condition <- entry_condition(df)
df$exit_condition <- exit_condition(df)

df$brake_applies <- FALSE

for(i in 0:nrow(df)){
  br <- brake_applies(df, i)
  df$brake_applies[i] <- br
}

