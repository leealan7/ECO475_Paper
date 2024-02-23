library(rstanarm)

df <- read.csv('data/incidence.csv')

df_test <- data.frame(
  weekly_incidence = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
)

make_lags <- function(df) {
  df$over <- df$weekly_incidence >= 100
  
  df$over_lag_1 = lag(df$over)
  df$over_lag_2 = lag(df$over, n=2)
  df$over_lag_3 = lag(df$over, n=3)
  df$over_lag_4 = lag(df$over, n=4)
  df$over_lag_5 = lag(df$over, n=5)
  df$over_lag_6 = lag(df$over, n=6)
  df$over_lag_7 = lag(df$over, n=7)
  
  return(df) 
}

df <- make_lags(df)
df_test <- make_lags(df_test)

entry_condition <- function(row) {
  return(row$over_lag_3 & row$over_lag_4 & row$over_lag_5)
}

exit_condition <- function(row) {
  return(!(row$over_lag_3 | row$over_lag_4 | row$over_lag_5 | row$over_lag_6 | row$over_lag_7))
}

brake_applies <- function(row) {
  return(entry_condition(row) | (TRUE == TRUE & exit_condition(row) == FALSE))
}

df_test$entry_condition <- entry_condition(df_test)
df_test$exit_condition <- exit_condition(df_test)

apply(df_test, 1, brake_applies)
