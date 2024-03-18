library(plm)
library(rstanarm)
library(data.table)
library(eventstudyr)

df <- read.csv('data/incidence_with_policy.csv')

df$date <- as.Date(df$date, format = "%Y-%m-%d")

df$days <- as.numeric(difftime(df$date, as.Date('2020/01/01')))

df$last_period_brake <- lag(df$brake_applies)
df$inc_change <- c(diff(df$weekly_incidence), 0)

make_lags <- function(df) {
  for(i in -10:-1){
    df[, sprintf("change_%d_in_past",-i)] = shift(df$inc_change, n=-i)
  }
  for(i in 1:10){
    df[, sprintf("change_%d_in_future",i)] = shift(df$inc_change, n=-i)
  }
  
  return(df)
}

df <- make_lags(df)

test_lm <- plm(change_2_in_future ~ cases + brake_applies,
           data = df,
           model = 'within',
           effect = 'twoways')

test_lm$coefficients[2]


get_reg_for_lag <- function(df, col){
  mod <- plm(col ~ cases + brake_applies,
         data = df,
         model = 'within',
         effect = 'twoways')
  return(summary(mod)$coefficients)
}

get_reg_for_lag(df, df$change_10_in_future)

selected_columns <- c("change_3_in_past", "change_2_in_past")

# Use lapply on selected columns
result <- lapply(df[selected_columns], function(col) {
  # Your function logic here
  # For example, print the column
  print(col)
})

estimates_ols_nocon <- EventStudy(
  estimator = "OLS",
  data = df,   # Use package sample data
  outcomevar = "inc_change",
  policyvar = "brake_applies",
  idvar = "id",
  timevar = "days",
  controls = NULL,
  pre = 0,  post = 4
)

estimates_ols_rd <- EventStudy(
  estimator = "OLS",
  data = df,   # Use package sample data
  outcomevar = "inc_change",
  policyvar = "brake_applies",
  idvar = "id",
  timevar = "days",
  controls = c('cases'),
  pre = 0,  post = 4
)

plt <- EventStudyPlot(estimates = estimates_ols_nocon)
plt

df$log_cng <- log(df$inc_change)

