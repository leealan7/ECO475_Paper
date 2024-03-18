library(plm)
library(did)
library(dplyr)
library(eventstudyr)

df <- read.csv('data/incidence_with_policy_new.csv')

df$percent_change <- c(diff(df$weekly_cases) / lag(df$weekly_cases) * 100)

df$percent_change[is.nan(df$percent_change) | is.infinite(df$percent_change)] <- 0

df$date <- as.Date(df$date, format = "%Y-%m-%d")

df <- filter(df, as.Date('2021/04/01') <= df$date & df$date <= as.Date('2021/07/01'))

df$days <- as.numeric(difftime(df$date, as.Date('2020/01/01')))

df$first_day_of_brake <- 0

add_brake_applies_date <- function(county_id){
  first_day <- filter(df, id==county_id & brake_applies) %>%
                slice(1)
  return(first_day$days[1])
}


for (county in unique(df$id)){
  df$first_day_of_brake[df$id==county] <- add_brake_applies_date(county)
}

twfe_mod = plm(percent_change ~ brake_applies, data=df, model="within", effect = "twoways")
summary(twfe_mod)

results <- ES(long_data=df,
              outcomevar="percent_change",
              unit_var="id",
              cal_time_var='days',
              onset_time_var='first_day_of_brake',
              cluster_vars='id')


out <- att_gt(yname = 'percent_change',
              gname='first_day_of_brake',
              idname='id',
              tname='days',
              xformla=~1,
              data = df,
              est_method='reg')

es <- aggte(out, type='dynamic')

estimates_ols_rd <- EventStudy(
  estimator = "OLS",
  data = df,   # Use package sample data
  outcomevar = "percent_change",
  policyvar = "brake_applies",
  idvar = "id",
  timevar = "days",
  pre = 0,  post = 4
)


plt <- EventStudyPlot(estimates = estimates_ols_rd)
