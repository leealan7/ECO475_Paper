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

df$brake_applies <- as.numeric(df$brake_applies)

df$first_day_of_brake <- 0

df$day <- weekdays(as.Date(df$date))



add_brake_applies_date <- function(county_id){
  first_day <- filter(df, id==county_id & brake_applies) %>%
                slice(1)
  return(first_day$days[1])
}


for (county in unique(df$id)){
  df$first_day_of_brake[df$id==county] <- add_brake_applies_date(county)
}

for (day_of_week in unique(df$day)){
  df[, day_of_week] <- as.numeric(df$day==day_of_week)
}

twfe_mod = plm(percent_change ~ brake_applies + factor(days), data=df, model="within", effect = "twoways")
summary(twfe_mod)



estimates_ols_rd <- EventStudy(
  estimator = "OLS",
  data = df,   # Use package sample data
  outcomevar = "percent_change",
  policyvar = "brake_applies",
  idvar = "id",
  timevar = "days",
  pre = 1,  post = 7,
  controls = "Monday + Tuesday"
)


plt <- EventStudyPlot(estimates = estimates_ols_rd)
plt
