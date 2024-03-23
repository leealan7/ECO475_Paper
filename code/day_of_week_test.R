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

df$day <- weekdays(as.Date(df$date))

for (day_of_week in unique(df$day)){
  df[, day_of_week] <- as.numeric(df$day==day_of_week)
}

model <- lm(percent_change ~ factor(day), data = df)

summary(model)
