library(plm)
library(did)
library(dplyr)
library(eventstudyr)
library(stargazer)
library(ggplot2)

df <- read.csv('data/incidence_with_policy_new.csv')

df$percent_change <- c(diff(df$weekly_cases) / lag(df$weekly_cases) * 100)

df$percent_change[is.nan(df$percent_change) | is.infinite(df$percent_change)] <- 0

df$date <- as.Date(df$date, format = "%Y-%m-%d")

df <- filter(df, as.Date('2021/04/01') <= df$date & df$date <= as.Date('2021/07/01'))

df$days <- as.numeric(difftime(df$date, as.Date('2020/01/01')))

df$brake_applies <- as.numeric(df$brake_applies)

df$day <- weekdays(as.Date(df$date))

bucket_start <- 60
bucket_size <- 80

get_bucket <- function(inc){
  if (inc <= bucket_start){return(0)}
  else{return(ceiling((inc - bucket_start) / bucket_size))}
}

df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

length(unique(df$bucket_inc))

df$bucket_day_inc <- df$days * 1000 + df$bucket_inc

add_brake_applies_date <- function(county_id){
  first_day <- filter(df, id==county_id & brake_applies) %>%
    slice(1)
  return(first_day$days[1])
}


for (county in unique(df$id)){
  df$first_day_of_brake[df$id==county] <- add_brake_applies_date(county)
}

df$days_after_treatment = df$days - df$first_day_of_brake

add_relative_time_dummies <- function(df, before, after){
  for(k in -before:-2){
    colname <- paste('before', -k, sep="_")
    df[, colname] <- df$days_after_treatment == k
  }
  for(k in 0:after){
    colname <- paste('after', k, sep="_")
    df[, colname] <- df$days_after_treatment == k
  }
  
  df$before_focus_period <- df$days_after_treatment < before
  df$after_focus_period <- df$days_after_treatment > after
  return(df)
}

add_relative_time_shortened <- function(df, before, after){
  df$rel_time <- df$days_after_treatment
  df$rel_time[df$rel_time < -before] <- -before - 1
  df$rel_time[df$rel_time > after] <- after + 1
  
  #factor omits the smallest value. we want it to omit -1
  df$rel_time[df$rel_time == -1] <- -before - 2
  
  return(df)
}

df <- add_relative_time_dummies(df, 5, 5)
df <- add_relative_time_shortened(df, 5, 5)

model <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + factor(rel_time), data=df)

#stargazer(model, type='text')

time_vars <- paste("factor(rel_time)", c(-6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6), sep="")

te_df <- tidy(model)

conf_intervals <- confint(model, level=0.95)

rel_time_estimates <- subset(cbind(te_df, conf_intervals), term %in% time_vars)

rel_time_estimates$rel_time <- c(-6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6)

colnames(rel_time_estimates)[6] <- "lower"
colnames(rel_time_estimates)[7] <- "upper"

p<- ggplot(rel_time_estimates, aes(x=rel_time, y=estimate)) + 
  geom_pointrange(aes(ymin=lower, ymax=upper),
                position=position_dodge(.9)) +
  geom_hline(yintercept=0)

p+labs(title="Effect Size Estimates", x="Event Time", y = "Effect")+
  theme_classic()
