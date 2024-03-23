library(plm)
library(did)
library(dplyr)
library(eventstudyr)
library(stargazer)
library(ggplot2)
library(minpack.lm)

df <- read.csv('data/incidence_with_policy_new.csv')

df$percent_change <- c(diff(df$weekly_cases) / lag(df$weekly_cases) * 100)

df$percent_change[is.nan(df$percent_change) | is.infinite(df$percent_change)] <- 0

df$date <- as.Date(df$date, format = "%Y-%m-%d")

df <- filter(df, as.Date('2021/04/01') <= df$date & df$date <= as.Date('2021/07/01'))

df$days <- as.numeric(difftime(df$date, as.Date('2020/01/01')))

df$brake_applies <- as.numeric(df$brake_applies)

df$day <- weekdays(as.Date(df$date))


# Make buckets for incidence
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

length(unique(df$bucket_day))

length(unique(df$bucket_day_inc))


exponential_model <- function(intercept, brake){
  
}


#model <- nls(percent_change ~ ,
#             start = list(c = 0, a=1, b=-1),
#             algorithm = "port",
#             data=df,
#             control=nls.control(maxiter = 20))
#
#summary(model)


make_model <- function(degree){
  return(lm(percent_change ~ factor(id) + factor(days)*poly(weekly_inc, degree) + brake_applies, data=df))
}

models <- lapply(1:4, make_model)


stargazer(models, type='latex', keep='brake_applies', omit.stat = c('f', 'ser'),
          add.lines = list(c('Degree of Interaction Polynonial', 1, 2, 3, 4),
                           c('unit FEs', 'Yes', 'Yes', 'Yes', 'Yes')))
