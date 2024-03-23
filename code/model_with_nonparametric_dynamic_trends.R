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


# Make buckets for incidence
bucket_start <- 2
bucket_size <- 5

get_bucket <- function(inc){
  if (inc <= bucket_start){return(0)}
  else{return(ceiling((inc - bucket_start) / bucket_size))}
}

df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

length(unique(df$bucket_inc))

# Make buckets for days
bucket_start <- 0
bucket_size <- 1

df$bucket_day <- lapply(df$days, get_bucket) %>%
  as.numeric()

df$bucket_day_inc <- df$bucket_day * 1000 + df$bucket_inc

length(unique(df$bucket_day))

length(unique(df$bucket_day_inc))

model_1 <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + brake_applies, data=df)

bucket_size <- 10
bucket_start <- ceiling((100-bucket_size/2)%%bucket_size)
df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

df$bucket_day_inc <- df$bucket_day * 1000 + df$bucket_inc
length(unique(df$bucket_day_inc))

model_2 <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + brake_applies, data=df)

bucket_size <- 25
bucket_start <- ceiling((100-bucket_size/2)%%bucket_size)
df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

df$bucket_day_inc <- df$bucket_day * 1000 + df$bucket_inc
length(unique(df$bucket_day_inc))

model_3 <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + brake_applies, data=df)

bucket_size <- 50
bucket_start <- ceiling((100-bucket_size/2)%%bucket_size)
df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

df$bucket_day_inc <- df$bucket_day * 1000 + df$bucket_inc
length(unique(df$bucket_day_inc))

model_4 <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + brake_applies, data=df)

bucket_size <- 80
bucket_start <- ceiling((100-bucket_size/2)%%bucket_size)
df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

df$bucket_day_inc <- df$bucket_day * 1000 + df$bucket_inc
length(unique(df$bucket_day_inc))

model_5 <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + brake_applies, data=df)

bucket_size <- 1000
bucket_start <- ceiling((100-bucket_size/2)%%bucket_size)
df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

df$bucket_day_inc <- df$bucket_day * 1000 + df$bucket_inc
length(unique(df$bucket_day_inc))

model_6 <- lm(percent_change ~ factor(id) + factor(bucket_day_inc) + brake_applies, data=df)

filter(tidy(model), term=='brake_applies')


group_sizes <- df %>%
  group_by(bucket_day_inc) %>%
  summarize(group_size = n())

# View the result
print(group_sizes)

stargazer(model_1, model_2, type='text', keep='brake_applies')
stargazer(model_4, model_5, model_6, type='text', keep='brake_applies')
