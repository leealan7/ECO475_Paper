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

bucket_start <- 50
bucket_size <- 25

get_bucket <- function(inc){
  if (inc <= bucket_start){return(0)}
  else{return(ceiling((inc - bucket_start) / bucket_size))}
}

df$bucket_inc <- lapply(df$weekly_inc, get_bucket) %>%
  as.numeric()

length(unique(df$bucket_inc))

df$bucket_day_inc <- df$days * 1000 + df$bucket_inc

df$Brake = ifelse(df$brake_applies==1, 'Applies', 'Does Not Apply')

ggplot(df, aes(x=bucket_inc, color=Brake, fill=Brake)) + 
  geom_histogram(bins=length(unique(df$bucket_inc)), position="dodge") + 
  theme(panel.background = element_rect(fill='#37474f'),
        plot.background = element_rect(fill = "#37474f"),
        legend.background = element_rect(fill = "#37474f"),
        text = element_text(colour = "#e0e0e0"),
        axis.text = element_text(colour = "#e0e0e0"))+
  xlab('Bucket')

