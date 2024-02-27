library(dplyr)
library(ggplot2)
library(plm)

df <- read.csv('data/incidence_with_policy.csv')

df$inc_change <- c(diff(df$weekly_incidence), 0)

plm(inc_change ~ cases + brake_applies,
    data = df,
    model = 'within',
    effect = 'twoways') %>%
  summary()
