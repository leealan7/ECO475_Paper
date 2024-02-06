df <- read.csv('data/cases_daily.csv')

df$date <- as.Date(df$date, format = "%Y-%m-%d")

to_merge <- read.csv('data/covid_de.csv')