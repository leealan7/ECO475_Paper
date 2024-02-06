df <- read.csv('data/covid_de.csv')

df$cases_lag1 <- lag(df$cases)
df$cases_lag2 <- lag(df$cases, n=2)
df$brake_applies <- (df$cases >= 100) & (df$cases_lag1 >= 100) & (df$cases_lag2 >= 100)
