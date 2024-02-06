df <- read.csv('data/covid_de.csv')

#combine Berlin and Hamburg
df$berlin <-grepl('Berlin', df$county)


#combine by county, date
df$date <- as.Date(df$date, format = "%Y-%m-%d")
total_cases <- aggregate(cases~county+date, df, FUN=sum)

brake_test <- function(df){
  return ((df$cases >= 100) & (lag(df$cases, n=1) >= 100) & (lag(df$cases, n=2) >= 100))
}

total_cases$brake_applies <- brake_test(total_cases)
