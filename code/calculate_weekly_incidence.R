library(tidyr)
library(dplyr)

df <- read.csv('data/cases_daily.csv')

df$date <- as.Date(df$date, format = "%Y-%m-%d")

to_merge <- read.csv('data/pop_census.csv')
to_merge <- drop_na(to_merge)
to_merge$Total <- as.numeric(to_merge$Total)

to_merge <- to_merge[,c("name", "id", "Total")]

df <- merge(df, to_merge, on.x="id", on.y="id")




get_county_daily_incidence <- function(df, county_id) {
  county_df <- df[df$id == county_id,]
  
  for (i in seq(1, 6)) {
    county_df <- mutate(county_df, !!paste0("cases_back_", i) := lag(cases, i))
  }
  
  county_df$weekly_cases <- rowSums(county_df[, c("cases", "cases_back_1", "cases_back_2", 'cases_back_3', 'cases_back_4', 'cases_back_5', 'cases_back_6')])
  
  county_df$weekly_incidence <- county_df$weekly_cases / county_df$Total * 100000
  
  return(county_df)
}
to_view <- get_county_daily_incidence(df, 1001)

list_of_dfs = list()

for (county_id in unique(df$id)) {
  county_df <- get_county_daily_incidence(df, county_id)
  list_of_dfs <- append(list_of_dfs, list(county_df))
}
result_df <- do.call(rbind, list_of_dfs)
result_df


nrow(result_df[result_df$weekly_incidence >= 100 & result_df$date >= as.Date("2021-04-24") & result_df$date <= as.Date("2021-06-30"),])

columns_to_drop <- c("cases_back_1", "cases_back_2", 'cases_back_3', 'cases_back_4', 'cases_back_5', 'cases_back_6')

result_df <- result_df %>% select(-columns_to_drop)
result_df <- result_df[result_df$date >= as.Date("2021-04-14") & result_df$date <= as.Date("2021-07-10"),]

colnames(result_df)[colnames(result_df) == "Total"] <- "population"

write.csv(result_df, file = 'data/incidence.csv', row.names = FALSE)
