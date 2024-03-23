library(dplyr)

df = read.csv('data/incidence_with_policy_new.csv')

in_out_in <- function(df, county_id) {
  cdf <- filter(df, id == county_id)
  in_brake <- FALSE
  out <- FALSE
  
  for (i in 1:nrow(cdf)) {
    row <- cdf[i, ]
    if (row$brake_applies) {
      in_brake <- TRUE
    } else if (!row$brake_applies & in_brake) {
      out <- TRUE
    } else if (row$brake_applies & out) {
      return(TRUE)
    }
  }
  return(FALSE)
}

for(id in unique(df$id)){
  if(in_out_in(df, id)){
    print(id)
  }
}
