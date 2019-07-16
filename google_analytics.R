library(ggplot2)
library(tidyverse)
library(googleAnalyticsR)
library(caTools)

ga_id <- 13471302

start_date <- "500daysAgo"
end_date <- "yesterday"

ga_auth()
my_accounts <- ga_account_list()

df <- google_analytics(ga_id,
                       date_range = c(start_date, end_date),
                       metrics = c("socialInteractions"),
                       dimensions = c("date"))
 

ggplot(df)+
  geom_line(aes(y=socialInteractions, x = date)) + 
  theme_bw()

df[df['socialInteractions'] == max(df['socialInteractions']),]

