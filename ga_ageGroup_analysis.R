#############################################################################
# SETUP

# Clear all variables in workspace
rm(list=ls())

library(tidyverse)
library(googleAnalyticsR)

ga_auth()
my_accounts <- ga_account_list()
ga_id <- 13471302

start_date <- "2017-07-01"
end_date <- "2018-06-30"

# Where do website visitors come from?
arrival_channel_byAge <- google_analytics(ga_id,
                                   date_range = c(start_date, end_date),
                                   metrics = c("sessions"),
                                   dimensions = c("channelGrouping","userAgeBracket"))



transactions_byAge <- google_analytics(ga_id,
                                       date_range = c(start_date, end_date),
                                       metrics = c("transactions"),
                                       dimensions = c("userAgeBracket"))

# Where do website visitors bounce?
exitPage_byAge <- google_analytics(ga_id,
                                   date_range = c(start_date, end_date),
                                   metrics = c("sessions"),
                                   dimensions = c("exitPagePath","userAgeBracket")) %>%
  spread(key = userAgeBracket,value = sessions, fill = 0) %>%
  mutate(total = rowSums(.[2:7])) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  select(-total) %>%
  gather(key = 'userAgeBracket', value = 'sessions', 2:7) %>%
  arrange(userAgeBracket,desc(sessions))

##############################################
# PLOTS
##############################################

# Plot total sessions
ggplot(arrival_channel_byAge,
       aes(x = userAgeBracket,
           y = sessions)) + 
  geom_bar(stat = 'identity', fill = '#151E5B') + 
  ggtitle('Total Sessions by Age Group') + 
  theme_bw()


# Plot Transaction Revenue by Age Group
ggplot(transactionRevenue_byAge,
       aes(x = userAgeBracket,
           y = transactionRevenue)) + 
  geom_bar(stat = 'identity', fill = '#155B25') +
  ggtitle('Transaction Revenue by Age Group') + 
  theme_bw()

# Plot Arrivals by channel grouping
ggplot(arrival_channel_byAge, 
       aes(x = userAgeBracket,
           y = sessions,
           fill = factor(channelGrouping))) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_discrete(name = 'Arrival Page') + 
  ggtitle('Arrival Channels by Age Group') +
  theme_bw()

# Plot Exits
ggplot(exitPage_byAge, 
       aes(x = userAgeBracket,
           y = sessions,
           fill = factor(exitPagePath))) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  scale_fill_discrete(name = 'Exit Page') + 
  ggtitle('Exit Pages by Age Group') +
  theme_bw()



