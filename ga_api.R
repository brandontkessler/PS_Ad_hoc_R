library(googleAnalyticsR)
ga_auth()
my_accounts <- ga_account_list()
ga_id <- 13471302

start_date <- "60daysAgo"
end_date <- "yesterday"

channels_byDate <- google_analytics(ga_id,
                                    date_range = c(start_date, end_date),
                                    metrics = c("sessions",
                                                "pageviews",
                                                "bounces",
                                                "avgSessionDuration",
                                                "transactions",
                                                "transactionRevenue",
                                                "totalValue",
                                                "itemRevenue"),
                                    dimensions = c("channelGrouping","date"))

channels <- google_analytics(ga_id,
                             date_range = c(start_date, end_date),
                             metrics = c("sessions",
                                         "pageviews",
                                         "bounces",
                                         "avgSessionDuration",
                                         "transactions",
                                         "transactionRevenue",
                                         "totalValue",
                                         "itemRevenue"),
                             dimensions = c("channelGrouping"))

site_locations <- google_analytics(ga_id,
                                   date_range = c(start_date, end_date),
                                   metrics = c("sessions",
                                               "bounces",
                                               "exits",
                                               "avgTimeOnPage",
                                               "pageValue"),
                                   dimensions = c("pageTitle"))




