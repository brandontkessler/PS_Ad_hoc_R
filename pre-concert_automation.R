rm(list = ls())

# --- NOTES ---
# Make sure to only include data of the four fiscal years in question
#   with the most recent being the one including the concert in question
#   therefore, if the concert in question takes place in FY17, only include
#   data for Fy14,15,16, and 17
#
# Make sure to update the imported data with new file names
#
# Make sure to update the if else statement in DATA SETUP
#   to include only the years in question

library(dplyr)
library(lubridate)
library(ggplot2)
library(xlsx)

###################################################################################
###################################################################################
# --------------------------------------------------------------------------
# --------------------------------- USER INPUTS -------------------------
# --------------------------------------------------------------------------
name_of_series <- 'MusicOfQueen'

concert_date <- as.Date('2019-04-26')
# Concert date must be in this format

concert_type <- 'pop'
# FOR concert_type, only use "clx","pop", or "other"

###################################################################################

# import ticketing data
df_audience <- rbind(read.csv('data/Clx16.csv', header = TRUE, skip = 3),
                     read.csv('data/Clx17.csv', header = TRUE, skip = 3),
                     read.csv('data/Clx18.csv', header = TRUE, skip = 3),
                     read.csv('data/Clx19.csv', header = TRUE, skip = 3),
                     read.csv('data/Pops16.csv', header = TRUE, skip = 3),
                     read.csv('data/Pops17.csv', header = TRUE, skip = 3),
                     read.csv('data/Pops18.csv', header = TRUE, skip = 3),
                     read.csv('data/Pops19.csv', header = TRUE, skip = 3),
                     read.csv('data/Summer17.csv', header = TRUE, skip = 3),
                     read.csv('data/Summer18.csv', header = TRUE, skip = 3),
                     read.csv('data/Summer19.csv', header = TRUE, skip = 3)) %>%
  mutate(perf_dt = mdy_hms(perf_dt))

summer16_df <- read.csv('data/summer16.csv', header = TRUE, skip = 3) %>%
  mutate(perf_dt = mdy_hm(perf_dt))

# Bind them together and remove summer16_df
df_audience <- rbind(df_audience,summer16_df)
rm(summer16_df)

df_audience <- df_audience[df_audience$summary_cust_id %in% names(which(table(df_audience$summary_cust_id) <= 500)),]

df <- df_audience %>%
  mutate(seat = paste(section,' ',row,'-',seat)) %>%
  select(summary_cust_id,season_desc,price_type_group,paid_amt,zone_desc,perf_dt,summary_cust_name,seat) %>%
  filter(summary_cust_id > 0,
         paid_amt > 5)

df <- df %>%  
  mutate(fy = ifelse(df$perf_dt < as.Date('2016-07-01'),
                     16,
                     ifelse(df$perf_dt < as.Date('2017-07-01'),
                            17,
                            ifelse(df$perf_dt < as.Date('2018-07-01'),
                                   18,19))))

# import donor data
df_donor <- read.csv('data/enhanced_fund_fy15-present.csv', header = TRUE) %>%
  select(summary_cust_id,gift_plus_pledge) %>%
  group_by(summary_cust_id) %>%
  summarize(gift_plus_pledge = sum(gift_plus_pledge))

df_for_segmentation <- df %>%
  filter(fy == max(fy) |
           fy == max(fy)-1)

renew_df_creation <- function(df){
  df_renew <- df %>%
    filter(fy == max(fy)-1 |
             (fy == max(fy) & 
                perf_dt < concert_date)) %>%
    group_by(summary_cust_id) %>%
    summarize() %>%
    mutate(renew = 1)
}

return_df_creation <- function(df){
  df_return <- df %>%
    filter(fy == max(fy)-2 |
             fy == max(fy)-3) %>%
    group_by(summary_cust_id) %>%
    summarize() %>%
    mutate(return = 1)
}

subs_df_creation <- function(df){
  df_subscribers <- df %>%
    filter((fy == max(fy) & price_type_group == 'Subscription') |
             (fy == max(fy) & price_type_group == 'Flex')) %>%
    group_by(summary_cust_id) %>%
    summarize() %>%
    mutate(subscriber = 1)
}

seat_finder_df_creation <- function(df){
  df_seat_finder <- df %>%
    filter(perf_dt > concert_date &
             perf_dt < concert_date + 1) %>%
    select(summary_cust_id,summary_cust_name,seat)
}

this_concert_df_creation <- function(df){
  df_this_concert <- df %>%
    filter(perf_dt > concert_date & 
             perf_dt < concert_date + 1) %>%
    group_by(summary_cust_id) %>%
    summarize()
}

aggregate_df_creation <- function(df_for_segmentation){
  aggregate_df <- if(concert_type == 'clx'){
    df_for_segmentation %>%
      filter(season_desc == 'PS 17-18 Classics' |
               season_desc == 'PS 18-19 Classics') %>%
      group_by(summary_cust_id) %>%
      summarize(freq = n(),
                avg_paid_amt = mean(paid_amt),
                tot_paid_amt = sum(paid_amt))
  } else if (concert_type == 'pop'){
    df_for_segmentation %>%
      filter(season_desc == 'PS 17-18 Pops' |
               season_desc == 'PS 18-19 Pops') %>%
      group_by(summary_cust_id) %>%
      summarize(freq = n(),
                avg_paid_amt = mean(paid_amt),
                tot_paid_amt = sum(paid_amt))
  } else {
    df_for_segmentation %>%
      group_by(summary_cust_id) %>%
      summarize(freq = n(),
                avg_paid_amt = mean(paid_amt),
                tot_paid_amt = sum(paid_amt))
  }
}

addSegments <- function(dataset, 
                        ticket_price_avg, 
                        ticket_price_upper,
                        ticket_price_lower,
                        frequency_avg,
                        frequency_upper,
                        frequency_lower) {
  ifelse(dataset$avg_paid_amt > ticket_price_avg &
           dataset$freq > frequency_upper,
         'Aficionado',
         ifelse(dataset$avg_paid_amt < ticket_price_avg & 
                  dataset$freq > frequency_avg,
                'Committed low-budget',
                ifelse(dataset$avg_paid_amt < ticket_price_upper &
                         dataset$freq < frequency_avg &
                         dataset$freq > frequency_lower,
                       'Evolving concert-goer',
                       ifelse(dataset$avg_paid_amt > ticket_price_avg & 
                                dataset$freq < frequency_lower,
                              'High-value prospect',
                              ifelse(dataset$avg_paid_amt < ticket_price_avg &
                                       dataset$freq < frequency_lower,
                                     'One-timer', 'High-value regular')))))
}

plot_classification <- function(dt=df_this_concert){
  ggplot(data = dt %>%
           group_by(classification) %>%
           summarize(count = n()), 
         aes(x=classification, y=count, fill = classification)) + 
    guides(fill=FALSE) + 
    geom_bar(stat='identity', color = 'black') +
    ggtitle('Concert Purchaser Classification')
}

plot_type <- function(dt=df_this_concert){
  ggplot(data = dt %>%
           group_by(subscriber) %>%
           summarize(count = n()), 
         aes(x=subscriber, y=count, fill=subscriber)) + 
    guides(fill=FALSE) +
    geom_bar(stat='identity', color = 'black') +
    ggtitle('Concert Purchaser Type') + 
    coord_flip()
}

plot_segment <- function(dt=df_this_concert){
  ggplot(data = dt %>%
           group_by(segment) %>%
           summarize(count = n()), 
         aes(x=segment, y=count, fill=segment)) + 
    guides(fill=FALSE) +
    geom_bar(stat='identity', color = 'black') +
    ggtitle('Concert Purchaser Segment') + 
    coord_flip()
}

plot_donor_group <- function(dt=df_this_concert){
  ggplot(data = dt %>%
           group_by(donor_group) %>%
           summarize(count = n()), 
         aes(x=donor_group, y=count, fill=donor_group)) + 
    guides(fill=FALSE) +
    geom_bar(stat='identity', color = 'black') +
    ggtitle('Concert Purchaser Donor Group')
}
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
# NEW DFS

df_renew <- renew_df_creation(df)
df_return <- return_df_creation(df)
df_subscribers <- subs_df_creation(df)
df_seat_finder <- seat_finder_df_creation(df)
df_this_concert <- this_concert_df_creation(df)
aggregate_df <- aggregate_df_creation(df_for_segmentation)


avg_price <- sum(aggregate_df$tot_paid_amt)/sum(aggregate_df$freq)
upper_quad_price <- avg_price + sd(aggregate_df$avg_paid_amt)
lower_quad_price <- avg_price - sd(aggregate_df$avg_paid_amt)

avg_freq <- mean(aggregate_df$freq)
upper_quad_freq <- avg_freq + sd(aggregate_df$freq)
lower_quad_freq <- ifelse(avg_freq - sd(aggregate_df$freq) <= 0,2.1,avg_freq - sd(aggregate_df$freq))

aggregate_df$segment <- addSegments(aggregate_df,
                                    avg_price,
                                    upper_quad_price,
                                    lower_quad_price,
                                    avg_freq,
                                    upper_quad_freq,
                                    lower_quad_freq)

df_this_concert <- merge(df_this_concert,df_renew,all.x=TRUE)
df_this_concert <- merge(df_this_concert,df_return,all.x=TRUE)
df_this_concert <- merge(df_this_concert,df_subscribers,all.x=TRUE)
df_this_concert <- merge(df_this_concert,df_donor,all.x=TRUE)
df_this_concert <- merge(df_this_concert,aggregate_df[,c('summary_cust_id','segment')],all.x=TRUE)
df_this_concert[is.na(df_this_concert)] <- 0

df_this_concert <- df_this_concert %>%
  mutate(classification = ifelse(renew == 1,'renew',
                                 ifelse(return == 1,'return','new')),
         donor_group = ifelse(gift_plus_pledge < 1000,'Group A',
                              ifelse(gift_plus_pledge < 10000,'Group B',
                                     ifelse(gift_plus_pledge < 100000,'Group C','Group D')))) %>%
  rename(donor_5yr_history = gift_plus_pledge)


# plot_classification()
# plot_type()
# plot_segment()
# plot_donor_group()


write.xlsx(df_this_concert %>%
             select(summary_cust_id,subscriber,classification,donor_5yr_history,segment),
           file = paste0(name_of_series,'_',concert_date,'.xlsx'),
           sheetName = 'concert_summary',
           row.names = FALSE,
           append = FALSE)
write.xlsx(df_seat_finder,
           file = paste0(name_of_series,'_',concert_date,'.xlsx'),
           sheetName = 'seat_finder',
           row.names = FALSE,
           append = TRUE)

