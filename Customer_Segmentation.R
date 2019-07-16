library(lubridate)
library(tidyverse)

########################### USER INPUT ##############################################

this_fy <- 2019
concert_type <- 'pop'   # Must be 'clx','pop', or 'all'

########################### VARS ##############################################
# import ticketing data
df_audience <- rbind(
                     read.csv('/data/ticketing//Clx18.csv', header = TRUE, skip = 3),
                     read.csv('/data/ticketing//Clx19.csv', header = TRUE, skip = 3),
                     read.csv('/data/ticketing//Pops18.csv', header = TRUE, skip = 3),
                     read.csv('/data/ticketing//Pops19.csv', header = TRUE, skip = 3),
                     read.csv('/data/ticketing//Summer18.csv', header = TRUE, skip = 3),
                     read.csv('/data/ticketing//Summer19.csv', header = TRUE, skip = 3)
                     ) %>%
  mutate(perf_dt = mdy_hms(perf_dt))



################################# FUNCTIONS #######################################

renew_df_creation <- function(df){
  df_renew <- df %>%
    filter(fy == max(fy)-1 |
             fy == max(fy)) %>%
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

##############################################################################
########################### IMPORT AND CLEAN AUDIENCE DATA ##################


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

########################### IMPORT AND CLEAN DONOR DATA ##################
# import donor data
df_donor <- read.csv('data/enhanced_fund_fy15-present.csv', header = TRUE) %>%
  filter(cont_fy <= this_fy) %>%
  select(summary_cust_id,gift_plus_pledge) %>%
  group_by(summary_cust_id) %>%
  summarize(gift_plus_pledge = sum(gift_plus_pledge))



#######################################################################
df_for_segmentation <- df

df_renew <- renew_df_creation(df)
df_return <- return_df_creation(df)
df_subscribers <- subs_df_creation(df)
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

final_df <- aggregate_df %>%
  