library(lubridate)
library(tidyverse)
library(xlsx)
########################### USER INPUT ##############################################

this_fy <- 2019
concert_type <- 'all'   # Must be 'clx','pop', or 'all'

########################### VARS ##############################################


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

##################################################################################################
########################### IMPORT AND CLEAN AUDIENCE DATA ##################
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

########################### IMPORT AND CLEAN DONOR DATA ##################
# import donor data
df_donor <- read.csv('data/donors_fy09-fy19.csv', header = TRUE) %>%
  filter(cont_fy <= this_fy)


########################### IMPORT AND CLEAN BDAY DATA ##################
df_bdays <- read.csv('data/active_user_info_birthyr.csv', header = TRUE, skip = 7) %>%
  select(customer_no,key_value) %>%
  mutate(age = ifelse(key_value > 0,as.numeric(format(Sys.Date(),"%Y")) - key_value,NA)) %>%
  rename(summary_cust_id = customer_no,
         birth_yr = key_value)


###################################################### EXECUTE ###############################
####################### Audience DF
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

aggregate_df <- aggregate_df %>%
  select(summary_cust_id,segment)

######################### Donor DF
donors <- df_donor %>%
  group_by(summary_cust_id) %>%
  summarise(amt_CY = sum(gift_plus_pledge[cont_fy == this_fy]),
            amt_PY = sum(gift_plus_pledge[cont_fy == this_fy-1]),
            amt_last5yrs = sum(gift_plus_pledge[cont_fy == this_fy |
                                                  cont_fy == this_fy-1 |
                                                  cont_fy == this_fy-2 |
                                                  cont_fy == this_fy-3 |
                                                  cont_fy == this_fy-4]),
            amt_last10yrs = sum(gift_plus_pledge[cont_fy == this_fy-5 |
                                                   cont_fy == this_fy-6 |
                                                   cont_fy == this_fy-7 |
                                                   cont_fy == this_fy-8 |
                                                   cont_fy == this_fy-9 |
                                                   cont_fy == this_fy-10])+amt_last5yrs,
            most_recent_cont_yr = max(cont_fy))

####################### MERGE DFs

final_df <- left_join(donors,df_bdays,by="summary_cust_id")
final_df <- left_join(final_df,aggregate_df,by="summary_cust_id")
final_df <- left_join(final_df,df_subscribers,by="summary_cust_id")

write.xlsx(as.data.frame(final_df),
           file='legacy_donor_list.xlsx',
           sheetName = '10yr_donors',
           row.names = FALSE,
           append = FALSE)
