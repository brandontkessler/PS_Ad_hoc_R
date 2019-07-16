###########################################
# Classics Daily 3yr Analysis
#
# Authored by: Brandon Kessler 3/21/19
###########################################

rm(list=ls())

# libs
library(tidyverse)
library(lubridate)
library(fpp2)
library(plotly)

# functions
source('functions/data_import.R')

# load the data
data_list <- c('Clx19','Clx18','Clx17','Clx16')
df.tix <- ticketing_data(data_list)
df.donor <- donor_data()

# donor formatting
df.donor.frmtd <- df.donor %>%
  group_by(summary_cust_id) %>%
  summarise(amt_5yr = sum(gift_plus_pledge)) %>%
  mutate(donor_group = ifelse(amt_5yr >= 500000,'A',
                              ifelse(amt_5yr >= 250000,'B',
                                     ifelse(amt_5yr >= 100000,'C',
                                            ifelse(amt_5yr >= 50000,'D',
                                                   ifelse(amt_5yr >= 10000,'E',
                                                          ifelse(amt_5yr >= 1000,'F',
                                                                 ifelse(amt_5yr >= 10,'G',
                                                                        'H')))))))) %>%
  select(-amt_5yr)

# tix formatting
df.tix.frmtd <- df.tix %>%
  mutate(price_type_group = ifelse(price_type_group == 'Single ' | price_type_group == 'Discount',
                                   'Single',
                                   ifelse(price_type_group == 'Subscription',
                                          'Subscription',
                                          ifelse(price_type_group == 'Flex','Flex',
                                                 ifelse(price_type_group == 'Comp','Comp','Unsold')))),
         price_zone = substr(zone_desc,1,7),
         day_of_week = weekdays(perf_dt)) %>%
  select(perf_dt,paid_amt,summary_cust_id,price_zone,day_of_week,price_type_group,attended)

# UNSOLD
df.unsold <- df.tix.frmtd %>%
  filter(price_type_group == 'Unsold') %>%
  mutate(day_of_week = factor(day_of_week))

# SOLD
df.sold <- df.tix.frmtd %>%
  filter(price_type_group != 'Unsold') %>%
  merge(df.donor.frmtd, by = 'summary_cust_id', all.x = TRUE) %>%
  mutate(donor_grp = ifelse(is.na(donor_group),'H',donor_group)) %>%
  select(-donor_group) %>%
  mutate(day_of_week = factor(day_of_week),
         attended = ifelse(attended == "Attended",1,0))


######################################
# Analysis
######################################

####################
# Total
####################
# total rev
total_rev <- df.sold %>%
  filter(day_of_week != 'Tuesday') %>%
  group_by(day_of_week) %>%
  summarise(rev = sum(paid_amt))

ggplot(total_rev, aes(x=day_of_week, fill = day_of_week)) +
  geom_bar(aes(y = rev), stat="identity", show.legend = FALSE, color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Revenue by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# tix sold + attendance
tix_sold <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group != 'Comp') %>%
  group_by(day_of_week) %>%
  summarise(tix_sold = n(),
            attended = (sum(attended)/tix_sold)*tix_sold) %>%
  gather(key = category, value = count, -day_of_week)

ggplot(tix_sold, aes(y = count, x = day_of_week, group = category, fill = category)) +
  geom_bar(stat="identity", width = 0.5, position = 'dodge', color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Tickets Sold/Attended by DoW (excluding Comps)') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# Comps
comps <- df.unsold %>%
  filter(day_of_week != 'Tuesday') %>%
  group_by(day_of_week) %>%
  summarise(count = n())

ggplot(comps, aes(y = count, x=day_of_week, fill = day_of_week)) +
  geom_bar(stat="identity", show.legend = FALSE, color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Tickets Comped by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# Subscribers
subs <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group == 'Subscription' | price_type_group == 'Flex') %>%
  group_by(day_of_week, price_type_group) %>%
  summarise(subscribers = length(unique(summary_cust_id)),
            rev = sum(paid_amt))

ggplot(subs, aes(y = subscribers, x = day_of_week, group = price_type_group, fill = price_type_group)) + 
  geom_bar(stat = "identity", position='dodge', width = 0.4, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Number of Subscribers by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))

ggplot(subs, aes(y = rev, x = day_of_week, group = price_type_group, fill = price_type_group)) + 
  geom_bar(stat = "identity", position='dodge', width = 0.4, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Sub Revenue by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# Donors
donors <- df.sold %>%
  filter(day_of_week != 'Tuesday') %>%
  group_by(day_of_week,donor_grp) %>%
  summarise(donors = length(unique(summary_cust_id)))

ggplot(donors, aes(y=donors, x=day_of_week, group=donor_grp, fill=donor_grp)) + 
  geom_bar(stat = 'identity', position='dodge', width = 0.9, color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Number of Donors by Group and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))

# Donors A-D
donors <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         donor_grp != 'H',
         donor_grp != 'G',
         donor_grp != 'F',
         donor_grp != 'E') %>%
  group_by(day_of_week,donor_grp) %>%
  summarise(donors = length(unique(summary_cust_id)))

ggplot(donors, aes(y=donors, x=day_of_week, group=donor_grp, fill=donor_grp)) + 
  geom_bar(stat = 'identity', position='dodge', width = 0.9, color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Number of Donors by Group and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))




####################
# By Price Zone
####################
#revenue
total_rev_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(rev = sum(paid_amt))

ggplot(total_rev_pz, aes(y=rev, x=day_of_week, group=price_zone, fill=price_zone)) + 
  geom_bar(stat = 'identity', position='dodge', width = 0.9, color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Rev by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))

# tix sold
tix_sold_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group != 'Comp') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(tix_sold = n())

ggplot(tix_sold_pz, aes(y = tix_sold, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Tix Sold by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# % of tix sold
tix_sold_perc_pz <- df.unsold %>%
  filter(day_of_week != 'Tuesday') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(unsold = n()) %>%
  merge(tix_sold_pz, by = c('day_of_week','price_zone')) %>%
  group_by(day_of_week,price_zone) %>%
  summarise(percentage_sold = tix_sold/(tix_sold + unsold))

ggplot(tix_sold_perc_pz, aes(y = percentage_sold, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.75, color = 'black') + 
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  ggtitle('Tix Sold Percentage by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# avg price paid by price zone
avg_price_pz <- df.sold %>% 
  filter(day_of_week != 'Tuesday',
         price_type_group != 'Comp') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(avg_price = sum(paid_amt)/n())

ggplot(avg_price_pz, aes(y = avg_price, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Avg Paid Ticket Price by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# avg price paid by singles by pz
avg_price_singles_pz <- df.sold %>% 
  filter(day_of_week != 'Tuesday',
         price_type_group == 'Single') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(avg_price = sum(paid_amt)/n())

ggplot(avg_price_singles_pz, aes(y = avg_price, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Avg Paid Ticket Price of Singles by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# Comps
comps_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group == 'Comp') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(comps = n(),
            attendance = sum(attended)) %>%
  gather(category, count, -price_zone, -day_of_week)

ggplot(comps_pz, aes(y = count, x = day_of_week, group = price_zone, fill = category)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Comps and Attendance by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# % attendance by pz
attendance_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group != 'Comp') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(attendance = sum(attended)/n())

ggplot(attendance_pz, aes(y = attendance, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = 'identity', position = 'dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Attendance by Paid Ticket and Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# subscribers non-flex by pz
subs_nonFlex_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group == 'Subscription') %>%
  group_by(day_of_week, price_zone, price_type_group) %>%
  summarise(subscribers = length(unique(summary_cust_id)),
            rev = sum(paid_amt))

ggplot(subs_nonFlex_pz, aes(y = subscribers, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = "identity", position='dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Number of Non-Flex Subscribers by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))

ggplot(subs_nonFlex_pz, aes(y = rev, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = "identity", position='dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Non-Flex Sub Revenue by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# subscribers flex by pz
subs_flex_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group == 'Flex') %>%
  group_by(day_of_week, price_zone, price_type_group) %>%
  summarise(subscribers = length(unique(summary_cust_id)),
            rev = sum(paid_amt))

ggplot(subs_flex_pz, aes(y = subscribers, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = "identity", position='dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Number of Flex Subscribers by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))

ggplot(subs_flex_pz, aes(y = rev, x = day_of_week, group = price_zone, fill = price_zone)) + 
  geom_bar(stat = "identity", position='dodge', width = 0.9, color = 'black') + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() +
  ggtitle('Flex Sub Revenue by DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))


# donors by pz
donors_ABCD_pz <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         donor_grp == 'A' | donor_grp == 'B' | donor_grp == 'C' | donor_grp == 'D') %>%
  group_by(day_of_week,price_zone) %>%
  summarise(donors = length(unique(summary_cust_id)))

ggplot(donors_ABCD_pz,
       aes(y=donors, x=day_of_week, group=price_zone, fill=price_zone)) + 
  geom_bar(stat = 'identity', position='dodge', width = 0.75, color = 'black') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  theme_bw() + 
  ggtitle('Number of A,B,C,D Donors by Price Zone and DoW') + 
  scale_x_discrete(limits = c('Thursday','Friday','Saturday'))



####################
# Time Series
####################
df.monthly <- df.sold %>%
  filter(day_of_week != 'Tuesday',
         price_type_group != 'Comp') %>%
  mutate(month = as.Date(paste0(year(perf_dt),'-',month(perf_dt),'-1'))) %>%
  group_by(month,day_of_week) %>%
  summarise(rev_per_perf = sum(paid_amt)/length(unique(perf_dt)),
            tix_sold = n())

monthly.tix_sold <- df.monthly %>%
  select(-rev_per_perf) %>%
  spread(day_of_week, tix_sold)

monthly.rev_per_perf <- df.monthly %>%
  select(-tix_sold) %>%
  spread(day_of_week, rev_per_perf)


# Create a monthly factor to merge with df.monthly to replace non-existent months with a 0
tt <- as.data.frame(seq(as.Date('2015-10-1'),as.Date('2019-6-1'),'month')) %>%
  rename(month = 1)

monthly.tix_sold <- merge(monthly.tix_sold, tt, by = 'month', all = TRUE) %>%
  mutate(Friday = replace(Friday, is.na(Friday), 0),
         Saturday = replace(Saturday, is.na(Saturday), 0),
         Thursday = replace(Thursday, is.na(Thursday), 0))

monthly.rev_per_perf <- merge(monthly.rev_per_perf, tt, by = 'month', all = TRUE) %>%
  mutate(Friday = replace(Friday, is.na(Friday), 0),
         Saturday = replace(Saturday, is.na(Saturday), 0),
         Thursday = replace(Thursday, is.na(Thursday), 0))

# Create time series
ts.tix_sold <- ts(monthly.tix_sold[,c('Thursday','Friday','Saturday')],
                  start = c(2015, 10),
                  end = c(2019, 6),
                  frequency = 12)

ts.rev_per_perf <- ts(monthly.rev_per_perf[,c('Thursday','Friday','Saturday')],
                      start = c(2015, 10),
                      end = c(2019, 6),
                      frequency = 12)

pol_deg <- 8

# tix sold
autoplot(ts.tix_sold) + 
  ggtitle('Tix Sold per DoW')

ggplot(monthly.tix_sold, aes(x = month)) + 
  stat_smooth(aes(y = Thursday, color = 'Thursday'),
              method = 'lm', 
              formula = y ~ poly(x, degree = pol_deg),
              se = FALSE) + 
  stat_smooth(aes(y = Friday, color = 'Friday'),
              method = 'lm', 
              formula = y ~ poly(x, degree = pol_deg),
              se = FALSE) +
  stat_smooth(aes(y = Saturday, color = 'Saturday'),
              method = 'lm', 
              formula = y ~ poly(x, degree = pol_deg),
              se = FALSE) +
  ggtitle('Tix Sold per DoW (polynomial smoothing)') +
  theme_bw()


# rev per perf
autoplot(ts.rev_per_perf) + 
  ggtitle('Rev Per Performance per DoW')

ggplot(monthly.rev_per_perf, aes(x = month)) + 
  stat_smooth(aes(y = Thursday, color = 'Thursday'),
              method = 'lm', 
              formula = y ~ poly(x, degree = pol_deg),
              se = FALSE) + 
  stat_smooth(aes(y = Friday, color = 'Friday'),
              method = 'lm', 
              formula = y ~ poly(x, degree = pol_deg),
              se = FALSE) +
  stat_smooth(aes(y = Saturday, color = 'Saturday'),
              method = 'lm', 
              formula = y ~ poly(x, degree = pol_deg),
              se = FALSE) +
  ggtitle('Revenue per Performance per DoW (polynomial smoothing)') +
  theme_bw()






