rm(list=ls())
setwd('U:/models')
library(tidyverse)
source('functions/data_import.R')


df <- ticketing_data(c('Clx18'))

df.wr <- df %>%
  select(summary_cust_id,perf_dt,paid_amt,price_type_group,zone_desc) %>%
  mutate(dow = weekdays(perf_dt)) %>%
  filter(!is.na(summary_cust_id),
         paid_amt != 0,
         summary_cust_id != 0) %>%
  select(-perf_dt) %>%
  mutate(price_zone = substr(zone_desc, start =1, stop = 7)) %>%
  select(-zone_desc)

df.by <- read.csv('/data/active_user_info_birthyr.csv', skip = 6) %>%
  rename(summary_cust_id = customer_no,
         birth_yr = key_value) %>%
  select(summary_cust_id,birth_yr) %>%
  filter(birth_yr != 0) %>%
  mutate(age = year(today()) - birth_yr,
         age_grp = ifelse(age <= 19,
                          'less20',
                          ifelse(age <= 29,
                                 'twenties',
                                 ifelse(age <= 39,
                                        'thirties',
                                        ifelse(age <= 49,
                                               'fourties',
                                               ifelse(age <= 59,
                                                      'fifties',
                                                      ifelse(age <= 69,
                                                             'sixties',
                                                             ifelse(age <= 79,
                                                                    'seventies',
                                                                    ifelse(age <= 89,
                                                                           'eighties',
                                                                           ifelse(age <= 99,
                                                                                  'nineties',
                                                                                  'hundreds')))))))))) %>%
  select(-birth_yr,-age)


df.fin <- df.wr %>%
  merge(df.by, by='summary_cust_id', all.x=TRUE)


# DOW ANALYSIS
dow_analysis <- df.fin %>%
  select(summary_cust_id,dow,age_grp) %>%
  unique() %>%
  filter(!is.na(age_grp)) %>%
  group_by(dow,age_grp) %>%
  summarise(count = n()) %>%
  filter(dow != 'Tuesday') %>%
  ungroup(dow,age_grp) %>%
  mutate(dow = factor(dow, levels = c('Thursday','Friday','Saturday')),
         percent = count / ifelse(dow == 'Friday',
                                  sum(count[dow == 'Friday']),
                                  ifelse(dow == 'Thursday',
                                         sum(count[dow == 'Thursday']),
                                         sum(count[dow == 'Saturday']))))


  
ggplot(dow_analysis, 
       aes(x = age_grp,
           y = count,
           fill = dow)) + 
  geom_bar(stat="identity", width = 0.5, position = 'dodge', color = 'black') + 
  ggtitle('DoW age Demographics - total number of customers') +
  theme_bw() + 
  scale_x_discrete(limits = c('thirties','fourties','fifties','sixties','seventies','eighties','nineties','hundreds'))

ggplot(dow_analysis, 
       aes(x = age_grp,
           y = percent,
           fill = dow)) + 
  geom_bar(stat="identity", width = 0.5, position = 'dodge', color = 'black') + 
  ggtitle('DoW age Demographics - percentage of total audience') +
  theme_bw() + 
  scale_x_discrete(limits = c('thirties','fourties','fifties','sixties','seventies','eighties','nineties','hundreds'))

