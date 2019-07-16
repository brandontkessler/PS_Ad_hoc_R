############################################
# Marketing Expenditure Regressions
# 
# Authored by: Brandon Kessler 3/21/2019
############################################

# SETUP
############################################
#clear
rm(list=ls())

#libs
library(tidyverse)
library(lubridate)
library(googleAnalyticsR)

#sources
source('functions/data_import.R')

#ticketing files to load
data_list <- c('Clx18','Clx17','Clx16','Clx15','Clx14',
               'Pops18','Pops17','Pops16','Pops15','Pops14',
               'Summer14','Summer15','Summer16','Summer17','Summer18')

#load ticketing data
df.tix <- ticketing_data(data_list)

#convert to non-sub singles
# convert order dates to the first of each month
# aggregate revenue by year-month
df.singles <- single_conversion_exclusive_and_return(df.tix)
df.singles.formatted <- df.singles %>%
  filter(paid_amt > 0) %>%
  mutate(order_dt = as.Date(order_dt,'%m/%d/%Y'),
         order_month = paste0(year(order_dt),'-',month(order_dt)),
         order_month = as.Date(paste0(order_month,'-01'))) %>%
  group_by(order_month) %>%
  summarise(revenue = sum(paid_amt)) %>%
  rename(date = order_month)
df.singles.formatted <- df.singles.formatted[-c(1:3),]


#count performances per month
df.performances <- df.tix %>%
  group_by(perf_dt) %>%
  summarise(count = 1) %>%
  mutate(perf_dt = as.Date(paste0(year(perf_dt),'-',month(perf_dt),'-1'))) %>%
  group_by(perf_dt) %>%
  summarise(performances = sum(count)) %>%
  rename(date = perf_dt)


#load expense data
# convert dates to the first of each month
# aggregate expenses by year-month
df.exp <- read.csv('/data/marketing_expenses/mktg_exp_byLineItem_fy14-18-singleTix.csv')
colnames(df.exp) <- c('date','exp','purpose')
df.exp.formatted <- df.exp %>%
  mutate(date = as.Date(date,'%m/%d/%Y'),
         date = paste0(year(date),'-',month(date),'-01'),
         date = as.Date(date),
         exp = as.numeric(exp)) %>%
  group_by(date,purpose) %>%
  summarise(exp = sum(exp)) %>%
  spread(purpose,exp)

df.exp.formatted[is.na(df.exp.formatted)] <- 0  


#combine dfs
df.exp_rev_merge <- merge(df.singles.formatted, df.exp.formatted, by='date') %>%
  merge(df.performances, by = 'date')


df.exp_rev_merge.lm <- lm(log(revenue) ~ 
                            DigitalAds + 
                            PrintMedia + 
                            PrintProduction + 
                            EmailServices +
                            DirectMail +
                            RadioAndTV +
                            performances + 
                            Promotions,
                          df)
summary(df.exp_rev_merge.lm)





# Bring in GA
ga_auth()
my_accounts <- ga_account_list()
ga_id <- 13471302

date_list <- c("2018-06-30","2017-06-30","2016-06-30","2015-06-30","2014-06-30")
names(date_list) <- c("2017-07-01","2016-07-01","2015-07-01","2014-07-01","2013-07-01")


df.ga <- data.frame()
for(i in names(date_list)){
  df <- google_analytics(ga_id,
                         date_range = c(i, date_list[i]),
                         metrics = c("sessions","transactions","transactionRevenue"),
                         dimensions = c("date"))
  
  df.ga <- rbind(df.ga,df)
}

# format df.ga
df.ga.formatted <- df.ga %>%
  mutate(date = as.Date(paste0(year(date),'-',month(date),'-1'))) %>%
  group_by(date) %>%
  summarise(sessions = sum(sessions),
            transactions = sum(transactions),
            transactionRevenue = sum(transactionRevenue))

# merge google analytics with expenses
df.gaMerge <- merge(df.exp_rev_merge,df.ga.formatted,by='date')

df.fin <- df.gaMerge %>%
  mutate(printMedia = PrintMedia + PrintProduction,
         rev_per_performance = revenue/performances)

df.fin.50_to_100k_rev <- df.fin %>%
  filter(revenue < 100000 & revenue > 50000)

# PLOTS
##################
ggplot(df.fin.50_to_100k_rev, aes(x = log(DigitalAds), y = log(sessions))) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(df.fin.50_to_100k_rev, aes(x = log(printMedia), y = sessions)) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(df.fin.50_to_100k_rev, aes(x = log(DirectMail), y = sessions)) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(df.fin.50_to_100k_rev, aes(x = log(RadioAndTV), y = sessions)) +
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(df.fin.50_to_100k_rev, aes(x = log(sessions), y = sessions)) +
  geom_point() + 
  geom_smooth(method = "lm")


df.lm <- lm(log(sessions) ~ 
              log(DigitalAds) + 
              printMedia + 
              EmailServices +
              DirectMail +
              RadioAndTV +
              Performances + 
              Promotions
            ,
            df.fin.50_to_100k_rev)
summary(df.lm)




