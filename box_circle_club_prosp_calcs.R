##########################
# Box Circle Prospects
##########################

#############################################################################
# SETUP

# Clear all variables in workspace
rm(list=ls())
#############################################################################

# libs
library(tidyverse)
library(lubridate)

# funs
source('functions/data_import.R')
source('functions/ticketing_functions.R')

# today
today <- Sys.Date()

# load ticketing data
df.tix.clx <- ticketing_data(c('Clx19','Clx18'))
df.tix.pop <- ticketing_data(c('Pops19','Pops18'))
df.fy19.clx <- ticketing_data(c('Clx19'))
df.fy19.pop <- ticketing_data(c('Pops19'))


# load donor data
df.donor <- donor_data() %>%
  group_by(summary_cust_id) %>%
  summarise(total_gift = sum(gift_plus_pledge))

# create segmentation dfs
clx.segmentation <- segmentation(df.tix.clx)
pop.segmentation <- segmentation(df.tix.pop)

# Subscriber dfs
unique_subs <- function(df){
  df.fin <- sub_conversion_and_return(df) %>%
    filter(summary_cust_id != 0, paid_amt > 0) %>%
    select(summary_cust_id) %>%
    unique() %>%
    mutate(subscriber = TRUE)
  
  return(df.fin)
}

df.sub.clx <- unique_subs(df.fy19.clx)
df.sub.pop <- unique_subs(df.fy19.pop)

# Identify 2019 first time subs
first_time_subs <- function(df){
  df1 <- sub_conversion_and_return(df)
  df1 <- add_fiscal_year(df1) %>%
    filter(summary_cust_id != 0, paid_amt >0) %>%
    select(summary_cust_id,fy) %>%
    unique()
  
  df.19 <- df1 %>%
    filter(fy == '19') %>%
    mutate(fy19 = 1) %>%
    select(summary_cust_id, fy19)
  
  df.18 <- df1 %>%
    filter(fy == '18') %>%
    mutate(fy18 = 1) %>%
    select(summary_cust_id, fy18)
  
  df.new <- merge(df.19,df.18,by='summary_cust_id',all.x=TRUE) %>%
    mutate(fy18 = ifelse(is.na(fy18),0,1)) %>%
    filter(fy18==0) %>%
    mutate(new_sub = TRUE) %>%
    select(summary_cust_id, new_sub)
  
  return(df.new)
}

df.sub.clx.first <- first_time_subs(df.tix.clx)
df.sub.pop.first <- first_time_subs(df.tix.pop)


# Final dfs
df.fin.clx <- clx.segmentation %>%
  merge(df.sub.clx, by = 'summary_cust_id', all.x = TRUE) %>%
  mutate(subscriber = ifelse(is.na(subscriber),FALSE,subscriber)) %>%
  merge(df.donor, by = 'summary_cust_id', all.x = TRUE) %>%
  mutate(total_gift = ifelse(is.na(total_gift),0,total_gift)) %>%
  merge(df.sub.clx.first,by='summary_cust_id',all.x=TRUE) %>%
  mutate(new_sub = ifelse(is.na(new_sub),0,1)) %>%
  filter(new_sub == 0,
         total_gift == 0,
         cust_seg == 'aficionado' | cust_seg == 'high value regular' | cust_seg == 'high value prospect') %>%
  select(-new_sub)

df.fin.pop <- pop.segmentation %>%
  merge(df.sub.pop, by = 'summary_cust_id', all.x = TRUE) %>%
  mutate(subscriber = ifelse(is.na(subscriber),FALSE,subscriber)) %>%
  merge(df.donor,by='summary_cust_id', all.x=TRUE) %>%
  mutate(total_gift = ifelse(is.na(total_gift),0,total_gift)) %>%
  merge(df.sub.pop.first,by='summary_cust_id',all.x=TRUE) %>%
  mutate(new_sub = ifelse(is.na(new_sub),0,1)) %>%
  filter(new_sub == 0,
         total_gift == 0,
         cust_seg == 'aficionado' | cust_seg == 'high value regular' | cust_seg == 'high value prospect') %>%
  select(-new_sub)


# Write to excel
library(xlsx)
write.xlsx(df.fin.clx,
           file = 'box_club_prosp.xlsx',
           sheetName = 'clx',
           row.names = FALSE,
           append = FALSE)

write.xlsx(df.fin.pop,
           file = 'box_club_prosp.xlsx',
           sheetName = 'pop',
           row.names = FALSE,
           append = TRUE)