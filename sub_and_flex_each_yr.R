##########################
# Subs/Flex each year
##########################

#############################################################################
# SETUP

# Clear all variables in workspace
rm(list=ls())

setwd("U:/models")
#############################################################################

# libs
library(tidyverse)
library(lubridate)

# funs
source('functions/data_import.R')
source('functions/ticketing_functions.R')

# load ticketing data
df.raw <- ticketing_data(c('Clx19','Clx18','Clx17','Clx16','Clx15','Clx14','Clx13','Clx12','Clx11','Clx10','Clx09','Clx08'))

# Working df
df <- add_fiscal_year(df.raw)

df.fin <- df %>%
  mutate(fy = ifelse(fy == 'Error','fy08',fy)) %>%
  filter(price_type_group == 'Subscription' | 
           price_type_group == 'Flex') %>%
  group_by(fy,price_type_group) %>%
  summarise(count = n()) %>%
  spread(price_type_group,count)




