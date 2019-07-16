#####################
# Association Rules #
#####################

# libs
library(tidyverse)
library(lubridate)
library(xlsx)
library(arules)

# funs
source('functions/data_import.R')
## functions include: 
##    ticketing_data(), single_conversion_exclusive()


# List of ticketing files to load
data_list <- c('Clx15','Clx16','Clx17','Clx18','Clx19',
               'Pops15','Pops16','Pops17','Pops18','Pops19',
               'Summer15','Summer16','Summer17','Summer18','Summer19')

# load ticketing data
df <- ticketing_data(data_list)

# format into singles only (EXCLUDING ANY SUBS)
df.singles <- single_conversion_exclusive(df)

# load unique performance data
perfs <- read.xlsx('/data/performances_and_types/unique_perfs_fy15-19.xlsx', 
                   sheetName='performances') %>%
  select('perf_dt','Name.of.Performance') %>%
  rename('name' = 'Name.of.Performance') %>%
  mutate(perf_dt = ymd_hms(perf_dt))


# merge df with perfs by perf_dt
df.merged <- merge(df.singles,perfs,by='perf_dt',all.x=TRUE)

# Organize data into column for each concert
# 1) Select columns
# 2) filter for unique summary_cust_id & perf_dt
#       meaning: one person buys two tix for same concert = 1 row.
# 4) spread the names and make the tmp column equal (1) the value of each.
#       meaning: a 1 represents a purchase of the concert, 0 represents no purchase.
df.fin <- df.merged %>%
  select(summary_cust_id,name) %>%
  group_by(summary_cust_id,name) %>%
  summarise(tmp = 1) %>%
  spread(name,tmp)

# Replace all NA with 0
df.fin[is.na(df.fin)] <- 0

# Convert entire df to factors
df.fin[] <- lapply(df.fin, factor)

# create list of unique ids and concerts attended
trans = as(df.fin,"transactions")


######## ASSOCIATION RULES ############
# Support is a threshold that describes minimum purchases required to be included
#   Support is calculated by purchases for a specific item/total purchases of all items
#   We set a minimum support to weed out anything that has very few purchases
#   In the case of these concerts, all will have reasonable sales and support is not necessary
#   example: We have a dataset of a week's worth of grocery store purchases in which 10,000 
#               transactions are made. Let's say we only want to include items that are 
#               purchased a minimum of 3 times a day
#            We would then take 3*7=21 weekly purchases
#            Divided by 10,000 = 0.0021.
#            Our support would then be set to a minimum of 0.0021
# Confidence

# ECLAT
#   Simply shows the pairs most commonly purchased together.
#   Doesn't tell much analytically but just provides some info.
#   minlen is included to prevent just 1 item as a combo.
rules.eclat = eclat(data = trans, parameter = list(support = 0.1, minlen = 2))
inspect(sort(rules.eclat, by = 'support')[1:10])

# APRIORI
#   Support
#   Confidence 
rules.apriori = apriori(data = trans, parameter = list(support = 0, confidence = ))



