# --- NOTES ---
# Make sure to update the imported data with new file names


###################################################################################
###################################################################################
# --------------------------------------------------------------------------
# --------------------------------- USER INPUTS -------------------------
# --------------------------------------------------------------------------


###################################################################################
###################################################################################
# --------------------------------- AGGREGATE DATA -------------------------
###################################################################################
###################################################################################
# --------------------------------------------------------------------------
# --------------------------------- DATA SETUP -------------------------
# --------------------------------------------------------------------------
# ------------------ DEV ---------------------

library(tidyr)

df_donors <- read.csv('data/enhanced_fund_2013plus.csv')

# Build the donors dataframe
donors_agg <- df_donors %>%
  mutate(campaign_fy = substr(campaign, 1, 8)) %>%
  filter(campaign_fy == 'PS 13-14' |
           campaign_fy == 'PS 14-15' |
           campaign_fy == 'PS 15-16' |
           campaign_fy == 'PS 16-17' |
           campaign_fy == 'PS 17-18') %>%
  select(gift_plus_pledge,fund_desc,campaign_fy,summary_cust_id,cont_fy)

donors_agg <- aggregate(donors_agg$gift_plus_pledge, 
                        by=list(campaign_fy=donors_agg$campaign_fy), 
                        FUN=sum)
names(donors_agg) <- c('fy','contributed_rev')


# ------------------ TIX ---------------------
# import ticketing data
df_tix <- rbind(read.csv('data/Clx14.csv', header = TRUE, skip = 3),
                read.csv('data/Clx15.csv', header = TRUE, skip = 3),
                read.csv('data/Clx16.csv', header = TRUE, skip = 3),
                read.csv('data/Clx17.csv', header = TRUE, skip = 3),
                read.csv('data/Clx18.csv', header = TRUE, skip = 3),
                read.csv('data/Pops14.csv', header = TRUE, skip = 3),
                read.csv('data/Pops15.csv', header = TRUE, skip = 3),
                read.csv('data/Pops16.csv', header = TRUE, skip = 3),
                read.csv('data/Pops17.csv', header = TRUE, skip = 3),
                read.csv('data/Pops18.csv', header = TRUE, skip = 3),
                read.csv('data/Summer14.csv', header = TRUE, skip = 3),
                read.csv('data/Summer15.csv', header = TRUE, skip = 3),
                read.csv('data/Summer17.csv', header = TRUE, skip = 3),
                read.csv('data/Summer18.csv', header = TRUE, skip = 3))

df_other <- rbind(read.csv('data/Chamber14-15.csv', header = TRUE, skip = 3),
                read.csv('data/Chamber16-18.csv', header = TRUE, skip = 3),                
                read.csv('data/Connections14-15.csv', header = TRUE, skip = 3),
                read.csv('data/Connections16-18.csv', header = TRUE, skip = 3),
                read.csv('data/Family14-15.csv', header = TRUE, skip = 3),
                read.csv('data/Family16-18.csv', header = TRUE, skip = 3),
                read.csv('data/Organ14-15.csv', header = TRUE, skip = 3),
                read.csv('data/Organ16-18.csv', header = TRUE, skip = 3),
                read.csv('data/Specials14-15.csv', header = TRUE, skip = 3),
                read.csv('data/Specials16-18.csv', header = TRUE, skip = 3))

summer16_df <- read.csv('data/summer16.csv', header = TRUE, skip = 3)

# --------------- Remove time from performance date
# -- Convert date/time -- Summer fy16 is a different format
library(lubridate)
df_tix$perf_dt <- mdy_hms(df_tix$perf_dt)
df_other$perf_dt <- mdy_hms(df_other$perf_dt)
summer16_df$perf_dt <- mdy_hm(summer16_df$perf_dt)

# Bind them together and remove other DFs
df_tix <- rbind(df_tix,df_other,summer16_df)
rm(df_other,summer16_df)

# Build the tickets dataframe
tickets_agg <- df_tix %>%
  mutate(fy = ifelse(perf_dt < as.Date('2014-07-01'),
                     'PS 13-14',
                     ifelse(perf_dt < as.Date('2015-07-01'),
                            'PS 14-15',
                            ifelse(perf_dt < as.Date('2016-07-01'),
                                   'PS 15-16',
                                   ifelse(perf_dt < as.Date('2017-07-01'),
                                          'PS 16-17','PS 17-18'))))) %>%
  filter(summary_cust_id > 0, paid_amt > 0)

tickets_agg <- aggregate(tickets_agg$paid_amt,
                         by=list(fy=tickets_agg$fy),
                         FUN=sum)
names(tickets_agg) <- c('fy','earned_rev')

# Join tickets with donors in aggregate
df_agg_trend <- left_join(donors_agg,tickets_agg,by="fy")

###################################################################################
###################################################################################
# --------------------------------- SPLIT DATA -------------------------
###################################################################################
###################################################################################
# ------------------ DEV ---------------------
donors_split <- df_donors %>%
  mutate(campaign_fy = substr(campaign, 1, 8)) %>%
  filter(campaign_fy == 'PS 13-14' |
           campaign_fy == 'PS 14-15' |
           campaign_fy == 'PS 15-16' |
           campaign_fy == 'PS 16-17' |
           campaign_fy == 'PS 17-18') %>%
  select(gift_plus_pledge,fund_desc,campaign_fy,summary_cust_id,cont_fy) 

# Aggregate by FY then summary customer ID to lookup group class
donors_split <- aggregate(gift_plus_pledge~campaign_fy+summary_cust_id, 
                          donors_split, 
                          sum)

# Group in A,B,C,or D based on giving amount in given FY
donors_split$group <- ifelse(donors_split$gift_plus_pledge <= 1000,
                             'A',
                             ifelse(donors_split$gift_plus_pledge <= 10000,
                                    'B',
                                    ifelse(donors_split$gift_plus_pledge <= 100000,
                                           'C','D')))

# Aggregate by FY then group
donors_split <- aggregate(gift_plus_pledge~campaign_fy+group,
                          donors_split,
                          sum)

# Spread the data
donors_split <- spread(donors_split,key=group,value=gift_plus_pledge)

names(donors_split) <- c('fy','grp_A','grp_B','grp_C','grp_D')

# ------------------ TIX ---------------------
tickets_split <- df_tix %>%
  mutate(fy = ifelse(perf_dt < as.Date('2014-07-01'),
                     'PS 13-14',
                     ifelse(perf_dt < as.Date('2015-07-01'),
                            'PS 14-15',
                            ifelse(perf_dt < as.Date('2016-07-01'),
                                   'PS 15-16',
                                   ifelse(perf_dt < as.Date('2017-07-01'),
                                          'PS 16-17','PS 17-18'))))) %>%
  filter(summary_cust_id > 0, paid_amt > 0) %>% 
  mutate(pr_grp_actual = ifelse(price_type_group == 'Single ' |
                                  price_type_group == 'Discount',
                                'Single','Subscription'))

# Aggregate by FY then pr_grp_actual
tickets_split <- aggregate(paid_amt~fy+pr_grp_actual,
                           tickets_split,
                           sum)

# Spread the data
tickets_split <- spread(tickets_split,key=pr_grp_actual,value=paid_amt)

# Join tickets with donors in split sets
df_split_trend <- left_join(donors_split,tickets_split,by="fy")


###################################################################################
###################################################################################
# --------------------------------- AS PERCENTAGE OF TOTAL -----------------------
###################################################################################
###################################################################################

# convert to percentages
df_agg_trend[,-1] <- df_agg_trend[,-1]/rowSums(df_agg_trend[,-1])
df_split_trend[,-1] <- df_split_trend[,-1]/rowSums(df_split_trend[,-1])

# Export to xlsx
library('xlsx')
write.xlsx(df_agg_trend, file = "revenue_trends_fy14-18.xlsx",
           sheetName = 'annual_agg', row.names = FALSE, append = FALSE)
write.xlsx(df_split_trend, file = "revenue_trends_fy14-18.xlsx",
           sheetName = 'annual_split', row.names = FALSE, append = TRUE)