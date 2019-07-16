# --- NOTES ---
# Make sure to update the imported data with new file names

###################################################################################
###################################################################################
# --------------------------------- USER INPUTS -------------------------

current_year <- 2019
# Must be in this format

cutoff_date <- as.Date('2018-12-31')
# What should be the cutoff date to exclude purchasers of tix for future concerts 
#   For example, if we're only interested in Q2, 2019, choose 2018-12-31 as the cutoff_date

###################################################################################
###################################################################################
# --------------------------------- PACKAGES ---------------------------------
library(lubridate)
library(dplyr)

###################################################################################
###################################################################################
# --------------------------------- DATA SETUP -------------------------
# import ticketing data
df <- rbind(read.csv('data/Clx16.csv', header = TRUE, skip = 3),
                read.csv('data/Clx17.csv', header = TRUE, skip = 3),
                read.csv('data/Clx18.csv', header = TRUE, skip = 3),
                read.csv('data/Clx19.csv', header = TRUE, skip = 3),
                read.csv('data/Pops16.csv', header = TRUE, skip = 3),
                read.csv('data/Pops17.csv', header = TRUE, skip = 3),
                read.csv('data/Pops18.csv', header = TRUE, skip = 3),
                read.csv('data/Pops19.csv', header = TRUE, skip = 3),
                read.csv('data/Summer17.csv', header = TRUE, skip = 3),
                read.csv('data/Summer18.csv', header = TRUE, skip = 3),
                read.csv('data/Summer19.csv', header = TRUE, skip = 3))

summer16_df <- read.csv('data/summer16.csv', header = TRUE, skip = 3)

# --------------- Remove time from performance date
# -- Convert date/time -- Summer fy16 is a different format
df$perf_dt <- mdy_hms(df$perf_dt)
df$order_dt <- mdy_hms(df$order_dt)
summer16_df$perf_dt <- mdy_hm(summer16_df$perf_dt)
summer16_df$order_dt <- mdy_hm(summer16_df$order_dt)

# Bind them together and remove other DFs
df <- rbind(df,summer16_df)
rm(summer16_df)

# Build the filtered df
tix <- df %>%
  filter(summary_cust_id > 0, perf_dt <= cutoff_date) %>%
  mutate(fy = ifelse(perf_dt < as.Date('2014-07-01') & perf_dt > as.Date('2013-06-30'),2014,
                     ifelse(perf_dt < as.Date('2015-07-01'),2015,
                            ifelse(perf_dt < as.Date('2016-07-01'),2016,
                                   ifelse(perf_dt < as.Date('2017-07-01'),2017,
                                          ifelse(perf_dt < as.Date('2018-07-01'),2018,
                                                 ifelse(perf_dt < as.Date('2019-07-01'),2019,NULL))))))) %>%
  mutate(price_type_group = ifelse(price_type_group == 'Subscription' | price_type_group == 'Flex',
                                   'Subscription','Single'))
tix <- tix[c('perf_dt','fy','price_type_group','summary_cust_id')]

#################################################################################################################
############################## HISTORICAL AUDIENCE - AS OF CURRENT_YEAR VAR #####################################
#################################################################################################################
# - PRIOR 3 YEARS
prior_3yr_df <- tix %>%
  filter(fy == current_year - 1 |
           fy == current_year - 2 |
           fy == current_year - 3)

prior_3yr_audience <- data.frame(unique(prior_3yr_df[,c('summary_cust_id')]))
names(prior_3yr_audience) <- c('summary_cust_id')

prior_3yr_subs <- data.frame(unique(prior_3yr_df[prior_3yr_df$price_type_group == 'Subscription',c('summary_cust_id')]))
names(prior_3yr_subs) <- c('summary_cust_id')

# - 2 YEARS BEFORE PRIOR YEAR
two_years_before <- tix %>%
  filter(fy == current_year - 2 |
           fy == current_year - 3)

two_yrs_before_subs <- data.frame(unique(two_years_before[two_years_before$price_type_group == 'Subscription',c('summary_cust_id')]))
names(two_yrs_before_subs) <- c('summary_cust_id')

# - PRIOR YEAR
prior_yr_df <- tix %>%
  filter(fy == current_year - 1)

prior_yr_subs <- data.frame(unique(prior_yr_df[prior_yr_df$price_type_group == 'Subscription',c('summary_cust_id')]))
names(prior_yr_subs) <- c('summary_cust_id')

# prior year excluding two years before
prior_yr_subs$exclusive <- !(prior_yr_subs$summary_cust_id %in% two_yrs_before_subs$summary_cust_id)
prior_yr_subs_exclusive <- prior_yr_subs %>%
  filter(exclusive == TRUE)

#################################################################################################################
############################## CURRENT AUDIENCE - AS OF CURRENT_YEAR VAR ########################################
#################################################################################################################
current_yr_df <- tix %>%
  filter(fy == current_year)

current_audience <- data.frame(unique(current_yr_df[,c('summary_cust_id')]))
names(current_audience) <- c('summary_cust_id')

current_subs <- data.frame(unique(current_yr_df[current_yr_df$price_type_group == 'Subscription',c('summary_cust_id')]))
names(current_subs) <- c('summary_cust_id')

# Identify current subscribers as 'new', 'second year', or 'multi'
current_subs$new_sub <- !(current_subs$summary_cust_id %in% prior_3yr_subs$summary_cust_id)
current_subs$second_yr_sub <- (current_subs$summary_cust_id %in% prior_yr_subs_exclusive$summary_cust_id)
current_subs$multi_sub <- ifelse(!current_subs$new_sub & !current_subs$second_yr_sub,TRUE,FALSE)

# Remove all other sub related dfs for cleanliness
rm(prior_3yr_subs,prior_yr_subs,prior_yr_subs_exclusive,two_yrs_before_subs)

# current singles is excluding any subscribers
current_singles <- current_audience %>%
  filter(!(current_audience$summary_cust_id %in% current_subs$summary_cust_id))

############################## 
# DOES IT MAKE SENSE TO LOOK AT NEW, 2ND, 3RD, MULTI - ACROSS A 3 YEAR PERIOD??
# DOES THAT MEAN SOMEONE WHO GOES TO ONE CONCERT PER YEAR SHOULD BE CONSIDERED MULTI??
current_singles$new <- !(current_singles$summary_cust_id %in% prior_3yr_audience$summary_cust_id)
























